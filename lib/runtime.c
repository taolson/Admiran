// Admiran runtime, with a heap that automatically grows from an initial memory size to a maximum memory size,
// and has a dyamically-sized rootlist that grows down from the top of memory

#define GC_DEBUG 0
#define SHOW_STATS 0

#include <fcntl.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>

// naming convention: Admiran-defined functions and values callable from runtime.c
// start with an uppercase letter, while standard C functions (callable from runtime.c
// or from Admiran code start with a lowercase letter.

// configuration
// NOTE: must match the corresponding values in the config.m file in the compiler
#define maxArgs 16
#define maxRegs 16
#define maxEnv  20

uint64_t gcInitMem = 0x02000000;  // initial memory size, in words (0x200000 = 16MB)
uint64_t gcMaxMem  = 0x20000000;  // maximum size, in words, to grow allocated memory (0x20000000 = 4GB)
uint64_t gcMinFree = 0x1000;      // minimum size, in words, that must be available to proceed after a gcMajor (otherwise terminate with out of memory)

// a Admiran tagged value type
typedef uint64_t word;
typedef void    *value;

// Admiran HeapObj structure definitions
enum heapInfoTag {Hfun, Hthunk, Hfwd, Hpap, Hfbalt, Hstream, Harray, HarrayR, Hreloc};   // note: must match enum values in codegen.m

// a generic heapObj representation, only providing the tag byte for inspection and a data value (Hreloc)
typedef struct _heapObj {
  uint8_t  tag;
  uint8_t  _pad1;
  uint16_t _pad2;
  uint32_t _pad3;
  value    data[0];
} heapObj;

// functions to help coersion between values and heapObjs
int isHeapObj (value v) {
  return ((word) v) & 1;
}

heapObj *untagHeapObj (value v)
{
  return (heapObj *) ((uint64_t) v & ~1);
}

value tagHeapObj (heapObj *obj)
{
  return (value) ((word) obj | 1);
}

word untagWord (value v)
{
  return ((word) v) >> 1;
}

value tagWord (word n)
{
  return (value) (n << 1);
}

// codeObj representation of a heapObj (Hfun, Hthunk, Hfwd, Hpap, Hfbalt)
typedef struct _codeObj {
  uint8_t  tag;
  uint8_t  arity;
  uint16_t envSize;
  uint32_t _pad;
  value    cp;
  value    env[0];
} codeObj;

// streamObj representation of a heapObj
typedef struct _streamObj {
  uint8_t  tag;
  uint8_t  fd;
  uint16_t bufSize;
  uint16_t bufLimit;
  uint16_t bufPos;
  char     strm[0];
} streamObj;

// arrayObj representation of a heapObj
typedef struct _arrayObj {
  uint8_t  tag;
  uint8_t  _pad1;
  uint16_t _pad2;
  uint32_t arraySize;
  value    data[0];
} arrayObj;

// functions and data to communicate between Admiran and C
extern value GetStartClosure(); // get the closure address for the Admiran start routine 
extern void  EnterAdmiran ();   // enter a Admiran closure
extern void  CallAdmiran ();    // call a Admiran codeblock
extern void  RetAdmiran ();     // return a value to a Admiran continuation
extern void  PackArgs0 ();      // needed to make Admiran-comptible return values
extern void *ApplyToEnvFns[];   // array of functions to perform ApplyToEnv on a variable number of env values
extern void *GlobalBase;        // marker for the base of the global heap area (static heapPtrs in Admiran code)
extern void *GlobalTop;         // marker for the top of the global heap area

// state save/restore area for registers used by Admiran code
// Env is a static area to save the continuation closure environment from the stack when returning to a continuation
// it solves the problem of needing to create a new continuation environment on the stack while in a continuation
value Arg[maxArgs];     // note: must match configuration size in codegen.m
value Reg[maxRegs];     // note: must match configuration size in codegen.m
value Env[maxEnv+2];    // +2 to reserve two slots at beginning of Env to emulate the tagword and codePtr of a heap closure
value *Frm;             // save area for FrmReg
value FrmSize;          // save area for frame size value (set when a new frame is allocated, for use by GC to ensure frame is contiguous)
value *Stk;             // save area for Admiran stack pointer

// heap and garbage collection 
value *gcHeapTop;    // top of heap
value *gcMaxStack;   // high point of stack
value *gcRootsTop;   // top address of roots list for heapPtrs in old space that point into new space (roots list grows down)
value *gcRootsAlloc; // allocation pointer for roots list
value *gcOldBase;    // base address of old space
value *gcOldAlloc;   // allocation pointer in old space, used during GC to copy heapObjs to old space
value *gcNewBase;    // base address of new space
value *gcNewAlloc;   // GC allocation pointer in new space, used by Admiran during allocation
word  gcRequestSize; // current request size, used to check that a GC has reclaimed enough space
value marked;        // temporary root to trace during GC
word  gcOffset;      // offset value for a relocated address used during gcMajor

// debug for tracing info on gc errors
char  *gcRootInfo;
word  gcRootIndex;
int   gcDidReloc;

// saved command line arguments for passing into Admiran
int     sysArgc;
char ** sysArgv;

// stats
int gcMinorCount;
int gcMajorCount;
int gcMinorInterval;
int gcRootsOverflowCount;
int applyCount;
int overAppliedCount;
int underAppliedNoArgsCount;
int underAppliedPapCount;
int applyPapCount;

void statsInit ()
{
  gcMinorCount            = 0;
  gcMajorCount            = 0;
  gcMinorInterval         = 0;
  gcRootsOverflowCount    = 0;
  applyCount              = 0;
  overAppliedCount        = 0;
  underAppliedNoArgsCount = 0;
  underAppliedPapCount    = 0;
  applyPapCount           = 0;
}

void finish (int status)
{
#if SHOW_STATS
  printf ("*** gcMinors: %d gcMajors: %d gcRootsOverflow: %d\n", gcMinorCount, gcMajorCount, gcRootsOverflowCount);
  printf ("applyCount: %d \noverAppliedCount: %d \nunderAppliedCount: %d \napplyPapCount: %d\n",
          applyCount, overAppliedCount, underAppliedPapCount, applyPapCount);
#endif
  exit (status);
}


// Heap operations

void gcMinor ();
void gcMajor (value *copyBase);
void gcGrowHeap ();

// initialize the heap, allocating two half-spaces plus a roots list
void heapInit ()
{
  gcOldBase    = mmap (0, gcMaxMem * sizeof (word), PROT_WRITE, MAP_ANON|MAP_PRIVATE, -1, 0);
  gcOldAlloc   = gcOldBase;
  gcHeapTop    = gcOldBase + gcInitMem;
  gcNewBase    = gcOldBase + (gcHeapTop - gcOldBase) / 2;
  gcNewAlloc   = gcNewBase;
  gcRootsTop   = gcOldBase + gcMaxMem;
  gcRootsAlloc = gcRootsTop;
  gcOffset     = 0;
  gcRootInfo   = "<no root>";
  gcRootIndex  = 0;

#if GC_DEBUG > 1
  printf ("gcOldBase = %p gcNewBase = %p gcHeapTop = %p rootsListTop = %p\n", gcOldBase, gcNewBase, gcHeapTop, gcRootsTop);
#endif
}

// allocate memory from the old space of the heap
heapObj *heapAllocOld (word nSlots)
{
  heapObj *obj;

  obj = (heapObj *)gcOldAlloc;
  gcOldAlloc += nSlots;
  return obj;
}

// allocate memory from the new space of the heap, performing garbage collection if out of space
heapObj *heapAllocNew (word nSlots)
{
  heapObj *obj;

 restart:
  obj = (heapObj *)gcNewAlloc;
  gcNewAlloc += nSlots;
  if (gcNewAlloc >= gcHeapTop) {
    gcMinor ();
    goto restart;
  }
  return obj;
}
  
// return the number of words that a heapObj takes in the heap
word heapObjWords (heapObj *obj)
{
  word nwords;

  switch (obj -> tag) {
  case Hstream:
    return ((streamObj *) obj) -> bufSize + 1;  // tagword, bufSize

  case Harray:
  case HarrayR:
    return ((arrayObj *) obj) -> arraySize + 1; // tagword, arraySize

  default:
    return ((codeObj *) obj) -> envSize + 2;    // tagword, cp, env
  }
  return 0;
}


// garbage collection for the heap, based upon "Simple Generational Garbage Collection and Fast Allocation" paper by Andrew Appel
//
// the heap is divided into 3 spaces:
//    global: at the bottom of the heap; these are Fclosures and Tclosures generated during compilation, and have
//            references to them in immediate values in the code section.  Therefore they are at fixed addresses
//            and cannot move.  Any global thunks here which are updated are placed in the gcRoots list.
//
//    old:    bottom half of collectable space, between gcOldBase and gcOldAlloc, holds heapObjs that have survived
//            gcMinor collection
//
//    new:    top half of collectable space, between gcNewBase and gcNewAlloc; holds newly-created heapObjs


// check to see if a value needs to be relocated from new space to old space
// if so, relocate it and return its new (tagged) value
value heapRelocateValue (value v)
{
  heapObj *obj, *robj;
  value   *src, *dst;   // for copying heap objects
  word     size;

 retry :
  // return value if it isn't a heapPtr, or isn't in the space we are moving
  if (! isHeapObj (v)                          ||       // value is a tagged word
      (v >= (value) (gcOldBase + gcMaxMem))    ||       // value is a stackPtr 
      (v < (value) gcOldBase)                  ||       // value is in globals (not relocated)
      (gcOffset == 0 && v < (value) gcNewBase)) {       // value is in old space and we are performing a gcMinor
#if GC_DEBUG > 2
    printf ("heapRelocateValue %p not relocated\n", v);
#endif
    return v;
  }
  obj  = untagHeapObj (v);
  size = heapObjWords (obj);

#if GC_DEBUG > 2
    printf ("relocate %p tag %d size %lu to ", obj, obj -> tag, size);
#endif

  if (obj -> tag > Hreloc) {
    fprintf (stderr, "*** heapRelocateValue: nonsensical tag %d for obj %p\n", obj -> tag, obj);
    fprintf (stderr, "*** originated from %s %" PRIu64 "\n", gcRootInfo, gcRootIndex);
    exit (1);
  }
  switch (obj -> tag) {

    // object is forwarded; follow fwd ptr without moving it, getting rid of the forwarding
  case Hfwd:
    v = ((codeObj *) obj) -> env[0];
    goto retry;

    // object is already relocated; return its new location
  case Hreloc:
    robj = obj -> data[0];
    break;

    // move the object, without examining its contents, yet
  default:
    robj = heapAllocOld (size);

    // copy obj data words into robj; written in explicit loop instead of memcpy because size is typically small
    src = (value) obj;
    dst = (value) robj;
    for (int i=0; i<size; ++i) {
      *dst++ = *src++;
    }
    robj -= gcOffset;                 // adjust to the final location if in gcMajor
    obj -> tag     = Hreloc;          // mark as relocated
    obj -> data[0] = robj;
  }

#if GC_DEBUG > 2
  printf ("%p\n", robj);
#endif

  return (tagHeapObj (robj));
}

// relocate a contiguous block of values
void heapRelocateBlock (value *blk, value *end)
{
  int i = 0;
  while (blk < end) {
    gcRootIndex = i;
    ++ i;
    *blk = heapRelocateValue (*blk);
    ++blk;
  }
}

// trace the contents of a heapObj, relocating any heapPtr values inside of it
void gcTraceObj (heapObj *obj)
{
  codeObj   *co;
  arrayObj  *ao;

  if (obj -> tag > Hreloc) {
    fprintf (stderr, "*** gcTraceObj: nonsensical tag %d for obj %p\n", obj -> tag, obj);
    fprintf (stderr, "*** originated from %s %" PRIu64 "\n", gcRootInfo, gcRootIndex);
    exit (1);
  }
    
#if GC_DEBUG > 2
  printf ("gcTraceObj %p tag = %d\n", obj, obj -> tag);
#endif

  switch (obj -> tag) {
  case Hfun:
  case Hthunk:
  case Hfbalt:
    co = (codeObj *)obj;
    heapRelocateBlock (co -> env, co -> env + co -> envSize);
    break;

    // an Hfwd only has a single env value, which is the heapPtr of the new obj
  case Hfwd:
    co           = (codeObj *) obj;
    co -> env[0] = heapRelocateValue (co -> env[0]);
    break;

  case Hpap:
    co       = (codeObj *) obj;
    co -> cp = heapRelocateValue (co -> cp);    // relocate the original heapPtr that was partially applied
    heapRelocateBlock (co -> env, co -> env + co -> envSize);
    break;

  case Harray:
  case HarrayR:
    ao        = (arrayObj *) obj;
    ao -> tag = Harray;        // unmark any HarrayR tags
    heapRelocateBlock (ao -> data, ao -> data + ao -> arraySize);
    break;

  case Hreloc:
    fprintf (stderr, "*** gcTraceObj encountered an Hreloc obj\n");
    fprintf (stderr, "*** originated from %s %" PRIu64 "\n", gcRootInfo, gcRootIndex);
    exit (1);
  }
}

// relocate the Frm region
// Admiran code assumes that objects within the Frm are contiguous and addresses them based upon offsets from the Frm
// register.  So the Frm is treated as a contiguous block of various-sized heapObjs that total FrmSize in size

void gcRelocateFrm ()
{
  word   size   = (word) FrmSize;
  value *fend   = Frm + size;
  value *fp;

  gcRootInfo = "Frame";
  if (!isHeapObj ((value) Frm)) {
    return;
  }

#if GC_DEBUG > 2
  printf ("gcRelocateFrm: size = %lu\n", size);
#endif

  fp = Frm + heapObjWords (untagHeapObj (Frm));
  Frm = heapRelocateValue ((value) Frm);
  while (fp < fend) {
    gcRootIndex = (word) fp;

#if GC_DEBUG > 2
    printf ("frm next obj %p size %lu\n", fp, heapObjWords (untagHeapObj (fp)));
#endif

    heapRelocateValue (fp);
    fp += heapObjWords (untagHeapObj (fp));
  } 
}

// trace the roots list, growing down from top of memory to gcRootsAlloc, compacting roots found in the global area
void gcTraceRootsList ()
{
  value *gcRootsNewAlloc = gcRootsTop;
  value *rp = gcRootsTop;
  heapObj *obj;

  gcRootInfo = "rootsList";
  while (rp > gcRootsAlloc) {
    --rp;
    gcRootIndex = (word) rp;
    obj = untagHeapObj (*rp);

#if GC_DEBUG > 2
    printf ("tracing root in rootsList at %p -> %p\n", rp, obj);
#endif

    gcTraceObj (obj);
    if (*rp < (value) &GlobalTop) {
      --gcRootsNewAlloc;
      *gcRootsNewAlloc = *rp;
    }
  }
  gcRootsAlloc = gcRootsNewAlloc;
}

// trace all of the heap roots, relocating directly-reachable heapObjs in the new area to the old area
void gcTraceRoots ()
{
  // relocate the frame first, because objects it holds are assumed to be contiguous by Admiran code
  // otherwise, individual objects within the frame may be relocated individually

#if GC_DEBUG > 2
  printf ("gcRelocateFrm\n");
#endif
  gcRelocateFrm ();

#if GC_DEBUG > 2
  printf ("gcTraceRootsList\n");
#endif
  gcTraceRootsList ();

#if GC_DEBUG > 2
  printf ("relocate marked\n");
#endif
  gcRootInfo = "marked";
  marked = heapRelocateValue (marked);

#if GC_DEBUG > 2
  printf ("relocate Args\n");
#endif
  gcRootInfo = "Arg";
  heapRelocateBlock (Arg, Arg + maxArgs);       // should revise to trace only live Args

#if GC_DEBUG > 2
  printf ("relocate Regs\n");
#endif
  gcRootInfo = "Reg";
  heapRelocateBlock (Reg, Reg + maxRegs);       // should revise to trace only live Regs (R0, R1, R2 always considered live?)

#if GC_DEBUG > 2
  printf ("relocate Env\n");
#endif
  gcRootInfo = "Env";
  heapRelocateBlock (Env+2, Env + maxEnv+2);    // should revise to trace only live Envs (Env 0, 1 are dummy space to match heapClosure format)

#if GC_DEBUG > 2
  printf ("relocate Stk\n");
#endif
  gcRootInfo = "Stk";
  heapRelocateBlock (Stk, gcMaxStack);

#if GC_DEBUG > 2
  printf ("gcTraceRoots done\n");
#endif
}

// trace all objects between base and the gcOldAlloc ptr, moving any new area heapPtr values to old and updating their addresses
void gcTraceFrom (value *base)
{
  heapObj *obj;
  word     size;
  int i = 0;
  
#if GC_DEBUG > 2
  printf ("gcTraceFrom %p\n", base);
#endif

  gcRootInfo = "TraceFrom";
  // gcOldAlloc is updated during gcTraceObj, so we chase that with the current base until we are done
  while (base < gcOldAlloc) {
    obj  = untagHeapObj (base);
    size = heapObjWords (obj);
    gcRootIndex = i;
    ++ i;

#if GC_DEBUG > 2
    printf ("gcTraceFrom base=%p tag=%d size=%lu\n", obj, obj -> tag, size);
#endif

    gcTraceObj (obj);
    base += size;
  }
}

// perform a minor garbage collection, tracing roots into the new area and moving reachable heapObj from new to old
void gcMinor ()
{
  value *copyBase = gcOldAlloc;         // base of oldspace memory we are copying into from newspace

#if GC_DEBUG > 1
  printf ("\n*** gcMinor ***\n");
  printf ("gcRequestSize: %lu\n", gcRequestSize);
  printf ("stack size: %#lx\n", gcMaxStack - Stk);
  printf ("roots list size before gc: %#lx\n", gcOldBase + gcMaxMem - gcRootsAlloc);
  printf ("gcOldBase  = %p\ngcOldAlloc = %p\ngcNewBase  = %p\ngcNewAlloc = %p\ngcHeapTop  = %p\n",
          gcOldBase, gcOldAlloc, gcNewBase, gcNewAlloc, gcHeapTop);
#endif

  ++gcMinorCount;
  ++gcMinorInterval;
  if (gcRootsAlloc <= gcHeapTop) {
    ++gcRootsOverflowCount;
  }
  gcOffset = 0;
  gcTraceRoots ();
  gcTraceFrom (copyBase);
  gcNewBase  = gcOldAlloc + (gcHeapTop - gcOldAlloc) / 2;       // divide remaining free space in half for gcOldAlloc, gcNewAlloc
  gcNewAlloc = gcNewBase;

#if GC_DEBUG > 1
  printf ("roots list size after gc: %#lx\n", gcRootsTop - gcRootsAlloc);
  printf ("old space used %#lx\n", gcOldAlloc - gcOldBase);
  printf ("old space free %#lx\n", gcNewBase - gcOldAlloc);
  printf ("new space free %#lx\n", gcHeapTop - gcNewAlloc);
  printf ("\n");
#endif

  // If the tracing ends up straddling the halfspace, we need to perform a gcMajor.
  // Since the newspace we just copied has already been traced (we know its live), we use it as the base of
  // the copy back to old space, and start tracing from gcOldAlloc in gcMajor
  if (gcOldAlloc - gcOldBase >= (gcHeapTop - gcOldBase) / 2) {
    gcMajor (copyBase);
    return;
  }

  // perform a gcMajor if we haven't yet recovered enough space to cover the gcRequestSize, or if we are
  // getting close to filling oldSpace (75%)
  if (gcHeapTop - gcNewAlloc <= gcRequestSize + gcMinFree || gcOldAlloc - gcOldBase >= (gcHeapTop - gcOldBase) * 3 / 8) {
    gcMajor (gcOldAlloc);
  }
}

// perform a major garbage collection, tracing all reachable heapObjs and moving reachable heapObjs from old to new, then swap new and old
void gcMajor (value *copyBase)
{
  value *oldTop = gcOldAlloc;
  word usedSize;
  heapObj *obj;

#if GC_DEBUG > 0
  printf ("\n*** gcMajor ***\n");
  printf ("gcOldBase  = %p\ngcOldAlloc = %p\ngcNewBase  = %p\ngcNewAlloc = %p\ngcHeapTop  = %p\ngcMinorInterval = %d\n",
          gcOldBase, gcOldAlloc, gcNewBase, gcNewAlloc, gcHeapTop, gcMinorInterval);
#endif

  // check if old space has exceeded the maximum halfspace size
  if (gcOldAlloc - gcOldBase > (gcRootsAlloc - gcOldBase) / 2) {
    fprintf (stderr, "\n*** out of memory\n");
    exit (1);
  }
    
  ++gcMajorCount;
  gcOffset = copyBase - gcOldBase;      // amount to adjust heapPtrs by in words to account for block move back to gcOldBase
  gcTraceRoots ();
  gcTraceFrom (oldTop);

  // copy back down to oldspace and adjust oldspace pointers
  usedSize = gcOldAlloc - copyBase;     // size of used space
  memcpy (gcOldBase, copyBase, usedSize * sizeof (word));
  gcOldAlloc = gcOldBase + usedSize;
  gcNewBase  = gcOldAlloc + (gcHeapTop - gcOldAlloc) / 2;
  gcNewAlloc = gcNewBase;
  gcOffset = 0;

#if GC_DEBUG > 0
  printf ("root space used after gc: %#lx\n", gcRootsTop - gcRootsAlloc);
  printf ("old space used %#lx\n", gcOldAlloc - gcOldBase);
  printf ("old space free %#lx\n", gcNewBase - gcOldAlloc);
  printf ("new space free %#lx\n", gcHeapTop - gcNewBase);
  printf ("\n");
#endif

  // grow heap, if required
  if (gcHeapTop - gcNewAlloc <= gcRequestSize + gcMinFree ||             // didn't recover enough space for request
      gcOldAlloc - gcOldBase >= (gcHeapTop - gcOldBase) * 3 / 8) {      // getting close to filling oldSpace (75%)
    gcGrowHeap ();
  }
  gcMinorInterval = 0;  // reset the gcMinorInterval count now that we have performed a gcMajor and checked for heap growth
}

// attempt to grow the heap, or fail with out of memory
// should only be called after a gc has emptied newspace
void gcGrowHeap ()
{
  value* growToRequest = gcNewAlloc + gcRequestSize + gcMinFree;
  value* growByHalf    = gcHeapTop + (gcHeapTop - gcOldBase) / 2;

  // check for out of memory condition (we have already grown the stack to the max possible value)
  if (gcHeapTop >= gcRootsAlloc - gcMinFree) {
    fprintf (stderr, "\n*** out of memory in growHeap\n");
    exit (1);
  }

  // grow heap to the greater of the shortage amount or +50%, limited to available memory
  growByHalf = growByHalf > gcRootsAlloc - gcMinFree ? gcRootsAlloc - gcMinFree : growByHalf;
  gcHeapTop  = growByHalf > growToRequest ? growByHalf : growToRequest;

  // check for out of memory condition (not enough space for current request)
  if (gcHeapTop > gcRootsAlloc - gcMinFree) {
    fprintf (stderr, "\n*** out of memory in growHeap\n");
    exit (1);
  }

  // recalculate gcNewBase and gcNewAlloc with new gcHeapTop
  gcNewBase  = gcOldAlloc + (gcHeapTop - gcOldAlloc) / 2;
  gcNewAlloc = gcNewBase;

#if GC_DEBUG > 0
  printf ("\n*** gcGrowHeap ***\n");
  printf ("gcOldBase  = %p\ngcOldAlloc = %p\ngcNewBase  = %p\ngcNewAlloc = %p\ngcHeapTop  = %p\n",
          gcOldBase, gcOldAlloc, gcNewBase, gcNewAlloc, gcHeapTop);
  printf ("old space used %#lx\n", gcOldAlloc - gcOldBase);
  printf ("old space free %#lx\n", gcNewBase - gcOldAlloc);
  printf ("new space free %#lx\n", gcHeapTop - gcNewBase);
  printf ("\n");
#endif

}
    

// make an Hstream object for a C string
heapObj *makeHstream (char *s)
{
  int       n      = strlen (s);
  int       qwords = n / 8 + (n % 8 > 0);
  streamObj *strm  = (streamObj *) heapAllocNew (qwords + 1);

  strm -> tag = Hstream;
  strm -> fd  = 0;
  strm -> bufSize = qwords;
  strm -> bufLimit = n;
  strm -> bufPos = 0;
  strncpy (strm->strm, s, n);
  return ((heapObj *) strm);
}

// make an Hpap object
heapObj *makeHpap (value hobj, word arity, word nargs)
{
  word     missing = arity - nargs;
  codeObj *pap;

  marked = hobj;        // move hobj to marked roots in case the heapAlloc triggers a GC
  pap    = (codeObj *) heapAllocNew (nargs + 2);     // 2 words in base Hpap + space for env of nargs

  pap -> tag     = Hpap;
  pap -> arity   = missing;
  pap -> envSize = nargs;
  pap -> cp      = marked;
  for (int i = 0; i < nargs; ++i) {
    pap -> env[i] = Arg[i];
  }
  marked = 0;   // clear marked, now that we are finished with it
  return ((heapObj *) pap);
}
  

// Apply operations that aren't handled in-line in Admiran code

// push an ApplyToEnv closure onto the stack
void pushApplyToEnv (word arity, word nargs)
{
  word extra = nargs - arity;

  Stk -= extra + 1;                     // allocate space on stack for closure env and codePtr
  Stk[0] = ApplyToEnvFns[extra];        // closure codePtr is the appropriate ApplyToEnvFns for extra number of args
  for (int i = 0; i < extra; ++i) {     // save the extra args in the stack closure env
    Stk[i + 1] = Arg[arity + i];
  }
}

// handle non-common cases of Apply
// Admiran has set Reg[2] to the heapObj to apply, and Reg[3] to the number of arguments passed in Args
void apply ()
{
  value    fobj    = Reg[2];              // function codeObj (tagged)
  word     nargs   = (word) Reg[3];       // number of arguments passed in Reg[3] (untagged)
  codeObj *obj     = (codeObj *) untagHeapObj (fobj);
  word     arity   = obj -> arity;
  word     envSize = obj -> envSize;
  value    argBuf[16];

  ++applyCount;
  switch (obj -> tag) {
  case Hfun:
  case Hthunk:

    // correct arity; set up the env and call the closure's cp
    if (nargs == arity) {
      Reg[0] = obj -> cp;       // return codePtr to jump to in Reg[0]
      CallAdmiran ();
    }

    // over-applied: push extra args in an ApplyEnv closure and call with correct number of args
    else if (nargs > arity) {
      ++overAppliedCount;
      pushApplyToEnv (arity, nargs);
      Reg[0] = obj -> cp;        // return codePtr to jump to in Reg[0]
      CallAdmiran ();
    }

    // under-applied with no args; return fobj as the value
    else if (nargs == 0) {
      ++underAppliedNoArgsCount;
      Reg[0] = fobj;
      Reg[1] = PackArgs0;
      RetAdmiran ();
    }

    // under-applied: make an Hpap closure and return it
    else {
      ++underAppliedPapCount;
      heapObj *pap = makeHpap (fobj, arity, nargs);
      Reg[0]       = tagHeapObj (pap);  // tag Hpap for return
      Reg[1]       = PackArgs0;         // Reg[1] must hold the runtime routine to pack the args (0, in this case)
      RetAdmiran ();
    }

  case Hpap:
    // copy Args to the argBuf to free up Args
    ++applyPapCount;
    for (int i = 0; i < nargs; ++i) {
      argBuf[i] = Arg[i];
    }
    // copy existing partially-applied args in env to Args
    for (int i = 0; i < envSize; ++i) {
      Arg[i] = obj -> env[i];
    }
    // append new args in argBuf
    for (int i = 0; i < nargs; ++i) {
      Arg[envSize+i] = argBuf[i];
    }
    // do apply of original hobj (in the Hpap's cp field)
    Reg[2] = obj -> cp;
    Reg[3] = (value) (envSize + nargs); // update the #args, not tagged
    apply ();
    
  case Hfwd:
    Reg[2] = ((codeObj *) obj) -> env[0];
    apply ();
    
  default:
    fprintf (stderr, "*** Apply not implemented for %d\n", obj -> tag);
    exit (1);
  }
}


// System Ops

// open a file for reading or open/create a file for writing
// file name to open is passed as a string in a streamObj
void sysOpenFile (value rd, value sval, int openMode)
{
  int      fmode;
  streamObj *obj  = (streamObj *) untagHeapObj (sval);
  uint16_t   slen = obj -> bufLimit;     // length of filename string
  char      *fn   = obj -> strm;
  int        fd;

  switch (openMode) {
  case 1: fmode = O_RDONLY; break;
  case 2: fmode = O_CREAT | O_TRUNC  | O_WRONLY; break;
  case 3: fmode = O_CREAT | O_APPEND | O_WRONLY; break;
  }

  fn[slen] = 0;                 // zero-terminate the filename string for C         
  fd = open (fn, fmode, 0644);  // open file specified by fn, passing a default mode of 0644 = rw/r/r to use if a new file is created

  if (fd < 0) {
    perror (fn);
    exit (1);
  }

  // update the streamObj to be an empty fileStream buffer
  obj -> fd       = fd;
  obj -> bufLimit = 0;
  obj -> bufPos   = 0;

  // return the updated fileStream buffer in the destination register
  Reg[untagWord (rd)] = sval;
}


// close the file descriptor associated with a fileStream buffer
void sysCloseFile (value rd, value sval)
{
  streamObj *obj = (streamObj *) untagHeapObj (sval);
  int fd = obj -> fd;
  int r  = fd > 2 ? close (fd) : 0;     // ignore close requests for stdin, stdout, stderr

  if (r < 0) {
    perror (NULL);
    exit (1);
  }
  Reg[untagWord (rd)] = tagWord (r);
}


// read into a fileStream buffer from its file descriptor
void sysReadFile (value rd, value sval)
{
  streamObj *obj   = (streamObj *) untagHeapObj (sval);
  int        fd    = obj -> fd;
  size_t     size  = obj -> bufSize * sizeof (word);
  char      *buf   = obj -> strm;
  ssize_t   nbytes = read (fd, buf, size);

  if (nbytes < 0) {
    perror (NULL);
    exit (1);
  }

  // update the streamObj limit and pos pointers
  obj -> bufLimit = nbytes;
  obj -> bufPos   = 0;

  // return the number of bytes read
  Reg[untagWord (rd)] = tagWord (nbytes);
}


// write a fileStream buffer to its file descriptor
void sysWriteFile (value rd, value sval)
{
  streamObj *obj   = (streamObj *) untagHeapObj (sval);
  int        fd    = obj -> fd;
  size_t     size  = obj -> bufLimit;
  char      *buf   = obj -> strm;
  ssize_t   nbytes = write (fd, buf, size);

  if (nbytes < 0) {
    perror (NULL);
    exit (1);
  }

  // update the streamObj limit and pos pointers
  obj -> bufLimit = 0;
  obj -> bufPos   = 0;

  // return the number of bytes written
  Reg[untagWord (rd)] = tagWord (nbytes);
}


// get a file modification timestamp
void sysMtimeFile (value rd, value sval)
{
  streamObj  *obj       = (streamObj *) untagHeapObj (sval);
  uint16_t    slen      = obj -> bufLimit;    // length of filename string
  char       *fn        = obj -> strm;
  long        timestamp = 0;             // long instead of word because it can hold a negative value
  struct stat statBuf;
  int         rslt;

  fn[slen] = 0;                 // zero-terminate the filename string for C         
  rslt = stat (fn, &statBuf);   // get the stat info for the file

#ifdef __APPLE__
  timestamp = rslt < 0 ? rslt : statBuf.st_mtimespec.tv_sec;
#else
  timestamp = rslt < 0 ? rslt : statBuf.st_mtim.tv_sec;
#endif

  // return the timestamp (0 for no timestamp)
  Reg[untagWord (rd)] = tagWord (timestamp);
}


// get command line arguments
// idx 0 = argc, idx 1 .. = argv[idx]
void sysGetArg (value rd, value idx)
{
  int uidx = untagWord (idx);
  int urd  = untagWord (rd);

  if (uidx == 0) {
    Reg[urd] = tagWord (sysArgc - 1);   // subtract 1 because we don't report the program name (argv[0])
  }
  else {
    heapObj *obj = makeHstream (sysArgv[uidx]);
    Reg[urd] = tagHeapObj (obj);
  } 
}


// perform a shell command and return its exit status
void sysCmd (value rd, value sval)
{
  streamObj *obj  = (streamObj *) untagHeapObj (sval);
  uint16_t   slen = obj -> bufLimit;   // length of system string for C
  char      *cmd  = obj -> strm;
  int rslt;

  cmd[slen] = 0;                 // zero-terminate the command string for C         
  rslt = system (cmd);
  Reg[untagWord (rd)] = tagWord (rslt);
}


// perform a system operation
void systemOp (value op, value rd, value sval)
{
  int uop = untagWord (op);

  switch (uop) {

  case 0:
    finish (untagWord (sval));
    break;

  case 1:
  case 2:
  case 3:
    sysOpenFile (rd, sval, uop);
    break;

  case 4:
    sysCloseFile (rd, sval);
    break;

  case 5:
    sysReadFile (rd, sval);
    break;

  case 6:
    sysWriteFile (rd, sval);
    break;

  case 7:
    sysMtimeFile (rd, sval);
    break;

  case 8:
    sysGetArg (rd, sval);
    break;

  case 9:
    sysCmd (rd, sval);
    break;

  default:
    fprintf (stderr, "*** systemOp %d not implemented\n", uop);
    exit (1);
  }
}

int main (int argc, char **argv)
{
  // capture the stack pointer high address for use in GC
#ifdef __APPLE__
  asm ( "movq %rsp, _gcMaxStack(%rip);" );
#else
  asm ( "movq %rsp, gcMaxStack(%rip);" );
#endif

  sysArgc = argc;
  sysArgv = argv;
  setvbuf (stdout, NULL, _IONBF, BUFSIZ);
  heapInit ();
  statsInit ();
  Stk    = gcMaxStack;
  Arg[0] = GetStartClosure ();
  EnterAdmiran ();

  // unreached
  return 0;
}
