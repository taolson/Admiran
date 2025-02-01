|| 
|| config.m -- compiler configuration parameters
||
|| currently these configuration parameters are defined in this module, but some or all of them may be migrated
|| to a data structure (passed as part of the RWS reader state to various compiler components), and modified
|| at compiler runtime via the command line


%export + -os -hostOS

os ::= MacOS | Linux

|| these parameters must be set when installing the compiler
hostOS                = Linux           || set to Linux or MacOS
miranda2LibPath       = "../lib"        || set to the absolute path name fo the lib directory
                                        || e.g. "/home/tim/Programming/Miranda2/lib"

|| os-dependant configuration parameters
(codegenCsymbolPrefix, ccOpts)
    = case hostOS of
        MacOS -> ("_", "-O2 -Wl,-no_pie")
        Linux -> ("",  "-O2 -fno-pie -no-pie -z noexecstack")

|| shouldn't have to be touched, but configurable
filePathSeparator     = "/"
stdlibModuleName      = "stdlib"
streamModuleName      = "stream"
runtimeName           = "runtime.c"
cCompilerName         = "cc"

|| enable/disable features
useInlining           = True
useDemandAnalysis     = False
useStreamFusion       = False
useSel                = False
useEagerCtors         = False

|| verbose output configuration
demandVerbose         = False
inlineVerbose         = False
noteLengthLimit       = 100

|| code generator resource sizes (NOTE: must match the sizes defined in runtime.c)
codegenMaxArgs        = 16
codegenMaxRegs        = 16
codegenMaxEnv         = 20

|| inliner tuning parameters
inlineMaxIterations   = 15     || maximum number of iterations of the inliner per function group
inlineMaxComplexity   = 1500   || maximum total additional complexity inlined per function group
inlineMaxFnComplexity = 150    || maximum complexity of a function for inlining
inlineMaxCaseDup      = 16     || maximum number of CaseAlt duplications to allow (4 * 4)

|| desugar tuning parameters
maxCaseVectored       = 6      || maximum allowable CaseAlts in a CVectored case expr (otherwise use CPriority)
