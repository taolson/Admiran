#pick which bootstrap compiler source and compiler options to use
OS := $(shell uname -s)
ifeq ($(OS), Darwin)
MIRAC_BOOT := miracMacOS.s
CCOPTS := -O2 -Wl,-no_pie
endif
ifeq ($(OS), Linux)
MIRAC_BOOT := miracLinux.s
CCOPTS := -O2 -fno-pie -no-pie -z noexecstack
endif

.PHONY:	clean distclean

all:	bin/mirac

# make bootstrap compiler from correct asm source
bin/miracBoot:	boot/$(MIRAC_BOOT) lib/runtime.c
	@echo "\n=== building bootstrap compiler ===\n"
	@mkdir -p bin
	$(CC) $(CCOPTS) -o bin/miracBoot boot/$(MIRAC_BOOT) lib/runtime.c

# make stage1 compiler by compiling compiler sources with bootstrap compiler
bin/miracStage1: bin/miracBoot
	@echo "=== building stage1 compiler with bootstrap compiler ===\n"
	make clean; cd compiler; ../bin/miracBoot mirac
	mv compiler/mirac bin/miracStage1

# make stage2 compiler by recompiling with stage1 compiler
bin/miracStage2: bin/miracStage1
	@echo "\n=== building stage2 compiler with stage1 compiler ===\n"
	make clean; cd compiler; ../bin/miracStage1 mirac
	mv compiler/mirac.s compiler/mirac.s.REF
	mv compiler/mirac bin/miracStage2

# make final mirac compiler by recompiling with stage2 compiler; ensure compilers match
bin/mirac: bin/miracStage2
	@echo "\nbuilding mirac compiler with stage2 compiler ===\n"
	make clean; cd compiler; ../bin/miracStage2 mirac
	diff compiler/mirac.s compiler/mirac.s.REF
	@echo "\n=== mirac compiler built successfully ==="
	mv compiler/mirac bin/mirac

clean:
	rm -rf compiler/*.x2 compiler/*.s lib/*.x2 lib/*.s

distclean:
	rm -rf bin/* 