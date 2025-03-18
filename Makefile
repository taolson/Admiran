#pick which bootstrap compiler source and compiler options to use
OS := $(shell uname -s)
ifeq ($(OS), Darwin)
AMC_BOOT := amcMacOS.s
CCOPTS := -O2 -Wl,-no_pie
endif
ifeq ($(OS), Linux)
AMC_BOOT := amcLinux.s
CCOPTS := -O2 -fno-pie -no-pie -z noexecstack
endif

.PHONY:	clean distclean

all:	bin/amc

# make bootstrap compiler from correct asm source
bin/amcBoot:	boot/$(AMC_BOOT) lib/runtime.c
	@echo "\n=== building bootstrap compiler ===\n"
	@mkdir -p bin
	$(CC) $(CCOPTS) -o bin/amcBoot boot/$(AMC_BOOT) lib/runtime.c

# make stage1 compiler by compiling compiler sources with bootstrap compiler
bin/amcStage1: bin/amcBoot
	@echo "=== building stage1 compiler with bootstrap compiler ===\n"
	make clean; cd compiler; ../bin/amcBoot amc
	mv compiler/amc bin/amcStage1

# make stage2 compiler by recompiling with stage1 compiler
bin/amcStage2: bin/amcStage1
	@echo "\n=== building stage2 compiler with stage1 compiler ===\n"
	make clean; cd compiler; ../bin/amcStage1 amc
	mv compiler/amc.s compiler/amc.s.REF
	mv compiler/amc bin/amcStage2

# make final amc compiler by recompiling with stage2 compiler; ensure compilers match
bin/amc: bin/amcStage2
	@echo "\nbuilding amc compiler with stage2 compiler ===\n"
	make clean; cd compiler; ../bin/amcStage2 amc
	diff compiler/amc.s compiler/amc.s.REF
	@echo "\n=== amc compiler built successfully ==="
	mv compiler/amc bin/amc

clean:
	rm -rf compiler/*.x2 compiler/*.s lib/*.x2 lib/*.s

distclean:
	rm -rf bin/* 