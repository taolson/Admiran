MIRAC= ../bin/mirac

TOOLS= badlines dumpX2 sloc

.SUFFIXES:
.SUFFIXES: .m

.PHONY: clean

all: $(TOOLS)

.m:
	$(MIRAC) $<

clean:
	rm -f $(TOOLS) *.x2 *.s
