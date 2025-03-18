AMC= ../bin/amc

TOOLS= badlines dumpSTG dumpX2 sloc

.SUFFIXES:
.SUFFIXES: .am

.PHONY: clean

all: $(TOOLS)

.am:
	$(AMC) $<

clean:
	rm -f $(TOOLS) *.x2 *.s
