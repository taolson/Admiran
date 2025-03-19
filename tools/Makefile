AMC= ../bin/amc

TOOLS= badlines dumpSTG dumpX2 sloc

.SUFFIXES:
.SUFFIXES: .am

.PHONY: clean

all: $(TOOLS)

.am:
	$(AMC) $<

install: $(TOOLS)
	mv $(TOOLS) ../bin

clean:
	rm -f $(TOOLS) *.x2 *.s
