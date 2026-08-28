AMC= ../bin/amc

TOOLS= badlines dumpSTG dumpX2 sloc genericCtors interface qualMigration

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
