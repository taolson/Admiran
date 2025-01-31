MIRAC= ../bin/mirac

EXAMPLES= hello fib eDigits primes sloc zebra

.SUFFIXES:
.SUFFIXES: .m

.PHONY: clean

all: $(EXAMPLES)

.m:
	$(MIRAC) $<

clean:
	rm -f $(EXAMPLES) *.x2 *.s
