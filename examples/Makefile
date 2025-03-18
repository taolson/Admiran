AMC= ../bin/amc

EXAMPLES= hello fib eDigits primes subMin zebra

.SUFFIXES:
.SUFFIXES: .am

.PHONY: clean

all: $(EXAMPLES)

.am:
	$(AMC) $<

clean:
	rm -f $(EXAMPLES) *.x2 *.s
