AMC= ../bin/amc

EXAMPLES= bowling caesar eDigits fib fizzBuzz hello primes wordHistogram zebra

.SUFFIXES:
.SUFFIXES: .am

.PHONY: clean

all: $(EXAMPLES)

.am:
	$(AMC) $<

clean:
	rm -f $(EXAMPLES) *.x2 *.s
