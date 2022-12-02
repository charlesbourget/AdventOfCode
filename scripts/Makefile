files := $(wildcard *.lisp)
names := $(files:.lisp=)

.PHONY: all clean $(names)

all: $(names)

$(names): %: bin/%

bin/%: %.lisp build.sh Makefile
	mkdir -p bin
	./build.sh $<
	mv $(@F) bin/

clean:
	rm -rf bin man
