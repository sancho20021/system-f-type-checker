.PHONY: all run runShow test clean
all: build
clean:
	rm -rf dist-newstyle

run: build
	cabal new-run typecheck

runShow: build
	cabal new-run typecheck show

test: build
	cabal new-test --test-show-details=streaming

build:
	cabal new-build