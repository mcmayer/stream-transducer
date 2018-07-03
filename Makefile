all: build

build:
	stack build

run: build
	stack exec stream-trans-exe

code:
	stack build hoogle intero stylish-haskell hlint; \
	zsh -c -i "code ."

.PHONY: build code
