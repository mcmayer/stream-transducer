all: build

build:
	stack build

code:
	stack build hoogle intero stylish-haskell hlint; \
	zsh -c -i "code ."

.PHONY: build code
