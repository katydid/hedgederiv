.PHONY: run test setup

test: 
	stack test

test-trace:
	stack test --trace	

run:
	stack build
	stack exec nwe-exe

setup:
	stack setup

ide-setup:
	stack build intero
