.PHONY: clean console rebar deps get_deps

build:
	erlc *.erl

clean:
	rm -rf deps

console:
	erl -pa ebin/ -pa deps/jsx/ebin/

deps:
	git clone git@github.com:talentdeficit/jsx.git deps/jsx/
	cd deps/jsx && make
