.PHONY: clean console rebar deps get_deps

clean:
	rm -rf deps

console: deps
	erl -pa deps/jsx/ebin/

deps:
	git clone git@github.com:talentdeficit/jsx.git deps/jsx/
	cd deps/jsx && make
