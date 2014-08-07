.PHONY: \
	fresh_build \
	deps \
	deps_get \
	deps_update \
	compile \
	clean \
	dialyze

fresh_build: \
	clean \
	deps \
	compile

deps: \
	deps_get \
	deps_update

deps_get:
	@rebar get-deps

deps_update:
	@rebar update-deps

compile:
	@rebar compile

console:
	@erl -pa `pwd`/deps/*/ebin -pa `pwd`/ebin

clean:
	@rebar clean
	@rm -rf ebin/

dialyze:
	@dialyzer deps/*/ebin ebin
