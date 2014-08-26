.PHONY: \
	fresh_build \
	deps \
	deps_get \
	deps_update \
	compile_all \
	compile_app \
	clean_all \
	clean_app \
	dialyze \
	test

ci: \
	fresh_build \
	remove_cover_spec_from_deps \
	test

# A kludge to avoid the conflict of cover.spec files with Travis' rebar
remove_cover_spec_from_deps:
	@find deps -name cover.spec -exec rm '{}' \;

fresh_build: \
	clean_all \
	deps \
	compile_all

deps: \
	deps_get \
	deps_update

deps_get:
	@rebar get-deps

deps_update:
	@rebar update-deps

compile_app:
	@rebar compile skip_deps=true

compile_all:
	@rebar compile skip_deps=false

console:
	@erl -pa `pwd`/deps/*/ebin -pa `pwd`/ebin

clean_all:
	@rebar clean skip_deps=false
	@rm -rf ebin/

clean_app:
	@rebar clean skip_deps=true
	@rm -rf ebin/

dialyze:
	@dialyzer deps/*/ebin ebin

test:
	@rebar ct skip_deps=true --verbose=0
