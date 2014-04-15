all: deps apps
.PHONY: all

APPS = $(patsubst apps/*/src/%.app.src,%,$(wildcard apps/*/src/*.app.src))

### DEPS -- Fetches & compiles deps recursively then moves every dep to deps/

deps: $(patsubst dep_%,deps/%/,$(filter dep_%,$(.VARIABLES))) | deps-dir
	$(if $(wildcard deps/*/deps/), \
	mv -v deps/*/deps/* deps/ 2>/dev/null ; rmdir $(wildcard deps/*/deps/))
.PHONY: deps

deps-dir:
	$(if $(wildcard deps/),,mkdir deps/)

deps/%/:
	git clone -n -- $(word 1,$(dep_$*)) $@
	cd $@ && git checkout -q $(word 2,$(dep_$*)) && cd ../..
	@bash -c "if [[ -f $@/Makefile ]]; \
	then echo 'make -C $@ all' ; make -C $@ all  ; \
	else echo 'cd $@ && rebar get-deps compile && cd ../..' ; \
	cd $@ && rebar get-deps compile && cd ../..  ; fi"

### APP -- Compiles src/ into ebin/

apps: $(APPS) 
	$(foreach ext, erl xrl yrl S core, \
	$(patsubst apps/*/src/%.$(ext), apps/*/ebin/%.beam, $(wildcard apps/*/src/*.$(ext)))) \
	$(patsubst apps/*/templates/%.dtl,  apps/*/ebin/%_dtl.beam,$(wildcard apps/*/templates/*.dtl))
	echo "-> $@"
.PHONY: apps

apps/*/ebin/%.app: apps/*/src/%.app.src | apps/*/ebin/
	@erl -noshell \
	-eval 'case file:consult("$<") of {ok,_} -> ok ; \
	{error,{_,_,M}} -> io:format("$<: ~s~s\n",M), halt(1) end.' \
	-s init stop
	cp $< $@
	echo "application -> $@"

apps/*/ebin/%.beam: apps/*/src/%.erl $(wildcard apps/*/include/*) | apps/*/ebin/ | deps/*/ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v -Iinclude/ -Ideps/ $<
	echo "compile -> $@"

apps/*/ebin/%_view.beam: apps/*/priv/templates/%.html | apps/*/ebin/
	$(if $(wildcard deps/erlydtl/),, \
	$(error Error compiling $<: deps/erlydtl/ not found))
	@erl -noshell -pa ebin/ -pa deps/*/ebin/ \
	-eval 'io:format("Compiling ErlyDTL template: $< -> $@\n").' \
	-eval 'erlydtl:compile("$<", $*_view, [{out_dir,"ebin/"},{auto_escape,false}]).' \
	-s init stop
	echo "template -> $@"

apps/*/ebin/:
	mkdir ebin/
	echo "-> $@"

#clean:
#	$(if $(wildcard apps/*/ebin/),rm -r ebin/)

#.PHONY: clean
