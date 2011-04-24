include ../../eliom.dev/Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}
export PATH := ${SRC}/src/tools:${PATH}

ELIOMC   := eliomc${BYTEDBG}
ELIOMOPT := eliomopt ${OPTDBG}
ELIOMDEP := eliomdep
JS_OF_ELIOM := js_of_eliom

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

#### Main site : eliom_testsuite ####

SERVER_PACKAGE :=
CLIENT_PACKAGE :=

SERVER_FILES := miniwiki.eliom
CLIENT_FILES := miniwiki.eliom \

STATICDIR := ../../local/var/www/miniwiki

byte:: miniwiki.cma ${STATICDIR}/miniwiki.js
opt:: miniwiki.cmxs ${STATICDIR}/miniwiki.js

#### Server side #######

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGE}}

SERVER_OBJS := $(patsubst %.eliom,_server/%.cmo, \
	          $(patsubst %.ml,_server/%.cmo,${SERVER_FILES}))

miniwiki.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $^
miniwiki.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $^

_server/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} -o $@ $<

_server/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $<

_server/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} $<
_server/%.cmo: %.eliom
	${ELIOMC} -c -noinfer ${SERVER_INC} $<

_server/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $<
_server/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

##### Client side ####

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGE}}

CLIENT_OBJS := $(patsubst %.eliom,_client/%.cmo, \
	          $(patsubst %.ml,_client/%.cmo,${CLIENT_FILES}))

${STATICDIR}/miniwiki.js:  ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ -jsopt -pretty -jsopt -noinline ${CLIENT_LIBS} $^

_client/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

_client/%.cmo: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<
_client/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

## Clean up

clean: clean.local
clean.local:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${STATICDIR}/miniwiki.js
	-rm -rf _client/* _server/*

distclean: clean.local
	-rm -f *~ \#* .\#*

## Dependencies

depend:
	$(ELIOMDEP) -server ${SERVER_INC} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client ${CLIENT_INC} ${CLIENT_FILES} >> .depend

include .depend

## Dependencies towards *.eliom are not handled by eliomdep.
#eliom_testsuite.cmo: eliom_testsuite3.cmo
#eliom_testsuite.cmx: eliom_testsuite3.cmx

## TODO

# %.wiki: %.ml
# cat $< | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > $@
