
all: $(patsubst %.elm,%.html,$(wildcard src/*.elm))

%.html: %.elm
	elm make $< --output=$@ --debug
