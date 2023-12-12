all: phase1 phase2
.PHONY: all

# Création des .cmi

graph.cmi: graph.mli
	ocamlc -o $@ -c $<

analyse.cmi: analyse.mli
	ocamlc -o $@ -c $<

# Création des .cmo

graph.cmo: graph.ml graph.cmi
	ocamlc -o $@ -c $<

analyse.cmo: analyse.ml analyse.cmi
	ocamlc -o $@ -c $<

phase1.cmo: phase1.ml graph.cmo analyse.cmo
	ocamlc -o $@ -c $<

phase2.cmo: phase2.ml graph.cmo analyse.cmo
	ocamlc -o $@ -c $<

# Création des exécutables

phase1: graph.cmo analyse.cmo phase1.cmo
	ocamlc -o $@ $^

phase2: graph.cmo analyse.cmo phase2.cmo
	ocamlc -o $@ $^

# Autre

clean:
	rm -f *.cmi
	rm -f *.cmo
	rm -f phase1
	rm -f phase2
	rm -f *_out*
