build : main.ml
	ocamlbuild -cflag -g -use-ocamlfind -package yojson main.native

test_all : build
	for json in input-DECIDE/*; do OCAMLRUNPARAM=b ./main.native $$json; done

stats : stats.ml
	ocamlbuild -cflags -g,-w,-A -use-ocamlfind -package yojson stats.native
	./stats.native
