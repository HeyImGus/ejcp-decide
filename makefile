build : main.ml
	ocamlbuild -cflag -g -use-ocamlfind -package yojson main.native

test_all : build
	for json in input-DECIDE/*; do OCAMLRUNPARAM=b ./main.native -f $$json; done
