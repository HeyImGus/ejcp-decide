# ejcp-decide


To run "make build", you need ocamlbuild and the Yojson package
It compiles main.ml with ocamlbuild.

To run "make test_all", you need a folder named input-DECIDE with Yojson files in it.
It tests the compiled program with all the files in the input-DECIDE folder.

After compiling, you can test on a single file with "./main.native FILE"