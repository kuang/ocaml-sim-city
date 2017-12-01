test:
	ocamlbuild -use-ocamlfind -pkg oUnit state_test.byte && ./state_test.byte

compile:
	ocamlbuild -use-ocamlfind main.byte

run:
	make compile && ./main.byte

clean:
	ocamlbuild -clean
