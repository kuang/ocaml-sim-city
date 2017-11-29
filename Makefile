compile:
	ocamlbuild -use-ocamlfind main.byte

run:
	make compile && ./main.byte

clean:
	ocamlbuild -clean
