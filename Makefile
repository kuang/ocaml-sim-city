test:
	ocamlbuild -use-ocamlfind -pkg oUnit state_test.byte && ./state_test.byte

compile:
	ocamlbuild -use-ocamlfind -pkg lablgtk2 -pkg async gui.byte

run:
	make compile && ./gui.byte

clean:
	ocamlbuild -clean
