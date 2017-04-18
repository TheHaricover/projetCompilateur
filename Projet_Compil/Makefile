all: comp

# Compilation of Ocaml files
# Attention: order of object files important
comp: lang.cmo parser.cmo lexer.cmo analyses.cmo typing.cmo\
      instrs.cmo gen.cmo print_instr.cmo interf.cmo comp.cmo
	ocamlc -o comp $^

# Compilation of .ml files
lang.cmo: lang.ml
	ocamlc -c $<

analyses.cmo: analyses.ml lang.cmo
	ocamlc -c $<

typing.cmo: typing.ml analyses.cmo lang.cmo
	ocamlc -c $<

instrs.cmo:  instrs.ml lang.cmo
	ocamlc -c $<

interf.cmo: interf.ml lexer.cmo print_instr.cmo gen.cmo typing.cmo
	ocamlc -c $<

gen.cmo: gen.ml lang.cmo analyses.cmo instrs.cmo
	ocamlc -c $<

print_instr.cmo: print_instr.ml lang.cmo instrs.cmo
	ocamlc -c $<

comp.cmo: comp.ml print_instr.cmo gen.cmo typing.cmo parser.cmo interf.cmo
	ocamlc -c $<


# ocaml lexer and parser
lexer.ml: lexer.mll lang.cmo
	ocamllex $<

parser.ml parser.mli: parser.mly lang.cmo
	ocamlyacc $<

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c $<
parser.cmo: parser.ml parser.cmi lang.cmo
	ocamlc -c $<


#### Generic rules

%.cmi: %.mli
	ocamlc -c $<


.PHONY: clean

clean:
	rm -f lexer.ml parser.ml *.mli *.cmi *.cmo

.PHONY: test

test:
	#unit tests
	#For all the evaluations put the '<' between both words
	ocaml tests.ml
