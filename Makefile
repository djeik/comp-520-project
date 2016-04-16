GOTO = dist/build/goto/goto
ARCH = macho64

.PRECIOUS: %.asm %.o

.PHONY: $(GOTO)
$(GOTO):
	cabal build

%.asm: %.go $(GOTO)
	$(GOTO) pretty < $< > $@

%.o: %.asm
	yasm -f $(ARCH) $^ -o $@

%.asd: %.o runtime/goto.o
	gcc -Xlinker -no_pie $^ -o $@

runtime/goto.o:
	make -C runtime
