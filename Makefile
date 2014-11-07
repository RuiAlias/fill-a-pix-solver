LISP_FINAL = submissao.lisp

FILES = projeto.lisp \
	exemplos.lisp


all: $(LISP_FINAL)

$(LISP_FINAL): $(FILES) # junta os ficheiros com uma quebra de linha entre eles
	sed -s "$$G" $^ > $@

clean:
	rm $(LISP_FINAL)

clean-windows:
	del $(LISP_FINAL)
