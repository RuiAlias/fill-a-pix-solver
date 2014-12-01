;;;;; Grupo 30 Taguspark - 77213 Rui Silva, 82134 Jorge Almeida

(load "exemplos.fas")

;;;; 2.1.1 Tipo Restricao
;;; cria-restricao: lista de variaveis x predicado -> restricao

(defstruct (restricao (:constructor cria-restricao (variaveis funcao-validacao))) 
  "Tipo restricao caracterizado por uma lista das variaveis envolvidas na restricao, e uma funcao
  que verifica a restricao."
  variaveis ; lista de variaveis
  funcao-validacao) ; predicado

;;; restricao-variaveis: restricao -> lista de variaveis

;;; restricao-funcao-validacao: restricao -> predicado

;;;; 2.1.2 Tipo PSR
(defstruct psr
  "Tipo PSR (Problema de Satisfacao de Restricoes)"
  variaveis-todas  ; lista com todas as variaveis por ordem
  restricoes       ; lista de restricoes
  hash-d           ; hashtable dominio (variavel->dominio)
  hash-a           ; hatribuicoes (variavel->valor)
  hash-r)          ; restricoes (variavel->lista de restricoes)


;;; cria-psr: lista variaveis x lista de dominios x lista de restricoes -> PSR
(defun cria-psr (variaveis dominios restricoes)
  ""
  (let ((hash-d (make-hash-table :test 'equal))
	(hash-a (make-hash-table :test 'equal))
	(hash-r (make-hash-table :test 'equal)))
    (mapcar #'(lambda (v d) (setf (gethash v hash-d) d)) variaveis dominios)
    (dolist (r restricoes)
      (dolist (v (restricao-variaveis r) v)
	(push r (gethash v hash-r))))
    (maphash #'(lambda (v r) (declare (ignore v)) (nreverse r)) hash-r)
    (make-psr :variaveis-todas variaveis
	      :restricoes restricoes
	      :hash-d hash-d
	      :hash-a hash-a
	      :hash-r hash-r)))


;;; psr-atribuicoes: PSR -> lista atribuicoes
(defun psr-atribuicoes (p)
  "Retorna uma lista com todas as atribuicoes - pares (variavel . valor) - do PSR."
  (loop for variavel being the hash-keys in (psr-hash-a p) using (hash-value valor)
     collect (cons variavel valor)))


;;; psr-variaveis-todas: PSR -> lista variaveis


;;; psr-variaveis-nao-atribuidas: PSR -> lista de variaveis
(defun psr-variaveis-nao-atribuidas (p)
  "Devolve lista de variaveis nao atribuidas (pela ordem inicial)."
  (loop for v in (psr-variaveis-todas p) when (not (psr-variavel-atribuida-p p v)) collect v))


;;; psr-variavel-valor: PSR x variavel -> objecto
(defun psr-variavel-valor (p v)
  "Devolve o valor atribuido a variavel (caso nao exista atribuicao devolve nil)."
  (nth-value 0 (gethash v (psr-hash-a p))))


;;; psr-variavel-dominio: PSR x variavel -> dominio
(defun psr-variavel-dominio (p v)
  "Devolve o dominio associado a uma variavel."
  (if (psr-variavel-atribuida-p p v)
      (list (psr-variavel-valor p v))
      (nth-value 0 (gethash v (psr-hash-d p)))))


(defun psr-variavel-atribuida-p (p v)
  "Devolve booleano que indica se a variavel esta ou nao atribuida."
  (psr-variavel-valor p v))


;;; psr-variavel-restricoes: PSR x variavel -> lista restricoes
(defun psr-variavel-restricoes (p v)
  "Devolve uma lista com todas as restricoes aplicaveis a uma variavel."
  (nth-value 0 (gethash v (psr-hash-r p))))


;;; psr-adiciona-atribuicao!: PSR x variavel x valor -> {}
(defun psr-adiciona-atribuicao! (p v valor)
  ""
  (setf (gethash v (psr-hash-a p)) valor))


;;; psr-remove-atribuicao!: PSR x variavel -> {}
(defun psr-remove-atribuicao! (p v)
  ""
  (remhash v (psr-hash-a p)))


;;; psr-altera-dominio!: PSR x variavel x dominio {}
(defun psr-altera-dominio! (p v d)
  ""
  (setf (gethash v (psr-hash-d p)) d))


;;; psr-completo-p: PSR -> logico
(defun psr-completo-p (p)
  ""
  (null (psr-variaveis-nao-atribuidas p)))


;;; AUXILIAR
(defun psr-testa-restricoes (p restricoes)
  "Testa uma lista de restricoes e devolve dois valores. O primeiro, um boleano que indica se todas
  as restricoes se verificam e o segundo indica o numero de testes realizados."
  (let ((testes 0)) 
    (dolist (r restricoes (values t testes)) 
      (incf testes) 
      (when (null (funcall (restricao-funcao-validacao r) p)) (return (values nil testes))))))


;;; psr-consistente-p: PSR -> logico, inteiro
(defun psr-consistente-p (p)
  "Testa todas as restricoes do psr."
  (psr-testa-restricoes p (psr-restricoes p)))


;;; psr-variavel-consistente-p: PSR x variavel -> logico, inteiro
(defun psr-variavel-consistente-p (p v)
  "Testa as restricoes associadas a variavel 'v'."
  (psr-testa-restricoes p (psr-variavel-restricoes p v)))


;;; psr-atribuicao-consistente-p: PSR x variavel x valor -> logico, inteiro
(defun psr-atribuicao-consistente-p (p v valor)
  ""
  (let ((atribuicao (psr-variavel-valor p v))
	(consistente t)
	(testes 0))

    (psr-adiciona-atribuicao! p v valor)
    (setf (values consistente testes) (psr-variavel-consistente-p p v))

    (if atribuicao (psr-adiciona-atribuicao! p v atribuicao) (psr-remove-atribuicao! p v))
    (values consistente testes)))


;;; psr-atribuicoes-consistentes-arco-p: PSR x variavel x valor x variavel x valor -> logico, inteiro
(defun psr-atribuicoes-consistentes-arco-p (p v1 valor1 v2 valor2)
  ""
  (let ((atribuicao1 (psr-variavel-valor p v1))
	(atribuicao2 (psr-variavel-valor p v2))
	(consistente t)
	(testes 0))

    (psr-adiciona-atribuicao! p v1 valor1)
    (psr-adiciona-atribuicao! p v2 valor2)
    (dolist (r (psr-variavel-restricoes p v1))
      (when (find v2 (restricao-variaveis r) :test 'equal)
	(incf testes)
	(when (null (funcall (restricao-funcao-validacao r) p))
	  (setf consistente nil)
	  (return))))

    (if atribuicao1 (psr-adiciona-atribuicao! p v1 atribuicao1) (psr-remove-atribuicao! p v1))
    (if atribuicao2 (psr-adiciona-atribuicao! p v2 atribuicao2) (psr-remove-atribuicao! p v2))
    (values consistente testes)))


;;;; 2.2.1 Funcoes de conversao

(defun variaveis-a-volta (linha coluna max-linha max-coluna)
  "Devolve uma lista com as coordenadas, em formato string '(linha . coluna)', a volta da coordenada
   fornecida tendo em conta as dimensoes do tabuleiro (0 <= linhas  < max-linha e 
                                                       0 <= colunas < max-coluna)."
  (let ((variaveis (list))) 
    (loop for delta-l from -1 to 1 do
	 (loop for delta-c from -1 to 1 do
	      (let ((l (+ linha delta-l)) (c (+ coluna delta-c))) 
		(when (and (>= l 0) (< l max-linha) (>= c 0) (< c max-coluna)) 
		  (push (write-to-string (cons l c)) variaveis)))))
    (nreverse variaveis)))


(defun psr-conta-valor (p variaveis valor)
  "Devolve o numero de variaveis (do argumento) que tem como valor o 'valor'."
  (count valor variaveis :key #'(lambda (v) (psr-variavel-valor p v)) :test 'eql)) ; TODO: eql vs =


;;; fill-a-pix->psr: array -> PSR
(defun fill-a-pix->psr (tab) ; tab de tabuleiro
  ""
  (let ((variaveis (list)) (dominios (list)) (restricoes (list)))
    (dotimes (l (array-dimension tab 0))   ; linha
      (dotimes (c (array-dimension tab 1)) ; coluna
	(push (write-to-string (cons l c)) variaveis)
	(push (list 0 1) dominios)
	(when (numberp (aref tab l c))
	  (let ((vav (variaveis-a-volta l c (array-dimension tab 0) (array-dimension tab 1)))
		(numero (aref tab l c))) ; TODO testar se e mesmo preciso e se sim passar para antes do when
	    (push 
	     (cria-restricao 
	      vav
	      #'(lambda (p) (let ((pretas (psr-conta-valor p vav 1))
				  (brancas (psr-conta-valor p vav 0)))
			      (and (<= pretas numero) (<= brancas (- (length vav) numero))))))
	     restricoes)))))
    (cria-psr (nreverse variaveis) (nreverse dominios) (nreverse restricoes))))


(defun lista->lista2d (lista linhas colunas)
  "Recebe uma lista de listas e devolve uma nova lista em formato de matriz."
  (loop for l below linhas
     collect (loop for c below colunas
		append (nth (+ (* l colunas) c) lista))))


;;; psr->fill-a-pix: PSR x inteiro x inteiro -> array
(defun psr->fill-a-pix (p linhas colunas)
  ""
  (let ((tab (make-array (list linhas colunas)))
	(i 0))
    (dolist (v (psr-variaveis-todas p) tab)
      (setf (aref tab (floor i colunas) (mod i colunas)) (psr-variavel-valor p v))
      (incf i))))

;;;; 2.2.2

;;; procura-retrocesso-simples: PSR -> PSR, inteiro
(defun procura-retrocesso-simples (p)
  ""
  (let ((testes-total 0))
    (if (psr-completo-p p)
	(return-from procura-retrocesso-simples (values p testes-total))
	(let* ((v (first (psr-variaveis-nao-atribuidas p)))
	       (d (psr-variavel-dominio p v)))

	  (dolist (valor d)
	    (multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	      (incf testes-total testes)
	      (when consistente
		(psr-adiciona-atribuicao! p v valor)
		(multiple-value-bind (recurs-consistente recurs-testes) (procura-retrocesso-simples p)
		  (incf testes-total recurs-testes)
		  (when (not (null recurs-consistente))
		    (return-from procura-retrocesso-simples (values p testes-total))))
		(psr-remove-atribuicao! p v))))
	  (return-from procura-retrocesso-simples (values nil testes-total))))))


;;; resolve-simples: array -> array
(defun resolve-simples (tab)
  ""
  (multiple-value-bind (p) (procura-retrocesso-simples (fill-a-pix->psr tab))
    (if (null p) nil (psr->fill-a-pix p (array-dimension tab 0) (array-dimension tab 1)))))


;;; resolve-simples: array -> array
(defun resolve-grau (tab)
  ""
  (multiple-value-bind (p) (procura-retrocesso-grau (fill-a-pix->psr tab))
    (if (null p) nil (psr->fill-a-pix p (array-dimension tab 0) (array-dimension tab 1)))))

;;; resolve-simples: array -> array
(defun resolve-fc-mrv (tab)
  ""
  (multiple-value-bind (p) (procura-retrocesso-fc-mrv (fill-a-pix->psr tab))
    (if (null p) nil (psr->fill-a-pix p (array-dimension tab 0) (array-dimension tab 1)))))


;;; resolve-simples: array -> array
(defun resolve-mac-mrv (tab)
  ""
  (multiple-value-bind (p) (procura-retrocesso-mac-mrv (fill-a-pix->psr tab))
    (if (null p) nil (psr->fill-a-pix p (array-dimension tab 0) (array-dimension tab 1)))))


(defun n-restricoes-c-natribuidas (p v)
  ""
  (count-if #'(lambda (r) (some #'(lambda (v1) (and (not (equal v1 v))
						    (null (psr-variavel-atribuida-p p v1))))
				(restricao-variaveis r)))
	    (psr-variavel-restricoes p v)))


(defun psr-var-maior-grau (p)
  ""
  (first (sort (psr-variaveis-nao-atribuidas p)
	       #'>
	       :key #'(lambda (v) (n-restricoes-c-natribuidas p v)))))


;;; procura-retrocesso-grau: PSR -> PSR, inteiro
(defun procura-retrocesso-grau (p)
  ""
  (let ((testes-total 0))
    (if (psr-completo-p p)
	(return-from procura-retrocesso-grau (values p testes-total))
	(let* ((v (psr-var-maior-grau p))
	       (d (psr-variavel-dominio p v)))

	  (dolist (valor d)
	    (multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	      (incf testes-total testes)
	      (when consistente
		(psr-adiciona-atribuicao! p v valor)

		(multiple-value-bind (recurs-consistente recurs-testes) (procura-retrocesso-grau p)
		  (incf testes-total recurs-testes)

		  (when (not (null recurs-consistente))
		    (return-from procura-retrocesso-grau (values p testes-total))))

		(psr-remove-atribuicao! p v))))
	  (return-from procura-retrocesso-grau (values nil testes-total))))))


(defun psr-mrv (p)
  ""
  (let* ((lista-vna (psr-variaveis-nao-atribuidas p))
	 (min-v (first lista-vna))
	 (min-length (length (psr-variavel-dominio p min-v))))

    (dolist (v lista-vna)
      (let ((d-length (length (psr-variavel-dominio p v))))
	(when (< d-length min-length)
	  (setf min-length d-length)
	  (setf min-v v))))

    min-v))


;;; procura-retrocesso-fc-mrv: PSR -> PSR, inteiro
(defun procura-retrocesso-fc-mrv (p)
  ""
  (let ((testes-totais 0))
    (when (psr-completo-p p) (return-from procura-retrocesso-fc-mrv (values p testes-totais)))

    (let* ((v (psr-mrv p)))
      (dolist (valor (psr-variavel-dominio p v))
	(multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	  (incf testes-totais testes)
	  (when consistente
	    (psr-adiciona-atribuicao! p v valor)

	    (multiple-value-bind (inferencias testes) (psr-forward-checking p v)
	      (incf testes-totais testes)
	      (when inferencias
		(let ((backup (make-hash-table :test 'equal)))
		  (maphash #'(lambda (iv id)
			       (setf (gethash iv backup) (psr-variavel-dominio p iv))
			       (psr-altera-dominio! p iv id))
			   inferencias)

		  (multiple-value-bind (resultado testes) (procura-retrocesso-fc-mrv p)
		    (incf testes-totais testes)
		    (when resultado
		      (return-from procura-retrocesso-fc-mrv (values resultado testes-totais))))

		  (maphash #'(lambda (bv bd)
			       (psr-altera-dominio! p bv bd))
			   backup)))

	      (psr-remove-atribuicao! p v))))))

    (return-from procura-retrocesso-fc-mrv (values nil testes-totais))))


(defun psr-forward-checking (p v)
  ""
  (let ((testes-totais 0)
	(inferencias (make-hash-table :test 'equal))
	(lista-arcos (psr-arcos-vizinhos-nao-atribuidos2 p v)))

    (dolist (arco lista-arcos)
      (let ((v2 (first arco))
	    (v1 (rest arco)))

	(multiple-value-bind (revise testes) (psr-revise2 p v2 v1 inferencias)
	  (incf testes-totais testes)
	  (when revise
	    (multiple-value-bind (d exists) (gethash v2 inferencias)
	      (when exists
		(when (= (length d) 0)
		  (return-from psr-forward-checking (values nil testes-totais)))))))))

    (return-from psr-forward-checking (values inferencias testes-totais))))


(defun psr-arcos-vizinhos-nao-atribuidos2 (p v)
  ""
  (let ((lista-arcos (list))
	(envolvidas (remove-duplicates (reduce #'append (psr-variavel-restricoes p v)
					       :key 'restricao-variaveis)
				       :test 'equal)))
    (dolist (v-na (psr-variaveis-nao-atribuidas p))
      (when (not (equal v v-na))
	(when (find v-na envolvidas :test 'equal)
	  (push (cons v-na v) lista-arcos))))

    (nreverse lista-arcos)))


(defun psr-revise2 (p x y inferencias)
  ""
  (let* ((testes-totais 0)
	 (revised nil)
	 (dominio-x (gethash x inferencias (psr-variavel-dominio p x)))
	 (novo-dominio-x dominio-x)
	 (dominio-y (if (psr-variavel-atribuida-p p y)
			(list (psr-variavel-valor p y))
			(gethash y inferencias (psr-variavel-dominio p y)))))

    (dolist (valor-x dominio-x)
      (let ((found-consistent-value nil))
	(dolist (valor-y dominio-y)
	  (multiple-value-bind (consistente testes) (psr-atribuicoes-consistentes-arco-p p x valor-x y valor-y)
	    (incf testes-totais testes)
	    (when consistente
	      (setf found-consistent-value t)
	      (return))))
	(when (null found-consistent-value)
	  (setf revised t)
	  (setf novo-dominio-x (remove valor-x novo-dominio-x)))))

    (when revised
      (setf (gethash x inferencias) novo-dominio-x))

    (values revised testes-totais)))


;;; procura-retrocesso-mac-mrv: PSR -> PSR, inteiro
(defun procura-retrocesso-mac-mrv (p)
  ""
  (let ((testes-totais 0))
    (when (psr-completo-p p) (return-from procura-retrocesso-mac-mrv (values p testes-totais)))

    (let* ((v (psr-mrv p)))
      (dolist (valor (psr-variavel-dominio p v))
	(multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	  (incf testes-totais testes)
	  (when consistente
	    (psr-adiciona-atribuicao! p v valor)

	    (multiple-value-bind (inferencias testes) (psr-mac p v)
	      (incf testes-totais testes)
	      (when inferencias
		(let ((backup (make-hash-table :test 'equal)))
		  (maphash #'(lambda (iv id)
			       (setf (gethash iv backup) (psr-variavel-dominio p iv))
			       (psr-altera-dominio! p iv id))
			   inferencias)

		  (multiple-value-bind (resultado testes) (procura-retrocesso-mac-mrv p)
		    (incf testes-totais testes)
		    (when resultado
		      (return-from procura-retrocesso-mac-mrv (values resultado testes-totais))))

		  (maphash #'(lambda (bv bd)
			       (psr-altera-dominio! p bv bd))
			   backup)))

	      (psr-remove-atribuicao! p v))))))

    (return-from procura-retrocesso-mac-mrv (values nil testes-totais))))


(defun psr-mac (p v)
  ""
  (let ((testes-totais 0)
	(inferencias (make-hash-table :test 'equal))
	(lista-arcos (psr-arcos-vizinhos-nao-atribuidos2 p v)))

    (dolist (arco lista-arcos)
      (let ((v2 (first arco))
	    (v1 (rest arco)))

	(multiple-value-bind (revise testes) (psr-revise2 p v2 v1 inferencias)
	  (incf testes-totais testes)
	  (when revise
	    (multiple-value-bind (d exists) (gethash v2 inferencias)
	      (when exists
		(when (= (length d) 0)
		  (return-from psr-mac (values nil testes-totais)))))

	    (nconc lista-arcos (remove (cons v1 v2)
				       (psr-arcos-vizinhos-nao-atribuidos2 p v2)
				       :test 'equal))))))

    (return-from psr-mac (values inferencias testes-totais))))

;;; -----------------------------------------------------------------------

(defstruct restricao-fapix
  ""
  ipixs
  funcao-validacao)

(defstruct fapix
  ""
  linhas ; numero de linhas do tabuleiro
  colunas ; numero de colunas do tabuleiro
  max-ipix ; numero posicoes do tabuleiro
  dominio ; array: dominio de cada pix
  atribuicao ; array: atribuicao de cada pix
  restricoes ; array: lista de restricoes associadas a cada pix
  a-volta ; array: lista de indices de pixs a volta de cada pix
  relacionados ; array: lista de pixs relacionados por uma restricao de cada pix
  natribuidos ; lista: indices de pixs nao atribuidos
  solucionavel ; valor logico: auxiliar para tabuleiros impossiveis
  dominio-len ; array: listas com indices de pixs por comprimento de dominio
  pix ; array: numero da casa no tabuleiro de cada pix (nil quando nao tem numero)
  hv) ; array: valor da heuristica de valor (acabou por nao ser usado visto nao se ter encontrado
      ; calculo util e eficiente)


(defun cria-restricao-fapix (ipixs pix a-volta-len)
  ""
  (make-restricao-fapix :ipixs ipixs
			:funcao-validacao #'(lambda (f) (multiple-value-bind (brancos pretos)
							    (fapix-conta-cores f ipixs)
							  (and (<= pretos pix)
							       (<= brancos (- a-volta-len pix)))))))

(defun fapix-ipixs-nao-atribuidos (f)
  "Recebe um `fapix' e devolve a lista de ipixs nao atribuidos desse fapix."
  (fapix-natribuidos f))

(defun fapix-pix-cor (f ipix)
  "Recebe um `fapix' e um indice de um pix e devolve a cor (atribuicao) associada ao pix ou nil caso
 nao tenha atribuicao."
  (aref (fapix-atribuicao f) ipix))

(defun fapix-pix-dominio (f ipix)
  "Recebe um `fapix' e um indice de um pix e devolve o dominio associado ao pix."
  (let ((cor (fapix-pix-cor f ipix)))
    (if cor
	(list cor)
	(aref (fapix-dominio f) ipix))))

(defun fapix-pix-restricoes (f ipix)
  "Recebe um `fapix' e um indice de um pix e devolve uma lista com todas as restricoes aplicaveis ao
 pix."
  (aref (fapix-restricoes f) ipix))

(defun fapix-adiciona-atribuicao! (f ipix cor)
  "Recebe um `fapix', um indice de um pix e uma cor (0 para branco e 1 para preto) e atribui a cor 
ao pix."
  (setf (aref (fapix-atribuicao f) ipix) cor))

(defun fapix-remove-atribuicao! (f ipix)
  "Recebe um `fapix' e um indice de um pix e remove a atribuicao de cor do pix."
  (fapix-adiciona-atribuicao! f ipix nil))

(defun fapix-altera-dominio! (f ipix d)
  "Recebe um `fapix', um indice de um pix e um dominio (lista de cores), atribui esse dominio ao pix
e atualiza as listas de comprimento de dominio."
  (setf (aref (fapix-dominio-len f) (length (fapix-pix-dominio f ipix)))
	(remove ipix (aref (fapix-dominio-len f) (length (fapix-pix-dominio f ipix)))))
  (setf (aref (fapix-dominio f) ipix) d)
  (push ipix (aref (fapix-dominio-len f) (length d))))

(defun fapix-completo-p (f)
  "Recebe um `fapix' e testa se o fapix esta completo (se nao tem variaveis sem atribuicao). Retorna
T se estiver completo e nil caso contrario."
  (null (fapix-ipixs-nao-atribuidos f)))

(defun fapix-testa-restricoes (f restricoes)
  "Recebe um `fapix' e uma lista de `restricao-fapix' e testa todas as restricoes. Retorna dois 
valores. O primeiro e um valor logico que indica se todas as restricoes se verificam. O segundo e
a ultima restricao a ser testada caso nem todas as restricoes se verifiquem e NIL caso contrario. Se
a lista de restricoes estiver vazia e devolvido T e NIL."
  (dolist (r restricoes (values t nil))
    (when (null (funcall (restricao-fapix-funcao-validacao r) f))
      (return (values nil r)))))

(defun fapix-consistente-p (f)
  "Recebe um `fapix' e testa todas as retricoes do mesmo. Retorna o mesmo que 
`fapix-testa-restricoes'"
  (fapix-testa-restricoes f (fapix-restricoes f)))

(defun fapix-pix-consistente-p (f ipix)
  "Recebe um `fapix' e um indice de um pix e testa as restricoes associadas ao pix. Retorna o mesmo 
que `fapix-testa-restricoes'."
  (fapix-testa-restricoes f (fapix-pix-restricoes f ipix)))

(defun fapix-atribuicao-consistente-p (f ipix cor)
  "Recebe um `fapix', um indice de um pix e uma cor. Testa se a atribuicao da cor ao pix e 
consistente com as restricoes associadas ao pix. Retorna o mesmo que `fapix-testa-restricoes'."
  (let ((atribuicao (fapix-pix-cor f ipix))
	(consistente t)
	(falhou nil))

    (fapix-adiciona-atribuicao! f ipix cor)

    (setf (values consistente falhou) (fapix-pix-consistente-p f ipix))

    (if atribuicao (fapix-adiciona-atribuicao! f ipix atribuicao) (fapix-remove-atribuicao! f ipix))
    (values consistente falhou)))

(defun fapix-atribuicoes-consistentes-p (f ipix1 cor1 ipix2 cor2)
  "Recebe um `fapix', dois indices de pixs e duas cores. Testa se a atribuicao da cor1 ao pix1 e da 
cor2 ao pix2 e consistente com as restricoes associadas a ambos os pix (testa as restricoes de forma
 sequencial - primeiro as do pix1 e depois as do pix2). Retorna o mesmo que `fapix-testa-restricoes'
."
  (let ((atribuicao1 (fapix-pix-cor f ipix1))
	(atribuicao2 (fapix-pix-cor f ipix2))
	(consistente t))

    (fapix-adiciona-atribuicao! f ipix1 cor1)
    (fapix-adiciona-atribuicao! f ipix2 cor2)

    (block testes
      (when (fapix-testa-restricoes f (fapix-pix-restricoes f ipix1))
	(when (fapix-testa-restricoes f (fapix-pix-restricoes f ipix2))
	  (return-from testes)))
      (setf consistente nil))

    (if atribuicao1 (fapix-adiciona-atribuicao! f ipix1 atribuicao1) (fapix-remove-atribuicao! f ipix1))
    (if atribuicao2 (fapix-adiciona-atribuicao! f ipix2 atribuicao2) (fapix-remove-atribuicao! f ipix2))
    (values consistente 0)))

(defun fapix-conta-cores (f ipixs)
  "Recebe um `fapix' e uma lista de indices de pixs. Retorna dois valores: quantos pixs brancos e 
pretos ha na lista."
  (let ((brancos 0)
	(pretos 0))

    (dolist (ipix ipixs (values brancos pretos))
      (let ((cor (fapix-pix-cor f ipix)))
	(when cor
	  (if (= cor 0)
	      (incf brancos)
	      (incf pretos)))))))

(defun linha (ipix colunas)
  "Devolve a linha em que se encontra o pix correspondente ao indice fornecido."
  (nth-value 0 (floor ipix colunas)))

(defun coluna (ipix colunas)
  "Devolve a coluna em que se encontra o pix correspondente ao indice fornecido."
  (nth-value 1 (floor ipix colunas)))

(defun pix-a-volta (ipix colunas max-ipix)
  "Devolve uma lista com os indices dos pix a volta do pix cujo o indice fornecido."
  (loop for l from (- ipix colunas) to (+ ipix colunas) by colunas when (< -1 l max-ipix)
    append (loop for c from (- l 1) to (+ l 1) when (= (linha c colunas) (linha l colunas))
      collect c)))

(defun fill-a-pix->fapix (tab) ; tab de tabuleiro
  "Converte um tabuleiro fill-a-pix para um `fapix'."
  (let* ((linhas (array-dimension tab 0))
	 (colunas (array-dimension tab 1))
	 (max-ipix (* linhas colunas))
	 (pix-dominio (make-array max-ipix :initial-element (list 0 1)))
	 (pix-atribuicoes (make-array max-ipix :initial-element nil))
	 (pix-restricoes (make-array max-ipix :initial-element nil))
	 (pix-a-volta (make-array max-ipix :initial-element nil))
	 (pix-relacionados (make-array max-ipix :initial-element nil))
	 (pix-natribuidos (list))
	 (pix-solucionavel t) ; t de talvez
	 (dominio-len (make-array 3 :initial-element nil))
	 (pix-dominio-len-p (make-array max-ipix :initial-element nil))
	 (pix-pix (make-array max-ipix :initial-element nil))
	 (pix-hv (make-array max-ipix :initial-element 0))
	 (ipix 0))

    (block ciclos
      (dotimes (l linhas)
	(dotimes (c colunas)
	  (let* ((pix (aref tab l c))
		 (a-volta (pix-a-volta ipix colunas max-ipix))
		 (a-volta-len (length a-volta)))

	    (setf (aref pix-a-volta ipix) a-volta)
	    (push ipix pix-natribuidos)

	    (when (numberp pix)
	      (when (> pix a-volta-len) (setf pix-solucionavel nil) (return-from ciclos))

	      (setf (aref pix-pix ipix) pix)
	      (let* ((restricao (cria-restricao-fapix a-volta pix a-volta-len))
		     (dominio (if (= pix 0) (list 0) (if (= pix a-volta-len) (list 1) nil))))

		(dolist (ipix-av a-volta)
		  (push restricao (aref pix-restricoes ipix-av))

		  (setf (aref pix-relacionados ipix-av) (merge 'list
							       (aref pix-relacionados ipix-av)
							       (remove ipix-av a-volta :test '=)
							       #'<))

		  (when dominio
		    (setf (aref pix-dominio ipix-av) dominio)
		    (push ipix-av (aref dominio-len 1))
		    (setf (aref pix-dominio-len-p ipix-av) t)))))

	    (let* ((soma 0)
		   (numero 0))

	      (dolist (ipix-av a-volta)
		(let ((pix-av (aref tab (linha ipix-av colunas) (coluna ipix-av colunas))))
		  (when pix-av
		    (incf soma pix-av)
		    (incf numero))))

;;	      (when (and (not (= numero 0)) (>= (round (round soma numero) a-volta-len) 1))
;;		(setf (aref pix-hv ipix) 1)
;;		))


	    (incf ipix)))))

    (dotimes (i max-ipix)
      (when (not (aref pix-dominio-len-p i))
	(push i (aref dominio-len 2)))
      (delete-duplicates (aref pix-relacionados i)))

    (make-fapix :linhas linhas
		:colunas colunas
		:max-ipix max-ipix
		:dominio pix-dominio
		:atribuicao pix-atribuicoes
		:restricoes pix-restricoes
		:a-volta pix-a-volta
		:relacionados pix-relacionados
		:natribuidos pix-natribuidos
		:solucionavel pix-solucionavel
		:dominio-len dominio-len
		:pix pix-pix
		:hv pix-hv)))

(defun fapix->fill-a-pix (f)
  "Converte um `fapix' para um tabuleiro fill-a-pix."
  (let* ((colunas (fapix-colunas f))
	 (tab (make-array (list (fapix-linhas f) colunas))))
    (dotimes (ipix (fapix-max-ipix f) tab)
      (setf (aref tab (linha ipix colunas) (coluna ipix colunas)) (fapix-pix-cor f ipix)))))

(defun algoritmo (f)
  ""
  (when (not (fapix-solucionavel f)) (return-from algoritmo nil))
  
  (procura-retrocesso-mac-mrv-fapix f))

(defun resolve-best (tab)
  ""
  (fapix->fill-a-pix (algoritmo (fill-a-pix->fapix tab))))

(defun fapix-mrv-modificado (f)
  ""
  (dolist (ipix (aref (fapix-dominio-len f) 1))
    (return-from fapix-mrv-modificado ipix))

  ;; criterio de desempate para quando o comprimento do dominio e 2
  (let* ((max-ipix (first (aref (fapix-dominio-len f) 2)))
	 (a-volta (aref (fapix-a-volta f) max-ipix))
	 (max-atrib-a-volta (count-if #'(lambda (ipix-av) (fapix-pix-cor f ipix-av)) a-volta))
	 (max-natrib-a-volta (- (length a-volta) max-atrib-a-volta)))

    (dolist (ipix (aref (fapix-dominio-len f) 2) max-ipix)
      (setf a-volta (aref (fapix-a-volta f) ipix))
      (let* ((atrib-a-volta (count-if #'(lambda (ipix-av) (fapix-pix-cor f ipix-av)) a-volta))
	     (natrib-a-volta (- (length a-volta) atrib-a-volta)))

	(when (or (> atrib-a-volta max-atrib-a-volta)
		  (and (= atrib-a-volta max-atrib-a-volta) (< natrib-a-volta max-natrib-a-volta)))
	  (setf max-atrib-a-volta atrib-a-volta)
	  (setf max-natrib-a-volta natrib-a-volta)
	  (setf max-ipix ipix))))))

(defun procura-retrocesso-fc-mrv-fapix (f)
  ""
  (when (fapix-completo-p f)
    (return-from procura-retrocesso-fc-mrv-fapix (values f 0)))

  (let* ((ipix (fapix-mrv-modificado f))
	 (d (fapix-pix-dominio f ipix)))

    ;; (when (= (length d) 2)
    ;;   (when (= (aref (fapix-hv f) ipix) 1)
    ;; 	(setf d (reverse d))))

    (dolist (cor d)
      (fapix-adiciona-atribuicao! f ipix cor)
      (setf (fapix-natribuidos f) (remove ipix (fapix-natribuidos f)))
      (setf (aref (fapix-dominio-len f) (length d))
	    (remove ipix (aref (fapix-dominio-len f) (length d))))

      (let ((inferencias (fapix-forward-checking f ipix)))
	(when inferencias
	  (let ((backup (make-hash-table :test 'eql)))
	    (maphash #'(lambda (iipix id)
			 (setf (gethash iipix backup) (fapix-pix-dominio f iipix))
			 (fapix-altera-dominio! f iipix id))
		     inferencias)

	    (multiple-value-bind (resultado) (procura-retrocesso-fc-mrv-fapix f)
	      (when resultado
		(return-from procura-retrocesso-fc-mrv-fapix (values resultado 0))))

	    (maphash #'(lambda (bipix bd)
			 (fapix-altera-dominio! f bipix bd))
		     backup)))

	(fapix-remove-atribuicao! f ipix)
	(push ipix (fapix-natribuidos f))
	(push ipix (aref (fapix-dominio-len f) (length d))))))

  (return-from procura-retrocesso-fc-mrv-fapix (values nil 0)))

(defun procura-retrocesso-mac-mrv-fapix (f)
  ""
  (when (fapix-completo-p f)
    (return-from procura-retrocesso-mac-mrv-fapix (values f 0)))

  (let* ((ipix (fapix-mrv-modificado f))
	 (d (fapix-pix-dominio f ipix)))

    ;; (when (= (length d) 2)
    ;;   (when (= (aref (fapix-hv f) ipix) 1)
    ;; 	(setf d (reverse d))))

    (dolist (cor d)
      (fapix-adiciona-atribuicao! f ipix cor)
      (setf (fapix-natribuidos f) (remove ipix (fapix-natribuidos f)))

      (setf (aref (fapix-dominio-len f) (length d))
	    (remove ipix (aref (fapix-dominio-len f) (length d))))

      (let ((inferencias (fapix-mac f ipix)))

	(when inferencias
	  (let ((backup (make-hash-table :test 'eql)))
	    (maphash #'(lambda (iipix id)
			 (setf (gethash iipix backup) (fapix-pix-dominio f iipix))
			 (fapix-altera-dominio! f iipix id))
		     inferencias)

	    (multiple-value-bind (resultado) (procura-retrocesso-mac-mrv-fapix f)
	      (when resultado
		(return-from procura-retrocesso-mac-mrv-fapix (values resultado 0))))

	    (maphash #'(lambda (bipix bd)
			 (fapix-altera-dominio! f bipix bd))
		     backup)))

	(fapix-remove-atribuicao! f ipix)
	(push ipix (fapix-natribuidos f))
	(push ipix (aref (fapix-dominio-len f) (length d))))))

    (return-from procura-retrocesso-mac-mrv-fapix (values nil 0)))

(defun fapix-forward-checking (f ipix)
  ""
  (let ((inferencias (make-hash-table :test 'eql)))

    (dolist (v2 (aref (fapix-relacionados f) ipix))
      (when (not (fapix-pix-cor f v2))
	(dolist (cor2 (gethash v2 inferencias (fapix-pix-dominio f v2)))
	  (multiple-value-bind (consistente r-falhou) (fapix-atribuicao-consistente-p f v2 cor2)
	    (when (not consistente)
	      (dolist (ipix3 (restricao-fapix-ipixs r-falhou))
		(when (not (fapix-pix-cor f ipix3))
		  (when (null (setf (gethash ipix3 inferencias)
				    (remove cor2
					    (gethash ipix3 inferencias (fapix-pix-dominio f ipix3)))))
		    (return-from fapix-forward-checking (values nil 0))))))))))

    (return-from fapix-forward-checking (values inferencias 0))))

(defun fapix-mac (f ipix)
  ""
  (let ((inferencias (make-hash-table :test 'eql))
	(lista-arcos (fapix-arcos-vizinhos-nao-atribuidos f ipix)))

    (dolist (arco lista-arcos)
      (let ((v2 (first arco))
	    (v1 (rest arco)))

        (dolist (cor2 (gethash v2 inferencias (fapix-pix-dominio f v2)))
	  (dolist (cor1 (gethash v1 inferencias (fapix-pix-dominio f v1)))
	    (let ((cor1-backup (fapix-pix-cor f v1)))
	      (fapix-adiciona-atribuicao! f v1 cor1)

	      (multiple-value-bind (consistente r-falhou) (fapix-atribuicao-consistente-p f v2 cor2)
		(when (not consistente)
		  (dolist (ipix3 (restricao-fapix-ipixs r-falhou))
		    (when (not (fapix-pix-cor f ipix3))
		      (when (null (setf (gethash ipix3 inferencias)
					(remove cor2 (gethash ipix3 inferencias (fapix-pix-dominio f ipix3)))))
			(fapix-adiciona-atribuicao! f v1 cor1-backup)
			(return-from fapix-mac (values nil 0)))))
		  (nconc lista-arcos (remove (cons v1 v2)
					     (fapix-arcos-vizinhos-nao-atribuidos f v2)
					     :test 'equal))))

	      (fapix-adiciona-atribuicao! f v1 cor1-backup))))))

    (return-from fapix-mac (values inferencias 0))))

(defun fapix-arcos-vizinhos-nao-atribuidos (f ipix)
  ""
  (loop for ipix-rel in (aref (fapix-relacionados f) ipix) unless (fapix-pix-cor f ipix-rel)
       collect (cons ipix-rel ipix)))
