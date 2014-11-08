;;;;; Grupo 30 Taguspark - 77213 Rui Silva, 82134 Jorge Almeida

(load (compile-file “exemplos.lisp”))

;;;; 2.1.1 Tipo Restricao
;;; cria-restricao: lista de variaveis x predicado -> restricao

(defstruct (restricao (:constructor cria-restricao (variaveis funcao-validacao))) 
  "Tipo restricao caracterizado por uma lista das variaveis envolvidas na restricao, e uma funcao que verifica a restricao."
  variaveis ; lista de variaveis
  funcao-validacao) ; predicado

;;; restricao-variaveis: restricao -> lista de variaveis

;;; restricao-funcao-validacao: restricao -> predicado

;;;; 2.1.2 Tipo PSR

;;; cria-psr: lista variaveis x lista de dominios x lista de restricoes -> PSR
(defstruct (psr (:constructor cria-psr (variaveis-todas dominios restricoes))) 
  "Tipo PSR (Problema de Satisfacao de Restricoes)"
  variaveis-todas  ; lista (tamanho N) de strings
  dominios         ; lista (tamanho N) de listas
  restricoes)      ; lista de restricoes

;;; psr-atribuicoes: PSR -> lista atribuicoes

(defun psr-atribuicoes (p)
  "Retorna uma lista com todas as atribuicoes - pares (variavel . valor) - do PSR."
  (remove nil (mapcar #'(lambda (v d) (when (= (length d) 1) (cons v (first d)))) 
		      (psr-variaveis-todas p) (psr-dominios p))))
	 
;;; psr-variaveis-todas: PSR -> lista variaveis

;;; psr-variaveis-nao-atribuidas: PSR -> lista de variaveis

(defun psr-variaveis-nao-atribuidas (p)
  "Devolve lista de variaveis nao atribuidas (pela ordem inicial)."
 (mapcan #'(lambda (v d) (when (not(= (length d) 1)) (list v))) 
	      (psr-variaveis-todas p) (psr-dominios p)))

;;; psr-variavel-valor: PSR x variavel -> objecto

(defun psr-variavel-valor (p v)
  "Devolve o valor atribuido a variavel (caso nao exista atribuicao devolve nil)."
  (rest (first (member v (psr-atribuicoes p) :test #'equal :key #'first))))

;;; psr-variavel-dominio: PSR x variavel -> dominio

(defun psr-variavel-dominio (p v)
  "Devolve o dominio associado a uma variavel."
  (rest (first (member v 
		       (mapcar #'cons (psr-variaveis-todas p) (psr-dominios p)) 
		       :test #'equal 
		       :key #'first))))

;;; psr-variavel-restricoes: PSR x variavel -> lista restricoes

(defun psr-variavel-restricoes (p v)
  "Devolve uma lista com todas as restricoes aplicaveis a uma variavel."
  (remove-if-not #'(lambda (l) (member v l :test #'equal)) (psr-restricoes p) :key #'restricao-variaveis)
  
  ; (remove "a" (list r1 r2) :key #'restricao-variaveis :test-not #'(lambda(r l) (member r l :test #'equal)))
)


(defun psr-restricoes-ambas-variaveis (p v1 v2)
  "Devolve uma lista com todas as restricoes aplicaveis a ambas as variaveis."
  (remove-if-not #'(lambda (l) (member v2 l :test #'equal))
		 (psr-variavel-restricoes p v1)
		 :key #'restricao-variaveis)
)

;;; psr-adiciona-atribuicao!: PSR x variavel x valor -> {}
(defun psr-adiciona-atribuicao! (p v n)
  ""
  (setf (nth (position v (psr-variaveis-todas p) :test #'equal) (psr-dominios p)) (list n)))


;;; psr-remove-atribuicao!: PSR x variavel -> {}
(defun psr-remove-atribuicao! (p v)
  ""
  (setf (nth (position v (psr-variaveis-todas p) :test #'equal) (psr-dominios p)) NIL))


;;; psr-altera-dominio!: PSR x variavel x dominio {}
(defun psr-altera-dominio! (p v d)
  ""
  (setf (nth (position v (psr-variaveis-todas p) :test #'equal) (psr-dominios p)) d))


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
  ""
  (psr-testa-restricoes p (psr-restricoes p)))


;;; psr-variavel-consistente-p: PSR x variavel -> logico, inteiro
(defun psr-variavel-consistente-p (p v)
  ""
  (psr-testa-restricoes p (psr-variavel-restricoes p v)))


;;; psr-atribuicao-consistente-p: PSR x variavel x valor -> logico, inteiro
(defun psr-atribuicao-consistente-p (p v valor)
  ""
  (let ((antigo-dominio (psr-variavel-dominio p v)) (consistente t) (testes 0))
    (psr-adiciona-atribuicao! p v valor)
    (setf (values consistente testes) (psr-variavel-consistente-p p v))
    (psr-altera-dominio! p v antigo-dominio)
    (values consistente testes)))


;;; psr-atribuicoes-consistentes-arco-p: PSR x variavel x valor x variavel x valor -> logico, inteiro
(defun psr-atribuicoes-consistentes-arco-p (p v1 valor1 v2 valor2)
  ""
  (let ((antigo-dominio1 (psr-variavel-dominio p v1))
	(antigo-dominio2 (psr-variavel-dominio p v2))
	(consistente t)
	(testes 0))
    (psr-adiciona-atribuicao! p v1 valor1)
    (psr-adiciona-atribuicao! p v2 valor2)
    (setf (values consistente testes) 
	  (psr-testa-restricoes p (psr-restricoes-ambas-variaveis p v1 v2)))
    (psr-altera-dominio! p v1 antigo-dominio1)
    (psr-altera-dominio! p v2 antigo-dominio2)
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
	      (if (= numero 0)
		  #'(lambda (p) (not (find t (mapcar #'(lambda (v)
						    (let ((d (psr-variavel-dominio p v)))
						      (= (first d) 1)))
						vav))))
		  #'(lambda (p) (<= numero
				    (count t (mapcar #'(lambda (v)
							 (let ((d (psr-variavel-dominio p v)))
							   (not (and (= (length d) 1)
								     (= (first d) 0)))))
						     vav))))))
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
  (make-array (list linhas colunas) 
	      :initial-contents (lista->lista2d (psr-dominios p) ; nao parece mto eficiente
					       linhas colunas)))

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
		(psr-altera-dominio! p v d))))
	  (return-from procura-retrocesso-simples (values nil testes-total))))))


;;; resolve-simples: array -> array
(defun resolve-simples (tab)
  ""
  (multiple-value-bind (p) (procura-retrocesso-simples (fill-a-pix->psr tab))
    (if (null p) nil (psr->fill-a-pix p (array-dimension tab 0) (array-dimension tab 1)))))
