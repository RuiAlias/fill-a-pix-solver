;;;;; projeto.lisp

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


;;; psr-consistente-p: PSR -> logico, inteiro
(defun psr-consistente-p (p)
  ""
  (let ((testes 0)) 
    (dolist (r (psr-restricoes p) (values t testes)) 
      (incf testes) 
      (when (null (funcall (restricao-funcao-validacao r) p)) (return (values nil testes))))))


;;; psr-variavel-consistente-p: PSR x variavel -> logico, inteiro
(defun psr-variavel-consistente-p (p v)
  ""
  (let ((testes 0)) 
    (dolist (r (psr-variavel-restricoes p v) (values t testes)) 
      (incf testes) 
      (when (null (funcall (restricao-funcao-validacao r) p)) (return (values nil testes))))))


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
  (let ((consistente1 t) (testes1 0) (consistente2 t) (testes2 0))
    (setf (values consistente1 testes1) (psr-atribuicao-consistente-p p v1 valor1))
    (when consistente1 
      (setf (values consistente2 testes2) (psr-atribuicao-consistente-p p v2 valor2)))
    (values (and consistente1 consistente2) (+ testes1 testes2))))


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
    variaveis)) ; a lista retornada vem na ordem oposta ao esperado. Pode-se corrigir com `nreverse'


;;; fill-a-pix->psr: array -> PSR
(defun fill-a-pix->psr (tab) ; tab de tabuleiro
  ""
  (let ((variaveis (list)) (dominios (list)) (restricoes (list)))
    (dotimes (l (array-dimension tab 0))   ; linha
      (dotimes (c (array-dimension tab 1)) ; coluna
	(push (write-to-string (cons l c)) variaveis)
	(push (list 0 1) dominios)
	(when (numberp (aref tab l c))
	  (push (cria-restricao 
		 (variaveis-a-volta l c (array-dimension tab 0) (array-dimension tab 1))
		 #'(lambda (x) t))
		restricoes))))
    (cria-psr (nreverse variaveis) (nreverse dominios) restricoes)))


;;; psr->fill-a-pix: PSR x inteiro x inteiro -> array
(defun psr->fill-a-pix (psr linhas colunas)
  (make-array (list linhas colunas) :initial-contents ' "para cada variavel no psr receber o dominio")

;;;; 2.2.2

;;; procura-retrocesso-simples: PSR -> PSR, inteiro

;;; resolve-simples: array -> array



