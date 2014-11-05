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
  (mapcan #'(lambda (x y) (when (= (length y) 1) (cons x y))) 
	      (psr-variaveis-todas p) (psr-dominios p)))
	 
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
  (if (null (psr-variaveis-nao-atribuidas p)) T NIL))

;;; psr-consistente-p: PSR -> logico, inteiro
(defun psr-consistente-p (p v))

;;; psr-variavel-consistente-p: PSR x variavel -> logico, inteiro
(defun psr-variavel-consistente-p (p v))

;;; psr-atribuicao-consistente-p: PSR x variavel x valor -> logico, inteiro
(defun psr-atribuicao-consistente-p (p v n))

;;; psr-atribuicoes-consistentes-arco-p: PSR x variavel x valor x variavel x valor -> logico, inteiro
(defun psr-atribuicoes-consistentes-arco-p (p v1 n1 v2 n2))
