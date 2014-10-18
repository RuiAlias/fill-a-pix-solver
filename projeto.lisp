;;;; projeto.lisp

(defstruct (restricao (:constructor cria-restricao (variaveis funcao-validacao))) 
  "Tipo restricao caracterizado por uma lista das variaveis envolvidas na restricao, e uma funcao que verifica a restricao."
  variaveis
  funcao-validacao)

(defstruct (psr (:constructor cria-psr (variaveis-todas dominios restricoes))) 
  "Tipo PSR (Problema de Satisfacao de Restricoes)"
  variaveis-todas ; lista
  dominios ; lista de listas
  restricoes)

(defun psr-atribuicoes (p)
  "Retorna uma lista com todas as atribuições - pares (variavel . valor) - do PSR."
  (remove nil (mapcar #'(lambda (x y) (if (= (length y) 1) (cons x y))) 
	      (psr-variaveis-todas p) (psr-dominios p))))

