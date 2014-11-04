;;;;; projeto.lisp

;;;;; 2.1.1 Tipo Restricao
;;;;; cria-restricao: lista de variaveis x predicado -> restricao

(defstruct (restricao (:constructor cria-restricao (variaveis funcao-validacao))) 
  "Tipo restricao caracterizado por uma lista das variaveis envolvidas na restricao, e uma funcao que verifica a restricao."
  variaveis ; lista de variaveis
  funcao-validacao) ; predicado

;;;;; restricao-variaveis: restricao -> lista de variaveis

;;;;; restricao-funcao-validacao: restricao -> predicado

;;;;; 2.1.2 Tipo PSR

;;;;; cria-psr: lista variaveis x lista de dominios x lista de restricoes -> PSR
(defstruct (psr (:constructor cria-psr (variaveis-todas dominios restricoes))) 
  "Tipo PSR (Problema de Satisfacao de Restricoes)"
  variaveis-todas ; lista de strings
  dominios ; lista de listas
  restricoes)

;;;;; psr-atribuicoes: PSR -> lista atribuicoes

(defun psr-atribuicoes (p)
  "Retorna uma lista com todas as atribuicoes - pares (variavel . valor) - do PSR."
  (remove nil (mapcar #'(lambda (x y) (when (= (length y) 1) (cons x y))) 
	      (psr-variaveis-todas p) (psr-dominios p))))
	 
;;;;; psr-variaveis-todas: PSR -> lista variaveis

;;;;; psr-variaveis-nao-atribuidas: PSR -> lista de variaveis

(defun psr-variaveis-nao-atribuidas (p)
  "Devolve lista de variaveis nao atribuidas (pela ordem inicial)."
  (mapcan #'(lambda (v d) (when (< (length d) 2) (list v))) 
	  (psr-variaveis-todas p) (psr-restricoes p)))

;;;;; psr-variavel-valor: PSR x variavel -> objecto

(defun psr-variavel-valor (p v)
  "Devolve o valor atribuido a variavel (caso nao exista atribuicao devolve nil)."
  (rest (first (member v (psr-atribuicoes p) :test #'equal :key #'first))))

;;;;; psr-variavel-dominio: PSR x variavel -> dominio

;;;;; psr-variavel-restricoes: PSR x variavel -> lista restricoes

;;;;; psr-adiciona-atribuicao!: PSR x variavel x valor -> {}

;;;;; psr-remove-atribuicao!: PSR x variavel x valor -> {}

;;;;; psr-altera-dominio!: PSR x variavel x domínio {}

;;;;; psr-completo-p: PSR -> logico

;;;;; psr-consistente-p: PSR -> logico, inteiro

;;;;; psr-variavel-consistente-p: PSR x variavel -> logico, inteiro

;;;;; psr-atribuicao-consistente-p: PSR x variavel x valor -> logico, inteiro

;;;;; psr-atribuicoes-consistentes-arco-p: PSR x variavel x valor x variavel x valor -> logico, inteiro
