;;;;; projeto.lisp

;;;;; 2.1.1 Tipo Restrição
;;;;; cria-restricao: lista de variáveis x predicado -> restrição

(defstruct (restricao (:constructor cria-restricao (variaveis funcao-validacao))) 
  "Tipo restricao caracterizado por uma lista das variaveis envolvidas na restricao, e uma funcao que verifica a restricao."
  variaveis
  funcao-validacao)

;;;;; restricao-variaveis: restrição -> lista de variáveis

;;;;; restricao-funcao-validacao: restrição -> predicado

;;;;; 2.1.2 Tipo PSR

;;;;; cria-psr: lista variáveis x lista de domínios x lista de restrições -> PSR
(defstruct (psr (:constructor cria-psr (variaveis-todas dominios restricoes))) 
  "Tipo PSR (Problema de Satisfacao de Restricoes)"
  variaveis-todas ; lista
  dominios ; lista de listas
  restricoes)

;;;;; psr-atribuicoes: PSR -> lista atribuições

(defun psr-atribuicoes (p)
  "Retorna uma lista com todas as atribuições - pares (variavel . valor) - do PSR."
  (remove nil (mapcar #'(lambda (x y) (if (= (length y) 1) (cons x y))) 
	      (psr-variaveis-todas p) (psr-dominios p))))
	 
;;;;; psr-variaveis-todas: PSR -> lista variáveis

;;;;; psr-variaveis-nao-atribuidas: PSR -> lista de variáveis

;;;;; psr-variavel-valor: PSR x variável -> objecto

;;;;; psr-variavel-dominio: PSR x variável -> domínio

;;;;; psr-variavel-restricoes: PSR x variável -> lista restrições

;;;;; psr-adiciona-atribuicao!: PSR x variável x valor -> {}

;;;;; psr-remove-atribuicao!: PSR x variável x valor -> {}

;;;;; psr-altera-dominio!: PSR x variável x domínio  {}

;;;;; psr-completo-p: PSR -> lógico

;;;;; psr-consistente-p: PSR -> lógico, inteiro

;;;;; psr-variavel-consistente-p: PSR x variável -> lógico, inteiro

;;;;; psr-atribuicao-consistente-p: PSR x variável x valor -> lógico, inteiro

;;;;; psr-atribuicoes-consistentes-arco-p: PSR x variável x valor x variável x valor -> lógico, inteiro
