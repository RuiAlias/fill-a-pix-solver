;;;;; Grupo 30 Taguspark - 77213 Rui Silva, 82134 Jorge Almeida

(load "exemplos.fas")

(defstruct restricao-fapix
  ""
  variaveis
  funcao-validacao)

(defstruct fapix
  ""
  linhas
  colunas
  max-ipix
  dominio
  atribuicao
  restricoes
  a-volta
  relacionados
  natribuidos
  solucionavel)

(defun cria-restricao-fapix (variaveis pix)
  ""
  (make-restricao-fapix :variaveis variaveis :funcao-validacao #'(lambda () pix)))

(defun linha (ipix colunas)
  "Devolve a linha em que se encontra o pix correspondente ao indice fornecido."
  (nth-value 0 (floor ipix colunas)))

(defun coluna (ipix colunas)
  "Devolve a coluna em que se encontra o pix correspondente ao indice fornecido."
  (nth-value 1 (floor ipix colunas)))

(defun pix-a-volta (ipix colunas max-ipix)
  "Devolve uma lista com os indices dos pix a volta do pix cujo o indice fornecido."
  (loop for l from (- ipix colunas) to (+ ipix colunas) by colunas when (<= 0 l max-ipix)
    append (loop for c from (- l 1) to (+ l 1) when (= (linha c colunas) (linha l colunas))
      collect c)))

(defun fill-a-pix->fapix (tab) ; tab de tabuleiro
  ""
  (let* ((linhas (array-dimension tab 0))
	 (colunas (array-dimension tab 1))
	 (max-ipix (* linhas colunas))
	 (pix-dominio (make-array max-ipix :initial-element (list 0 1)))
	 (pix-atribuicoes (make-array max-ipix :initial-element nil))
	 (pix-restricoes (make-array max-ipix :initial-element nil)) ; ipix->lista de restricoes
	 (pix-a-volta (make-array max-ipix :initial-element nil))
	 (pix-relacionados (make-array max-ipix :initial-element nil)) ; lista de ipix relacionados
					                               ; por restricao
	 (pix-natribuidos (list))
	 (pix-solucionavel t) ; t de talvez :D
	 (ipix 0))
    
    (block ciclos
      (dotimes (l linhas)
	(dotimes (c colunas)
	  (let* ((pix (aref tab l c))
		 (a-volta (pix-a-volta ipix colunas max-ipix))
		 (a-volta-len (length a-volta)))
	    
	    (setf (aref pix-a-volta ipix) a-volta)
	    
	    (when (numberp pix)
	      (when (> pix a-volta-len) (setf pix-solucionavel nil) (return-from ciclos))
	    
	      (let* ((restricao (cria-restricao-fapix a-volta pix))
		     (dominio (if (= pix 0) (list 0) (if (= pix 9) (list 1) nil))))
		
		(dolist (ipix-av a-volta)
		  (setf (aref pix-restricoes ipix-av) restricao)
		  (setf (aref pix-relacionados ipix-av) (remove ipix-av a-volta :test '=))
		  
		  (when dominio
		    (setf (aref pix-dominio ipix-av) dominio))))) ; TODO: so dominio? ou tmb atrib?
	    
	    (push ipix pix-natribuidos)
	    (incf ipix)))))
    
    (make-fapix :linhas linhas
		:colunas colunas
		:max-ipix max-ipix
		:dominio pix-dominio
		:atribuicao pix-atribuicoes
		:restricoes pix-restricoes
		:a-volta pix-a-volta
		:relacionados pix-relacionados
		:natribuidos pix-natribuidos
		:solucionavel pix-solucionavel)))

(defun algoritmo (f)
  ""
  (when (not (fapix-solucionavel f)) (return-from algoritmo nil))
  
  (print "TODO"))

(defun resolve-best (tab)
  ""
  (algoritmo (fill-a-pix->fapix tab)))
