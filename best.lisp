;;;;; Grupo 30 Taguspark - 77213 Rui Silva, 82134 Jorge Almeida

(load "exemplos.fas")

(defstruct restricao-fapix
  ""
  ipixs
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
  solucionavel
  dominio-len
  pix
  hv)


(defun cria-restricao-fapix (ipixs pix a-volta-len)
  ""
  (make-restricao-fapix :ipixs ipixs
			:funcao-validacao #'(lambda (f) (multiple-value-bind (brancos pretos)
							    (fapix-conta-cores f ipixs)
							  (and (<= pretos pix)
							       (<= brancos (- a-volta-len pix)))))))

(defun fapix-ipixs-nao-atribuidos (f)
  "Devolve lista de ipixs nao atribuidos."
  (fapix-natribuidos f))

(defun fapix-pix-cor (f ipix)
  "Devolve a cor (atribuicao) de um pix ou nil caso nao tenha atribuicao."
  (aref (fapix-atribuicao f) ipix))

(defun fapix-pix-dominio (f ipix)
  "Devolve o dominio associado a um pix."
  (let ((cor (fapix-pix-cor f ipix)))
    (if cor
	(list cor)
	(aref (fapix-dominio f) ipix))))

(defun fapix-pix-restricoes (f ipix)
  "Devolve uma lista com todas as restricoes aplicaveis a uma variavel."
  (aref (fapix-restricoes f) ipix))

(defun fapix-adiciona-atribuicao! (f ipix cor)
  ""
  (setf (aref (fapix-atribuicao f) ipix) cor))

(defun fapix-remove-atribuicao! (f ipix)
  ""
  (fapix-adiciona-atribuicao! f ipix nil))

(defun fapix-altera-dominio! (f ipix d)
  ""
  (setf (aref (fapix-dominio-len f) (length (fapix-pix-dominio f ipix)))
	(remove ipix (aref (fapix-dominio-len f) (length (fapix-pix-dominio f ipix)))))
  (setf (aref (fapix-dominio f) ipix) d)
  (push ipix (aref (fapix-dominio-len f) (length d))))

(defun fapix-completo-p (f)
  ""
  (null (fapix-ipixs-nao-atribuidos f)))

(defun fapix-testa-restricoes (f restricoes)
  "Testa uma lista de restricoes e devolve dois valores. O primeiro, um boleano que indica se todas
  as restricoes se verificam e o segundo indica o numero de testes realizados."
  (dolist (r restricoes (values t nil))
    (when (null (funcall (restricao-fapix-funcao-validacao r) f))
      (return (values nil r)))))

(defun fapix-consistente-p (f)
  "Testa todas as restricoes do fapix."
  (fapix-testa-restricoes f (fapix-restricoes f)))

(defun fapix-pix-consistente-p (f ipix)
  "Testa as restricoes associadas ao 'ipix'."
  (fapix-testa-restricoes f (fapix-pix-restricoes f ipix)))

(defun fapix-atribuicao-consistente-p (f ipix cor)
  ""
  (let ((atribuicao (fapix-pix-cor f ipix))
	(consistente t)
	(falhou nil))

    (fapix-adiciona-atribuicao! f ipix cor)

    (setf (values consistente falhou) (fapix-pix-consistente-p f ipix))

    (if atribuicao (fapix-adiciona-atribuicao! f ipix atribuicao) (fapix-remove-atribuicao! f ipix))
    (values consistente falhou)))

(defun fapix-atribuicoes-consistentes-p (f ipix1 cor1 ipix2 cor2)
  ""
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
  ""
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

	      (when (and (not (= numero 0)) (>= (round (round soma numero) a-volta-len) 1))
;		(setf (aref pix-hv ipix) 1)
		))


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
  ""
  (let* ((colunas (fapix-colunas f))
	 (tab (make-array (list (fapix-linhas f) colunas))))
    (dotimes (ipix (fapix-max-ipix f) tab)
      (setf (aref tab (linha ipix colunas) (coluna ipix colunas)) (fapix-pix-cor f ipix)))))

(defun array->fill-a-pix (a linhas colunas)
  (let ((tab (make-array (list linhas colunas))))
    (dotimes (ipix (* linhas colunas) tab)
      (setf (aref tab (linha ipix colunas) (coluna ipix colunas)) (aref a ipix)))))

(defun algoritmo (f)
  ""
  (when (not (fapix-solucionavel f)) (return-from algoritmo nil))
  
  (procura-retrocesso-mac-mrv-fapix f))

(defun resolve-best (tab)
  ""
  (fapix->fill-a-pix (algoritmo (fill-a-pix->fapix tab))))

(defun fapix-mrv (f)
  ""
  (dolist (ipix (aref (fapix-dominio-len f) 1))
    (return-from fapix-mrv ipix))

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
;  (format t "pr-fc-mrv~%")
  (when (fapix-completo-p f)
    (return-from procura-retrocesso-fc-mrv-fapix (values f 0)))

  (let* ((ipix (fapix-mrv f))
	 (d (fapix-pix-dominio f ipix)))

    (when (= (length d) 2)
      (when (= (aref (fapix-hv f) ipix) 1)
	(setf d (reverse d))))

;    (format t "Vou experimentar a variavel:~a cujo dominio e:~a~%" ipix (fapix-pix-dominio f ipix))
    (dolist (cor d)
;      (format t "Atribuindo o valor:~a a variavel:~a~%" cor ipix)
      (fapix-adiciona-atribuicao! f ipix cor)
      (setf (fapix-natribuidos f) (remove ipix (fapix-natribuidos f)))
;      (format t "natribuidos:~a~%" (fapix-natribuidos f))
      (setf (aref (fapix-dominio-len f) (length d))
	    (remove ipix (aref (fapix-dominio-len f) (length d))))
;      (format t "dominio-len:~a~%" (fapix-dominio-len f))
;      (format t "Depois da atribuicao~%")
;      (desenha-fill-a-pix (fapix->fill-a-pix f))
      (let ((inferencias (fapix-forward-checking f ipix)))
;	(format t "PR: inferencias:~a~%" inferencias)
	(when inferencias
	  (let ((backup (make-hash-table :test 'eql)))
	    (maphash #'(lambda (iipix id)
;			 (format t "iipix:~a id:~a~%" iipix id)
			 (setf (gethash iipix backup) (fapix-pix-dominio f iipix))
			 (fapix-altera-dominio! f iipix id))
		     inferencias)

;	    (format t "Depois das inferencias~%")
;	    (desenha-fill-a-pix (fapix->fill-a-pix f))

	    (multiple-value-bind (resultado) (procura-retrocesso-fc-mrv-fapix f)
	      (when resultado
		(return-from procura-retrocesso-fc-mrv-fapix (values resultado 0))))

	    (maphash #'(lambda (bipix bd)
			 (fapix-altera-dominio! f bipix bd))
		     backup)

;	    (format t "Removidas as inferencias:~%")
;	    (desenha-fill-a-pix (fapix->fill-a-pix f))
	    ))

	(fapix-remove-atribuicao! f ipix)
	(push ipix (fapix-natribuidos f))
	(push ipix (aref (fapix-dominio-len f) (length d)))

;	(format t "Removendo a atribuicao da variavel:~a:~%" ipix)
;	(desenha-fill-a-pix (fapix->fill-a-pix f))
	)))

  (return-from procura-retrocesso-fc-mrv-fapix (values nil 0)))

(defun procura-retrocesso-mac-mrv-fapix (f)
  ""
;  (format t "pr-mac-mrv~%")
  (when (fapix-completo-p f)
    (return-from procura-retrocesso-mac-mrv-fapix (values f 0)))

  (let* ((ipix (fapix-mrv f))
	 (d (fapix-pix-dominio f ipix)))
;    (format t "Vou experimentar a variavel:~a cujo dominio e:~a~%" ipix (fapix-pix-dominio f ipix))
      ; TODO: heuristica de valor (media dos pixs a volta? implica mais um array)
    (when (= (length d) 2)
      (when (= (aref (fapix-hv f) ipix) 1)
	(setf d (reverse d))))

    (dolist (cor d)
;      (format t "Atribuindo o valor:~a a variavel:~a~%" cor ipix)
      (fapix-adiciona-atribuicao! f ipix cor)
      (setf (fapix-natribuidos f) (remove ipix (fapix-natribuidos f)))
;	    (format t "natribuidos:~a~%" (fapix-natribuidos f))
      (setf (aref (fapix-dominio-len f) (length d))
	    (remove ipix (aref (fapix-dominio-len f) (length d))))
;	    (format t "dominio-len:~a~%" (fapix-dominio-len f))
;	    (format t "Depois da atribuicao~%")
;      (desenha-fill-a-pix (fapix->fill-a-pix f))
      (let ((inferencias (fapix-mac f ipix)))
;	(format t "PR: inferencias:~a~%" inferencias)

	(when inferencias
	  (let ((backup (make-hash-table :test 'eql)))
	    (maphash #'(lambda (iipix id)
;			 (format t "iipix:~a id:~a~%" iipix id)
			 (setf (gethash iipix backup) (fapix-pix-dominio f iipix))
			 (fapix-altera-dominio! f iipix id))
		     inferencias)

;		  (format t "Depois das inferencias~%")
;		  (desenha-fill-a-pix (fapix->fill-a-pix f))

	    (multiple-value-bind (resultado) (procura-retrocesso-mac-mrv-fapix f)
	      (when resultado
		(return-from procura-retrocesso-mac-mrv-fapix (values resultado 0))))

	    (maphash #'(lambda (bipix bd)
			 (fapix-altera-dominio! f bipix bd))
		     backup)

;		  (print "Removidas as inferencias:")
;		  (desenha-fill-a-pix (fapix->fill-a-pix f))
	    ))

	(fapix-remove-atribuicao! f ipix)
	(push ipix (fapix-natribuidos f))
	(push ipix (aref (fapix-dominio-len f) (length d)))

;	(print "Removendo a atribuicao:")
;	(desenha-fill-a-pix (fapix->fill-a-pix f))
	)))

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
