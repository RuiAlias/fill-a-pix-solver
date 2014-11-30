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
  solucionavel)


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
	cor
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
  (setf (aref (fapix-dominio f) ipix) d))

(defun fapix-completo-p (f)
  ""
  (null (fapix-ipixs-nao-atribuidos f)))

(defun fapix-testa-restricoes (f restricoes)
  "Testa uma lista de restricoes e devolve dois valores. O primeiro, um boleano que indica se todas
  as restricoes se verificam e o segundo indica o numero de testes realizados."
  (let ((testes 0))
    (dolist (r restricoes (values t testes))
      (incf testes)
      (when (null (funcall (restricao-fapix-funcao-validacao r) f)) (return (values nil testes))))))

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
	(testes 0))

    (fapix-adiciona-atribuicao! f ipix cor)

    (setf (values consistente testes) (fapix-pix-consistente-p f ipix))

    (if atribuicao (fapix-adiciona-atribuicao! f ipix atribuicao) (fapix-remove-atribuicao! f ipix))
    (values consistente testes)))

(defun fapix-atribuicoes-consistentes-arco-p (f ipix1 cor1 ipix2 cor2)
  ""
  (let ((atribuicao1 (fapix-pix-cor f ipix1))
	(atribuicao2 (fapix-pix-cor f ipix2))
	(consistente t)
	(testes 0))

    (fapix-adiciona-atribuicao! f ipix1 cor1)
    (fapix-adiciona-atribuicao! f ipix2 cor2)
    (dolist (r (fapix-pix-restricoes f ipix1))
      (when (find ipix2 (restricao-fapix-ipixs r) :test '=)
	(incf testes)
	(when (null (funcall (restricao-fapix-funcao-validacao r) f))
	  (setf consistente nil)
	  (return))))

    (if atribuicao1 (fapix-adiciona-atribuicao! f ipix1 atribuicao1) (fapix-remove-atribuicao! f ipix1))
    (if atribuicao2 (fapix-adiciona-atribuicao! f ipix2 atribuicao2) (fapix-remove-atribuicao! f ipix2))
    (values consistente testes)))

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
;    (format t "Declarei tudo~%")
    (block ciclos
      (dotimes (l linhas)
	(dotimes (c colunas)
	  (let* ((pix (aref tab l c))
		 (a-volta (pix-a-volta ipix colunas max-ipix))
		 (a-volta-len (length a-volta)))

	    (setf (aref pix-a-volta ipix) a-volta)
	    
	    (when (numberp pix)
	      (when (> pix a-volta-len) (setf pix-solucionavel nil) (return-from ciclos))
	    
	      (let* ((restricao (cria-restricao-fapix a-volta pix a-volta-len))
		     (dominio (if (= pix 0) (list 0) (if (= pix 9) (list 1) nil))))
		
		(dolist (ipix-av a-volta)
		  (push restricao (aref pix-restricoes ipix-av))

		  (setf (aref pix-relacionados ipix-av) (merge 'list
							       (aref pix-relacionados ipix-av)
							       (remove ipix-av a-volta :test '=)
							       #'<))

		  (when dominio
		    (setf (aref pix-dominio ipix-av) dominio))))) ; TODO: so dominio? ou tmb atrib?
	    
	    (push ipix pix-natribuidos)
	    (incf ipix)))))

    (dotimes (i max-ipix)
      (nreverse (aref pix-restricoes i))
      (delete-duplicates (aref pix-relacionados i)))
    (nreverse pix-natribuidos)

;    (format t "Percorri o tab todo~%")
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

(defun fapix->fill-a-pix (f)
  ""
  (let* ((colunas (fapix-colunas f))
	 (tab (make-array (list (fapix-linhas f) colunas))))
    (dotimes (ipix (fapix-max-ipix f) tab)
      (setf (aref tab (linha ipix colunas) (coluna ipix colunas)) (fapix-pix-cor f ipix)))))

(defun algoritmo (f)
  ""
  (when (not (fapix-solucionavel f)) (return-from algoritmo nil))
  
  (procura-retrocesso-fc-mrv-fapix f))

(defun resolve-best (tab)
  ""
  (fapix->fill-a-pix (algoritmo (fill-a-pix->fapix tab))))

(defun fapix-mrv (f) ; TODO: rever
  ""
  (let* ((lista-vna (fapix-ipixs-nao-atribuidos f))
	 (min-v (first lista-vna))
	 (min-length (length (fapix-pix-dominio f min-v))))

    (dolist (v lista-vna)
      (let ((d-length (length (fapix-pix-dominio f v))))
	(when (< d-length min-length)
	  (setf min-length d-length)
	  (setf min-v v))))

    min-v))

(defun procura-retrocesso-fc-mrv-fapix (f)
  ""
  (let ((testes-totais 0))
    (when (fapix-completo-p f) (return-from procura-retrocesso-fc-mrv-fapix (values f testes-totais)))

    (let* ((ipix (fapix-mrv f))
;	   (d (psr-variavel-dominio p v))
	   )
;      (format t "Vou experimentar a variavel:~a cujo dominio e:~a~%" ipix (fapix-pix-dominio f ipix))
      (dolist (cor (fapix-pix-dominio f ipix))
	(multiple-value-bind (consistente testes) (fapix-atribuicao-consistente-p f ipix cor)
	  (incf testes-totais testes)
	  (when consistente
;	    (format t "Atribuindo o valor:~a a variavel:~a~%" cor ipix)
	    (fapix-adiciona-atribuicao! f ipix cor)
	    (setf (fapix-natribuidos f) (remove ipix (fapix-natribuidos f)))
;	    (format t "Depois da atribuicao~%")
;	    (desenha-fill-a-pix (fapix->fill-a-pix f))
	    (multiple-value-bind (inferencias testes) (fapix-forward-checking f ipix)
;	      (format t "PR: inferencias:~a~%" inferencias)
	      (incf testes-totais testes)
	      (when inferencias ; teste suficiente?
		(let ((backup (make-hash-table :test 'eql)))
		  (maphash #'(lambda (iipix id)
;			       (when DEBUG (format t "iv:~a id:~a~%" iv id))
			       (setf (gethash iipix backup) (fapix-pix-dominio f iipix))
			       (fapix-altera-dominio! f iipix id))
			   inferencias)

;		  (format t "Depois das inferencias~%")
;		    (desenha-fill-a-pix (psr->fill-a-pix p LC LC))

		  (multiple-value-bind (resultado testes) (procura-retrocesso-fc-mrv-fapix f)
		    (incf testes-totais testes)
		    (when resultado
		      (return-from procura-retrocesso-fc-mrv-fapix (values resultado testes-totais))))

		  (maphash #'(lambda (bipix bd)
			       (fapix-altera-dominio! f bipix bd))
			   backup)

;		  (print "Removidas as inferencias:")
;		    (desenha-fill-a-pix (fapix->fill-a-pix f LC LC))
		  ))

	      (fapix-remove-atribuicao! f ipix)
	      (push ipix (fapix-natribuidos f))
;	      (when DEBUG
;		(print "Removendo a atribuicao:")
;		(desenha-fill-a-pix (fapix->fill-a-pix f LC LC)))
	      )))))

    (return-from procura-retrocesso-fc-mrv-fapix (values nil testes-totais))))


(defun fapix-forward-checking (f ipix)
  ""
  (let ((testes-totais 0)
	(inferencias (make-hash-table :test 'eql))
	(lista-arcos (fapix-arcos-vizinhos-nao-atribuidos f ipix)))
;    (format t "FC arcos:~a~%" lista-arcos)
    (dolist (arco lista-arcos)
      (let ((v2 (first arco))
	    (v1 (rest arco)))

	(multiple-value-bind (revise testes) (fapix-revise f v2 v1 inferencias)
	  (incf testes-totais testes)
	  (when revise
	    (multiple-value-bind (d exists) (gethash v2 inferencias)
	      (when exists
		(when (= (length d) 0)
		  (return-from fapix-forward-checking (values nil testes-totais)))))))))

    (return-from fapix-forward-checking (values inferencias testes-totais))))

(defun fapix-arcos-vizinhos-nao-atribuidos (f ipix)
  ""
  (let ((lista-arcos (list)))
    (dolist (relacionado (aref (fapix-relacionados f) ipix))
      (when (not (fapix-pix-cor f relacionado))
	(push (cons relacionado ipix) lista-arcos)))

    (nreverse lista-arcos)))


;; (defun psr-revise (p x y inferencias)
;;   ""
;;   (let* ((testes-total 0)
;; 	 (revised nil)
;; 	 (dominio-x (gethash x inferencias (psr-variavel-dominio p x)))
;; 	 (novo-dominio-x dominio-x)
;; 	 (dominio-y (if (psr-variavel-atribuida-p p y)
;; 			(psr-variavel-dominio p y)
;; 			(gethash y inferencias (psr-variavel-dominio p y)))))

;;     (dolist (valor-x dominio-x)
;;       (when (dolist (valor-y dominio-y t)
;; 	      (multiple-value-bind (consistente testes)
;; 		  (psr-atribuicoes-consistentes-arco-p p x valor-x y valor-y)
;; 		(incf testes-total testes)
;; 		(when consistente (return nil))))
;; 	(setf revised t)
;; 	(setf novo-dominio-x (remove valor-x novo-dominio-x))))

;;     (when revised
;;       (setf (gethash x inferencias) novo-dominio-x)) ; TODO: sobrepor?

;;     (values revised testes-total)))


(defun fapix-revise (f x y inferencias)
  ""
  (let* ((testes-totais 0)
	 (revised nil)
	 (dominio-x (gethash x inferencias (fapix-pix-dominio f x)))
	 (novo-dominio-x dominio-x)
	 (dominio-y (if (fapix-pix-cor f y)
			(list (fapix-pix-cor f y))
			(gethash y inferencias (fapix-pix-dominio f y)))))

    (dolist (valor-x dominio-x)
      (let ((found-consistent-value nil))
	(dolist (valor-y dominio-y)
	  (multiple-value-bind (consistente testes) (fapix-atribuicoes-consistentes-arco-p f x valor-x y valor-y)
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
