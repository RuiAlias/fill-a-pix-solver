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
;	  (format t "v:~a d:~a~%" v d)
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


(defparameter LC 5)
(defparameter DEBUG t)


(defun n-restricoes-c-natribuidas (p v)
  ""
  (count-if #'(lambda (r) (some #'(lambda (v1) (and (not (equal v1 v))
						    (null (psr-variavel-atribuida-p p v1))))
				(restricao-variaveis r)))
	    (psr-variavel-restricoes p v)))


(defun psr-var-maior-grau (p)
  ""
;  (dolist (v (sort (psr-variaveis-nao-atribuidas p) #'> :key #'(lambda (v) (n-restricoes-c-natribuidas p v))))
;    (format DEBUG "GRAU: v:~a grau:~a~%" v (n-restricoes-c-natribuidas p v)))
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

;	  (when DEBUG (format t "Escolhi a variavel:~a cujo dominio e:~a~%" v (psr-variavel-dominio p v)))
	  (dolist (valor d)
	    (multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	      (incf testes-total testes)
;	      (when DEBUG (format t "Fiz ~a testes de consistencia. O total vai em:~a~%" testes testes-total))
	      (when consistente
;		(when DEBUG (format t "Atribuindo o valor:~a a variavel:~a~%" valor v))
		(psr-adiciona-atribuicao! p v valor)

;		(when DEBUG
;		  (format DEBUG "Depois da atribuicao~%")
;		  (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))

		(multiple-value-bind (recurs-consistente recurs-testes) (procura-retrocesso-grau p)
		  (incf testes-total recurs-testes)
;		  (when DEBUG
;		    (format t "Somando os ~a desta iteracao com os ~a que vem de tras fica:~a~%" testes recurs-testes testes-total))
		  (when (not (null recurs-consistente))
		    (return-from procura-retrocesso-grau (values p testes-total))))

		(psr-remove-atribuicao! p v)
;		(when DEBUG
;		  (print "Removendo a atribuicao:")
;		  (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
		)))
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

    (let* ((v (psr-mrv p))
;	   (d (psr-variavel-dominio p v))
	   )
;      (when DEBUG (format t "Vou experimentar a variavel:~a cujo dominio e:~a~%" v (psr-variavel-dominio p v)))
      (dolist (valor (psr-variavel-dominio p v))
	(multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	  (incf testes-totais testes)
	  (when consistente
;	    (when DEBUG (format t "Atribuindo o valor:~a a variavel:~a~%" valor v))
	    (psr-adiciona-atribuicao! p v valor)

;	    (when DEBUG
;	      (format DEBUG "Depois da atribuicao~%")
;	      (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
	    (multiple-value-bind (inferencias testes) (psr-forward-checking p v)
;	      (when DEBUG (format t "PR: inferencias:~a~%" inferencias))
	      (incf testes-totais testes)
	      (when inferencias ; teste suficiente?
		(let ((backup (make-hash-table :test 'equal)))
		  (maphash #'(lambda (iv id)
;			       (when DEBUG (format t "iv:~a id:~a~%" iv id))
			       (setf (gethash iv backup) (psr-variavel-dominio p iv))
			       (psr-altera-dominio! p iv id))
			   inferencias)

;		  (when DEBUG
;		    (format DEBUG "Depois das inferencias~%")
;		    (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))

		  (multiple-value-bind (resultado testes) (procura-retrocesso-fc-mrv p)
		    (incf testes-totais testes)
		    (when resultado
		      (return-from procura-retrocesso-fc-mrv (values resultado testes-totais))))

		  (maphash #'(lambda (bv bd)
			       (psr-altera-dominio! p bv bd))
			   backup)
;		  (when DEBUG
;		    (print "Removidas as inferencias:")
;		    (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
		  ))

	      (psr-remove-atribuicao! p v)
;	      (when DEBUG
;		(print "Removendo a atribuicao:")
;		(desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
	      )))))

    (return-from procura-retrocesso-fc-mrv (values nil testes-totais))))


(defun psr-forward-checking (p v)
  ""
  (let ((testes-totais 0)
	(inferencias (make-hash-table :test 'equal))
	(lista-arcos (psr-arcos-vizinhos-nao-atribuidos2 p v)))
;    (format t "FC arcos:~a~%" lista-arcos)
    (dolist (arco lista-arcos)
      (let ((v2 (first arco))
	    (v1 (rest arco)))

	(multiple-value-bind (revise testes) (psr-revise2 p v2 v1 inferencias)
;	  (when (equal v2 "(6 . 6)") (format t "v2:~a v1:~a~%inferencias:~a~%" v2 v1 inferencias))
	  (incf testes-totais testes)
	  (when revise
	    (multiple-value-bind (d exists) (gethash v2 inferencias)
	      (when exists
		(when (= (length d) 0)
		  (return-from psr-forward-checking (values nil testes-totais)))))))))

    (return-from psr-forward-checking (values inferencias testes-totais))))


;(defun psr-arcos-vizinhos-nao-atribuidos (p v)
;  ""
;  (let ((lista-arcos (list))) ; lista vs vector?
;    (dolist (v-na (psr-variaveis-nao-atribuidas p))
;      (when (not (equal v v-na))
;	(when (some #'(lambda (r) (find r (psr-variavel-restricoes p v-na)))
;		    (psr-variavel-restricoes p v))
;	  (push (cons v-na v) lista-arcos))))
;    (nreverse lista-arcos)))


(defun psr-arcos-vizinhos-nao-atribuidos2 (p v)
  ""
  (let ((lista-arcos (list))
	(envolvidas (remove-duplicates (reduce #'append (psr-variavel-restricoes p v) ; TODO: rever
					       :key 'restricao-variaveis)
				       :test 'equal)))
;    (format t "A variavel ~a tem estas restricoes:~a~%" v (psr-variavel-restricoes p v))
;    (format t "PAVNA: v:~a~%restricoes:~a~%envolvidas:~a~%" v (psr-variavel-restricoes p v) envolvidas)
;    (format t "PAVNA: var-natribuidas:~a~%" (psr-variaveis-nao-atribuidas p))
    (dolist (v-na (psr-variaveis-nao-atribuidas p))
      (when (not (equal v v-na))
	(when (find v-na envolvidas :test 'equal)
	  (push (cons v-na v) lista-arcos))))

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

    (let* ((v (psr-mrv p))
;	   (d (psr-variavel-dominio p v))
	   )
;      (when DEBUG (format t "Vou experimentar a variavel:~a cujo dominio e:~a~%" v (psr-variavel-dominio p v)))
      (dolist (valor (psr-variavel-dominio p v))
	(multiple-value-bind (consistente testes) (psr-atribuicao-consistente-p p v valor)
	  (incf testes-totais testes)
	  (when consistente
;	    (when DEBUG (format t "Atribuindo o valor:~a a variavel:~a~%" valor v))
	    (psr-adiciona-atribuicao! p v valor)

;	    (when DEBUG
;	      (format DEBUG "Depois da atribuicao~%")
;	      (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
	    (multiple-value-bind (inferencias testes) (psr-mac p v)
;	      (when DEBUG (format t "PR: inferencias:~a~%" inferencias))
	      (incf testes-totais testes)
	      (when inferencias ; teste suficiente?
		(let ((backup (make-hash-table :test 'equal)))
		  (maphash #'(lambda (iv id)
;			       (when DEBUG (format t "iv:~a id:~a~%" iv id))
			       (setf (gethash iv backup) (psr-variavel-dominio p iv))
			       (psr-altera-dominio! p iv id))
			   inferencias)

;		  (when DEBUG
;		    (format DEBUG "Depois das inferencias~%")
;		    (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))

		  (multiple-value-bind (resultado testes) (procura-retrocesso-mac-mrv p)
		    (incf testes-totais testes)
		    (when resultado
		      (return-from procura-retrocesso-mac-mrv (values resultado testes-totais))))

		  (maphash #'(lambda (bv bd)
			       (psr-altera-dominio! p bv bd))
			   backup)
;		  (when DEBUG
;		    (print "Removidas as inferencias:")
;		    (desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
		  ))

	      (psr-remove-atribuicao! p v)
;	      (when DEBUG
;		(print "Removendo a atribuicao:")
;		(desenha-fill-a-pix (psr->fill-a-pix p LC LC)))
	      )))))

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


;(defun resolve-best (tab)
;  ""
;  (multiple-value-bind (p) (procura-retrocesso-fc-mrv (fill-a-pix->psr tab))
;    (if (null p) nil (psr->fill-a-pix p (array-dimension tab 0) (array-dimension tab 1)))))
