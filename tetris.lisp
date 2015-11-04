;GRUPO tg043
;70492 - Tiago Sousa
;78418 - Magda Resende

;Tipo Accao
(defun cria-accao(coluna peca)
	(cons coluna peca))

(defun accao-coluna(accao)
	(car accao))

(defun accao-peca(accao)
	(cdr accao))

;Tipo Tabuleiro

(defun cria-tabuleiro()
	(make-array (list 18 10)))

(defun copia-tabuleiro (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

(defun tabuleiro-preenchido-p(tabuleiro linha coluna)
  (if (eql (aref tabuleiro linha coluna) NIL) NIL T))

(defun tabuleiro-altura-coluna(tabuleiro coluna)
	(let ((altura 0))
    (loop for i from 17 downto 0 do
	    (if (aref tabuleiro i coluna)
        (progn (setf altura (+ i 1)) (return))))
    altura))

(defun tabuleiro-linha-completa-p(tabuleiro linha)
  (let ((completa T))
		(loop for i from 0 upto 9 do
		  (if (eq (aref tabuleiro linha i) NIL) (progn (setf completa NIL) (return))))
  	completa))

(defun tabuleiro-preenche!(tabuleiro linha coluna)
  (if (and (>= linha 0)(< linha 18)
       		 (>= coluna 0)(< coluna 10))
      (setf (aref tabuleiro linha coluna) T)))

;Loop ate linha 16 porque a 17 esta sempre
;a NIL ou caso contrario o jogo termina
(defun tabuleiro-remove-linha!(tabuleiro linha)
  (loop for l from linha upto 16 do
  	(loop for c from 0 upto 9 do
    	(setf (aref tabuleiro l c)
            (aref tabuleiro (+ l 1) c)))))

(defun tabuleiro-topo-preenchido-p(tabuleiro)
  (loop for coluna from 0 upto 9 do
    (if (tabuleiro-preenchido-p tabuleiro 17 coluna) (return T))))

;changed eq to equalp for last test06
(defun tabuleiros-iguais-p(tabuleiro1 tabuleiro2)
  (equalp tabuleiro1 tabuleiro2))

;Just this!?
(defun tabuleiro->array(tabuleiro)
  (copia-tabuleiro tabuleiro))

(defun array->tabuleiro(array)
  (copia-tabuleiro array))
;-----------

;Tipo Estado

(defstruct estado
  pontos
  pecas-por-colocar
  pecas-colocadas
  Tabuleiro
)

(defun copia-estado(estado)
  (make-estado :pontos (estado-pontos estado)
    :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado))
    :pecas-colocadas (copy-list (estado-pecas-colocadas estado))
    :Tabuleiro (copia-tabuleiro (estado-Tabuleiro estado))))

;So para testar
; (defparameter *estadoteste* (make-estado
; :pontos 100
;  :pecas-por-colocar (list )
;  :pecas-colocadas (list )
;  :Tabuleiro t1))

(defun estados-iguais-p(estado1 estado2)
  (equalp estado1 estado2))

(defun estado-final-p(estado)
  (or (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado))
      (null (estado-pecas-por-colocar estado))))

;Tipo Problema
(defstruct problema
  estado-inicial
  (solucao 'solucao)
  (accoes 'accoes)
  (resultado 'resultado)
  (custo-caminho 'custo-caminho)
)

;So para testar
;(defparameter *problemateste* (make-problema
; :estado-inicial 100))

;(funcall (problema-solucao *problemateste*) *estadoteste*)
(defun solucao(estado)
  (and (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado)))
       (null (estado-pecas-por-colocar estado))))

;Fazer rotacao matriz para usar na peca (aplicavel a 2D arrays)
(defun mtp (A)
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (B (make-array `(,n ,m) :initial-element 0)))
    (loop for i from 0 below m do
          (loop for j from 0 below n do
                (setf (aref B j i)
                      (aref A i j))))
    B))

;Reverter array
(defun reverse-array(array)
  (let ((reversed (make-array (list (array-dimension array 0)
                              (array-dimension array 1)))))
  (loop for c from 0 upto (- (array-dimension array 1) 1) do
  	(loop for l from 0 upto (- (array-dimension array 0) 1) do
    	(setf (aref reversed (- (- (array-dimension reversed 0) 1) l) c)
            (aref array l c))))
  reversed))

;Parametriza a letra da peca com a sua geometria
(defun obter-geo-peca(peca)
  (if (equalp peca 'I) peca-i0
    (if (equalp peca 'L) peca-l0
      (if (equalp peca 'J) peca-j0
        (if (equalp peca 'O) peca-o0
          (if (equalp peca 'S) peca-s0
            (if (equalp peca 'Z) peca-z0
              peca-t0)))))))

;Devolve lista de accoes possiveis
(defun accoes(estado)
  (if (null (estado-pecas-por-colocar estado))
    (list )
    (let ((peca (obter-geo-peca (car (estado-pecas-por-colocar estado))))
          ;limit: largura do tabuleiro - largura da peca (10 - ?)
          (limit (- 10 (array-dimension (obter-geo-peca (car (estado-pecas-por-colocar estado))) 1)))
          (lista-accoes (list ))
          (iter 0))
      (loop for i from 0 upto limit do
  	    (push (cons i peca) lista-accoes)) ;lista accoes para primeira rotacao 
      
      (setf peca (reverse-array (mtp peca))) ;roda peca
      (setf limit (- 10 (array-dimension peca 1))) ;actualiza limit
      
      (if (not (equalp (obter-geo-peca (car (estado-pecas-por-colocar estado))) peca)) ;se a rotacao da peca nao for igual a original continua
        (loop 
          (push (cons iter peca) lista-accoes)
          (incf iter)
          (when (= (- iter 1) limit) ;chegou ao fim do ciclo
            (progn (setf peca (reverse-array (mtp peca))) ;roda peca
              (if (equalp (obter-geo-peca (car (estado-pecas-por-colocar estado))) peca) ;se a rotacao da peca for igual a original sai do ciclo
                (return)
                (progn (setf limit (- 10 (cadr (array-dimensions peca)))) ;actualiza limit
                       (setf iter 0))))))) ;iter a 0 para fazer o ciclo para a nova rotacao
    (reverse lista-accoes)))) ;devolve lista de accoes pela ordem correcta

;Calcula os pontos e remove as linhas completas do tabuleiro
(defun calcula-pontos(estado peca linha-inicial)
  (let ((tabuleiro (estado-Tabuleiro estado))
        (altura (- (array-dimension peca 0) 1))
        (nlinhas 0))
    
  ;ciclo para percorrer as linhas do tabuleiro (n de linhas = altura da peca)
  (loop for i from 0 upto altura do
    (if (tabuleiro-linha-completa-p tabuleiro linha-inicial) ;se linha estiver completa
      (progn (tabuleiro-remove-linha! tabuleiro linha-inicial) ;remove linha
             (incf nlinhas)) ;e incrementa n linhas removidas
      (incf linha-inicial))) ;caso contrario ve a linha acima
  
  (if (= nlinhas 1) ;calculo de pontos
    (setf (estado-pontos estado) (+ (estado-pontos estado) 100))
    (if (= nlinhas 2)
      (setf (estado-pontos estado) (+ (estado-pontos estado) 300))
      (if (= nlinhas 3)
        (setf (estado-pontos estado) (+ (estado-pontos estado) 500))
        (if (= nlinhas 4)
          (setf (estado-pontos estado) (+ (estado-pontos estado) 800))))))))
      
  
(defun resultado(estado accao)
  (let ((novo-estado (copia-estado estado)) ;copia estado
        (coluna (car accao))                ;coluna onde a peca vai ser colocada
        (peca (cdr accao))                  ;estrutura da peca
        ;altura onde a peca vai ser colodada a partir da 1 coluna
        (altura-max (tabuleiro-altura-coluna (estado-Tabuleiro estado) (car accao))))
    
    ;ciclo para descer a altura da peca se tiver elementos a NIL na 1 coluna
    (loop for l from 0 upto (- (array-dimension peca 0) 1) do
      (if (not (aref peca l 0)) ;se elemento da peca for NIL
        (decf altura-max) ;decrementa altura
        (return)))
    
    ;ciclo para descer a altura da peca se tiver elementos a NIL nas restantes coluna (desde 2 a ultima)
    (loop for c from (+ 1 coluna) upto (- (+ coluna (array-dimension peca 1)) 1) do
      (if (> (tabuleiro-altura-coluna (estado-Tabuleiro novo-estado) c) altura-max) ;se topo da coluna for maior que o topo da anterior
        (progn (setf altura-max (tabuleiro-altura-coluna (estado-Tabuleiro novo-estado) c)) ;actualiza altura 
              ;ciclo para descer a altura da peca se tiver elementos a NIL na coluna respectiva (- c coluna)
              (loop for l from 0 upto (- (array-dimension peca 0) 1) do
                (if (not (aref peca l (- c coluna))) ;se elemento da peca for NIL
                  (decf altura-max) ;decrementa altura
                  (return))))))
    ;ciclo para preencher o tabuleiro com a peca
   	(loop for l from altura-max upto (min 17 (- (+ altura-max (array-dimension peca 0)) 1)) do
	    (loop for c from coluna upto (- (+ coluna (array-dimension peca 1)) 1) do
        (if (aref peca (- l altura-max) (- c coluna))
          (setf (aref (estado-Tabuleiro novo-estado) l c) (aref peca (- l altura-max) (- c coluna))))))
    (push (car (estado-pecas-por-colocar novo-estado)) (estado-pecas-colocadas novo-estado)) ;actualiza lista das pecas-colocadas
    (pop (estado-pecas-por-colocar novo-estado)) ;retira a peca colocada da lista das pecas por-colocar
    (if (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro novo-estado))) ;se nao foi game over
     (calcula-pontos novo-estado peca altura-max)) ;calcula pontos
    novo-estado))
	 
(defun qualidade(estado)
  (-(estado-pontos estado)))

(defun custo-oportunidade(estado)
  (let ((maxPontos 0)
        (pecas (estado-pecas-colocadas estado))
        (height 0))
    (loop for i from 0 upto (- (length pecas) 1) do
      (setf height (array-dimension (obter-geo-peca (nth i pecas)) 0))
      (if (= height 2)
        (setf maxPontos (+ maxPontos 300))
        (if (= height 3)
          (setf maxPontos (+ maxPontos 500))
          (if (= height 4)
            (setf maxPontos (+ maxPontos 800))))))
    (- maxPontos (if (not (estado-pontos estado)) 0
                     (estado-pontos estado))))) 
  
; (load (compile-file "utils.lisp"))
(load "utils.fas")