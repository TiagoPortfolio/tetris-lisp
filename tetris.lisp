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

(defun tabuleiros-iguais-p(tabuleiro1 tabuleiro2)
  (equalp tabuleiro1 tabuleiro2))

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
  (if (or (null (estado-pecas-por-colocar estado))
          (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado))
          nil)
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
    
    (setf altura-max (max 0 altura-max)) ; evitar altura-max ser inferior a 0
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

; Estructura que guarda uma lista de estruturas nodeStruct e uma key para escolher o no a ser expandido
(defstruct q
  (key #'identity)
  (state nil))

; Estructura que guarda um estado e a accao que originou esse mesmo estado
(defstruct nodeStruct
  (state nil)
  (action nil))

; Funcao que expande os nos de acordo com o tipo de procura
(defun procura-geral (problem queue-insert-fn)
  (let ((nodes (make-queue-inicial problem))
        node    ;no a ser avaliado
        actions ;lista de accoes a ser retornada no fim
        depth)  ;depth = numero pecas por colocar
    
    (setf depth (length (estado-pecas-por-colocar (nodeStruct-state (car (q-state nodes))))))
    
    (loop (if (equalp (q-state nodes) (cria-heap)) (RETURN nil))
      (setq node (remove-front nodes)) ;remove no da fila/pilha para ser expandido
      (if (null node) (RETURN nil)) ;retorn NIL se nao houver mais nos
      (if (funcall (problema-solucao problem) (nodeStruct-state node)) ; se no for solucao 
          (progn (push (nodeStruct-action node) actions)
                 (setq actions (reverse actions))
                 (loop for i from 0 upto (- (- (length actions) 1) depth) do ;elimina accoes que estao a mais na lista
                   (pop actions))
                   (RETURN actions))) ;devolve a lista de accoes desde o estado inicial ao final
      (if (not (estado-final-p (nodeStruct-state node))) ;se no nao for estado final
       (progn (push (nodeStruct-action node) actions)
              (mapcar #'(lambda (x) (funcall queue-insert-fn nodes ;insere na fila/pilha o estado gerado por cada accao aplicada ao no
                          (if (eq #'insert-at-front queue-insert-fn)
                          (make-nodeStruct :state (funcall (problema-resultado problem) (nodeStruct-state node) x) :action x)
                          (list (make-nodeStruct :state (funcall (problema-resultado problem) (nodeStruct-state node) x) :action x)))))
                      (funcall (problema-accoes problem) (nodeStruct-state node))))
     ))))

; funcao auxiliar que chama a procura-geral com o tipo de insercao na fila/pilha de nos
(defun best-first-search (problem heuristic)
  (procura-geral problem #'(lambda (q state) 
            (insert-by-priority q state heuristic))))

(defun procura-pp (problem)
  (procura-geral problem #'insert-at-front))

(defun procura-A* (problem fn)
  (setf fn #'f-cost)
  (best-first-search problem fn))

(defun procura-best (tabuleiro pecas)
  (let (
    (problem (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro tabuleiro :pecas-colocadas () :pecas-por-colocar pecas)
               :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
    fn )
    (if (> (length pecas ) 3)
        (setf fn #'sum-of-h) ; escolhemos optar por esta funcao heuristica mais simples para numero de pecas a colocar superior a 3
        (setf fn #'f-cost))
  (best-first-search problem fn)))

;***HEURISTICAS***

; soma qualidade com custo-oportunidade
(defun f-cost (state)
  (+ (qualidade state) (custo-oportunidade state)))

; procura buracos no tabuleiro
(defun get-holes (tabuleiro)
  (let ((empty-line nil)
        (holes 0))
    (loop for linha from 0 upto 15 do
      (if empty-line (return holes)
        (loop for coluna from 0 upto 9 do
          (if (tabuleiro-preenchido-p tabuleiro linha coluna)
            (setf empty-line nil)
            (progn (setf empty-line T)
                   (if (tabuleiro-preenchido-p tabuleiro (+ linha 1) coluna)
                       (incf holes)))))))
    holes))

; retorna altura maxima do tabuleiro
(defun get-max-altitude (tabuleiro)
  (let ((max-altitude 0)
         altitude)
    (loop for coluna from 0 upto 9 do
      (setf altitude (tabuleiro-altura-coluna tabuleiro coluna))
      (if (> altitude max-altitude)
        (setf max-altitude altitude)))
    max-altitude))

; retorna numero de celulas preenchidas no tabuleiro
(defun get-cells (tabuleiro)
  (let ((empty-line nil)
        (cells 0))
    (loop for linha from 0 upto 15 do
      (if empty-line (return cells)
        (progn (setf empty-line T)
               (loop for coluna from 0 upto 9 do
                 (if (tabuleiro-preenchido-p tabuleiro linha coluna)
                   (progn (setf empty-line nil)
                          (incf cells)))))))
    cells))

; retorna declive maximo do tabuleiro
(defun get-max-declive (tabuleiro)
  (let ((max-declive 0)
         declive)
    (loop for coluna from 0 upto 8 do
      (setf declive (abs (- (tabuleiro-altura-coluna tabuleiro coluna) (tabuleiro-altura-coluna tabuleiro (+ coluna 1)))))
      (if (> declive max-declive)
        (setf max-declive declive)))
    max-declive))

; retorna declive total do tabuleiro
(defun get-declive-total (tabuleiro)
  (let ((declive 0))
    (loop for coluna from 0 upto 8 do
      (incf declive (abs (- (tabuleiro-altura-coluna tabuleiro coluna) (tabuleiro-altura-coluna tabuleiro (+ coluna 1))))))
    declive))

;soma de conjunto de heuristicas
(defun sum-of-h (state)
  (let ((tabuleiro (estado-Tabuleiro (nconc state))))
  (+ (* 20 (get-holes tabuleiro)) (get-cells tabuleiro)
     (get-max-altitude tabuleiro) (get-max-declive tabuleiro)
     (get-declive-total tabuleiro) (qualidade state))))
;***FIM HEURISTICAS***

; funcao que prepara a funcao heuristica e a pilha para ser feito as insercoes
(defun insert-by-priority (q nodes key)
  (setf (q-key q) key) ;assinala funcao heuristica
  (when (null (q-state q)) ;cria pilha se ainda nao existir
    (setf (q-state q) (cria-heap)))
  (loop for node in nodes do ;insere na fila/pilha
      (heap-insert (q-state q) node key)))

; insere na pilha
(defun heap-insert (heap node key)
  (vector-push-extend nil heap) ;insere nil na fila/pilha para ser substituido posteriormente
  (let ((i (- (length heap) 1))
        (val (funcall key (nodeStruct-state node)))) ;val contem o valor da heuristica
    ; enquanto tamanho da pilha for maior que zero e o valor do no antecessor for maior que o no actual
    (loop while (and (> i 0) (>= (heap-valor heap (heap-ante i) key) val))
      do (setf (aref heap i) (aref heap (heap-ante i)) ; actualiza a ordenacao da pilha
               i (heap-ante i)))
    (setf (aref heap i) node))) ; substitui o valor nil pelo no

; cria pilha
(defun cria-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

; funcoes auxiliares para extrair informacao da pilha
(defun heap-valor (heap i key) (declare (fixnum i)) (funcall key (nodeStruct-state (aref heap i))))
(defun heap-ante (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-esquerda (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-direita (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

; retira o melhor item da pilha que corresponde ao item com valor mais baixo
(defun heap-retira-min (heap key)
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (organiza-heap heap 0 key)
    min))

; organiza a pilha de acordo com os valores e tamanhos dos nos sucessores
(defun organiza-heap (heap i key)
  (let ((e (heap-esquerda i))
        (d (heap-direita i))
        (N (- (length heap) 1))
        minimo)
    (setf minimo (if (and (<= e N) (<= (heap-valor heap e key)
           (heap-valor heap i key)))
           e i))
    (if (and (<= d N) (<= (heap-valor heap d key) (heap-valor heap minimo key)))
      (setf minimo d))
    (when (/= minimo i)
      (rotatef (aref heap i) (aref heap minimo))
      (organiza-heap heap minimo key))))

; confirma se fila/pilha esta vazia
(defun empty-queue? (q)
  (= (length (q-state q)) 0))

; cria estructura q com o estado inicial
(defun make-queue-inicial (problem)
  (let ((q (make-q)))
    (insert-at-front q (make-nodeStruct :state (problema-estado-inicial problem) :action ()))
    q))

; insere no inicio da fila/pilha
(defun insert-at-front (q state)
  (let ((node state))
      (push node (q-state q))))

; remove primeiro no da fila/pilha
(defun remove-front (q)
  (if (listp (q-state q))
      (pop (q-state q))
    (heap-retira-min (q-state q) (q-key q))))
  
(load "utils.fas")