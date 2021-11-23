(setf game-board (make-array `(5 5) :initial-element `empty))

(defun generate-board()
  (let 
    ((state (make-random-state T)))
    (dotimes (i 5)
      (dotimes (j 5) (
        if (> (random 10 state) 7) (setf (aref game-board i j) `wall)
      ))
    )
    (setf (aref game-board 0 2) `empty)
    (setf (aref game-board 3 2) `empty)
  )
)

(defstruct bfs-node
  coord
  parent
)

(defun bfs-expand(node)
  (let* (
    (res nil)
    (coord (bfs-node-coord node))
    (i (car coord))
    (j (cdr coord))
  )
    (if (and (> i 0) (not (eq (aref game-board (- i 1) j) `wall))) (setf res (append res (list (make-bfs-node :coord (cons (- i 1) j) :parent node)))))
    (if (and (< i 4) (not (eq (aref game-board (+ i 1) j) `wall))) (setf res (append res (list (make-bfs-node :coord (cons (+ i 1) j) :parent node)))))
    (if (and (> j 0) (not (eq (aref game-board i (- j 1)) `wall))) (setf res (append res (list (make-bfs-node :coord (cons i (- j 1)) :parent node)))))
    (if (and (< j 4) (not (eq (aref game-board i (+ j 1)) `wall))) (setf res (append res (list (make-bfs-node :coord (cons i (+ j 1)) :parent node)))))
    res
  )
)

(defun expand(coord)
  (let* (
    (res nil)
    (i (car coord))
    (j (cdr coord))
  )
    (if (and (> i 0) (not (eq (aref game-board (- i 1) j) `wall))) (setf res (append res (list (cons (- i 1) j)))))
    (if (and (< i 4) (not (eq (aref game-board (+ i 1) j) `wall))) (setf res (append res (list (cons (+ i 1) j)))))
    (if (and (> j 0) (not (eq (aref game-board i (- j 1)) `wall))) (setf res (append res (list (cons i (- j 1))))))
    (if (and (< j 4) (not (eq (aref game-board i (+ j 1)) `wall))) (setf res (append res (list (cons i (+ j 1))))))
    (setf res (append res (list coord)))
    res
  )
)

(defun BFS(start_node goal_node)
  (defun bfs-iteration (goal_node queue visited)
    (if queue
      (let ((node (car queue)))
        (if (equal (bfs-node-coord node) (bfs-node-coord goal_node)) node
          (progn
            (mapcar (lambda(n)
              (let (
                (i (car (bfs-node-coord n)))
                (j (cdr (bfs-node-coord n)))
              )
              (if (not (aref visited i j))
                (progn
                  (setf (aref visited i j) T)
                  (setf queue (append queue (list n)))
                )
              )
              )
            ) (bfs-expand node))
            (bfs-iteration goal_node (cdr queue) visited)
          )
        )
      )
    Nil)
  )
  (bfs-iteration goal_node (list (make-bfs-node :coord (bfs-node-coord start_node))) (make-array `(5 5) :initial-element nil))
)

(defun get-path(node)
  (defun build-path (node history)
    (if (bfs-node-parent node)
      (build-path (bfs-node-parent node) (cons (bfs-node-coord node) history))
      (cons (bfs-node-coord node) history)
    )
  )
  (build-path node nil)
)

(defun maximum (list)
  (reduce #'max list))

(defun minimum (list)
  (reduce #'min list))

(defstruct game-state
  player
  max-coord
  min-coord
  score
  child-nodes
  depth
)

(defun minimax(state)
  (defun max-move(state)
    (if (equal (game-state-max-coord state) (game-state-min-coord state)) -1
      (let*
        (
          (possible-moves (expand (game-state-max-coord state)))
          (child-nodes (mapcar (lambda(m)
            (let ((new-state 
              (make-game-state
                :player `min
                :max-coord m
                :min-coord (game-state-min-coord state)
                :score 0
                :depth (+ (game-state-depth state) 1)
            )))
              (setf (game-state-child-nodes state) (cons new-state (game-state-child-nodes state)))
              (minimax new-state)
            )
          ) possible-moves))
        )
        (maximum child-nodes)
      )
    )
  )

  (defun min-move(state)
    (if (equal (game-state-max-coord state) (game-state-min-coord state)) 1
      (let*
        (
          (possible-moves (expand (game-state-min-coord state)))
          (child-nodes (mapcar (lambda(m)
            (let ((new-state 
              (make-game-state
                :player `max
                :max-coord (game-state-max-coord state)
                :min-coord m
                :score 0
                :depth (+ (game-state-depth state) 1)
            )))
              (setf (game-state-child-nodes state) (cons new-state (game-state-child-nodes state)))
              (minimax new-state)
            )
          ) possible-moves))
        )
        (minimum child-nodes)
      )
    )
  )

  (if (> (game-state-depth state) 2) 0
    (if (eq (game-state-player state) `max)
      (setf (game-state-score state) (max-move state))
      (setf (game-state-score state) (min-move state))
    )
  )
)

(setf game-start-state
  (make-game-state 
    :player `max
    :max-coord (cons 0 2)
    :min-coord (cons 3 2)
    :score 0
    :child-nodes nil
    :depth 0
  )
)

(generate-board)
(minimax game-start-state)

(print game-board)
(print game-start-state)
