(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-csv)
(ql:quickload "parse-float")
(use-package :parse-float)
(setq data (cl-csv:read-csv #P"~/Desktop/git/battle-city/logs/data.csv"))

(defun get-col(table col) 
  (setf res `())
  (setf i 0)
  (loop 
    (setf res (append res (list (parse-float (nth col (nth i data))))))
    (setf i (+ i 1))
    (when (= i (length table)) (return res))
  )
)

(defun avg (x)
  (defun sumup (x)
    (if (equal x nil) 0
      (+ (car x) (sumup (cdr x)))
    )
  )

  (if 
    (equal x nil) 0
    (/ (sumup x) (list-length x))
  )
)

(defun range(from to) 
  (setf res `())
  (setf i from)

  (loop 
    (setf res (append res (list i)))
    (setf i (+ i 1))
    (when (> i to) (return res))
  )
)

(defun sum-list(l) (apply `+ l))

(defun estimate-coef(x y)
  (setf n (length x))
  (setf m_x (avg x))
  (setf m_y (avg y))
  
  (setf SS_xy (- (sum-list (mapcar #'* x y)) (* n m_y m_x)))
  (setf SS_xx (- (sum-list (mapcar #'* x x)) (* n m_x m_x)))

  (setf b_1 (/ SS_xy SS_xx))
  (setf b_0 (- m_y (* b_1 m_x)))

  (list b_0 b_1)
)

(defun make-pred(coef args)
  (let 
    (
      (b_0 (car coef)) 
      (b_1 (cadr coef))
    )

    (mapcar #'(lambda (x) (+ b_0 (* b_1 x))) args)
  )
)

(defun linear-regression(values name)
  (let
    (
      (len (length values))
      (coefs (estimate-coef (range 1 (length values)) values))
    )

    (format T "<======= Linear regression for ~d =======>" name)
    (terpri)
    (format T "b_0: ~d, b_1: ~d" (car coefs) (cadr coefs))
    (terpri)
    (princ "Predictions:")
    (setq preds (make-pred coefs (range (+ len 1) (+ len 5))))
    (print preds)
    (terpri)
    (terpri)
    preds
  )
)

(defun write-csv(durations scores)
  (let 
    ((i 0))

    (with-open-file (str "./prediction.csv"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (loop 
      (format str "~d, ~d, ~d, minimax~%" 
        (if (< (nth i scores) 0) "player_lost" "player_won") 
        (nth i durations) 
        (nth i scores))
      (setq i (+ i 1))
      (when (= i (length durations)) (return))
    ))
  )
)


(setq durs (linear-regression (get-col data 1) "Game Duration"))
(setq scores (linear-regression (get-col data 2) "Game Score"))

(write-csv durs scores)
