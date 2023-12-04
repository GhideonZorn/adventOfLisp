(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defparameter *map-of-copy* nil)

(defun get-numbers-list (game-part)
  (mapcar #'(lambda (value) (parse-integer value))
  (remove-if
   #'(lambda (value) (or (equal (length value) 0) (not (parse-integer value))))
   (cl-ppcre:split " " game-part))
  ))

(defun add-one-to-card (card-number)
  (if (assoc card-number *map-of-copy*)
      (rplacd (assoc card-number *map-of-copy*) (+ (cdr (assoc card-number *map-of-copy*)) 1))
      (setq *map-of-copy* (acons card-number 1 *map-of-copy*))
      )
  )

(defun card-number (game)
  (parse-integer (nth 1 (remove-if #'(lambda (value) (equal (length value) 0))
                         (cl-ppcre:split " " game))))
  )

(defun get-card-points (card-and-game)
  (let* ((game (cl-ppcre:split "\\|" (nth 1 card-and-game)))
         (winnings (get-numbers-list (nth 0 game)))
         (possessed (get-numbers-list (nth 1 game)))
         (card-value 0)
         (matching 0)
         (card-num (card-number (nth 0 card-and-game))))

    (add-one-to-card card-num)
    (loop for card in winnings
        when (member card possessed)
            do (progn (if (equal card-value 0)
                          (setf card-value (1+ card-value))
                          (setf card-value (* card-value 2)))
                      (setf matching (1+ matching))))

    (loop for j from 0 below (cdr (assoc card-num *map-of-copy*)) do
      (loop for i from 1 below (1+ matching) do
        (add-one-to-card (+ card-num i))))

    card-value))

(defun parse-line (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        0
        (+ (get-card-points (cl-ppcre:split ":" line))
           (parse-line stream-in)))
    ))

(defun compute-result (file)
  (with-open-file (file-stream file)
        (parse-line file-stream)))

(defun compute-result-2 (file)
  (reduce #'+
  (loop for item in *map-of-copy*
        collect (cdr item)
    ))
  )

(format T "Day 4 part 1: ~d~%" (compute-result "day4.txt"))
(format T "Day 4 part 2: ~d~%" (compute-result-2 "day4.txt"))
