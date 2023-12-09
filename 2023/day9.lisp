(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-histories (stream-in)
    (let ((line (read-line stream-in nil)))
      (if (null line)
          nil
          (cons (mapcar #'(lambda (value) (parse-integer value))
                  (cl-ppcre:split " " line))
                (parse-histories stream-in))
        )))

(defun compute-steps-list (history)
  (if (eq (cadr history) nil)
      nil
      (cons (- (nth 1 history) (nth 0 history))
            (compute-steps-list (cdr history)))
      ))

(defun compute-steps-lists (history)
  (if (every #'(lambda (value) (equal value 0)) history)
      (list (nconc history (list 0)))
      (cons history (compute-steps-lists (compute-steps-list history)))
  ))

(defun get-last (history-steps)
  (car (last history-steps)))

(defun get-before-last (history-steps)
  (nth (- (length history-steps) 2) history-steps))

(defun list-next-values (history-steps)
    (if (eq (cadr history-steps) nil)
      nil
      (let ((next-value (+ (car (last (get-last history-steps)))
                           (car (last (get-before-last history-steps))))))

        (setf (nth (- (length history-steps) 2) history-steps)
              (nconc (get-before-last history-steps) (list next-value)))
        (cons next-value
        (list-next-values (reverse (cdr (reverse history-steps)))))
        )
      ))

(defun list-prev-values (history-steps)
    (if (eq (cadr history-steps) nil)
      nil
      (let ((prev-value (- (car (get-before-last history-steps))
                           (car (get-last history-steps)))))

        (setf (nth (- (length history-steps) 2) history-steps)
              (cons prev-value (get-before-last history-steps)))
        (cons prev-value (list-prev-values (reverse (cdr (reverse history-steps)))))
        )
      ))

(defun history-value (history list-func)
  (let ((history-steps (compute-steps-lists history)))
        (funcall list-func history-steps)
    ))

(defun histories-values (histories list-func)
  (loop for history in histories
    collect (car (last (history-value history list-func)))
        ))

(defun compute-result (file list-func)
  (with-open-file (file-stream file)
    (let ((histories (parse-histories file-stream)))
      (reduce #'+ (histories-values histories list-func))
    )))

(format T "Day 9 part 1: ~d~%" (compute-result "day9.txt" 'list-next-values))
(format T "Day 9 part 2: ~d~%" (compute-result "day9.txt" 'list-prev-values))
