(defun is-symbol (char)
  (if (digit-char-p char)
      nil
      (if (eq char #\.)
          nil
          t)))

(defun is-gear (char)
  (if (eq char #\*)
      t
      nil))

;; check if there is a symbol in the given range
;; range is a concatenation of the number and all
;; symbols around it
(defun is-number-in-engine (range)
  (loop for char across range do
        (if (is-symbol char)
          (return-from is-number-in-engine t))
        )
  nil
  )

(defun get-gear-pos (range start)
  (let ((pos 0))
    (loop for char across range
          when (is-gear char)
            collect pos
          do (setf pos (1+ pos))
        )))


;; Return list of number in 'current' which are part
;; of the engine
(defun check-line (before current after)
  (let ((start 0)
        (end 0)
        (number 0)
        (res nil))
    (loop for char across current do
      (if (digit-char-p char)
          (progn (setf end (1+ end))
                 (setf number (+ (* number 10) (digit-char-p char)))))

      (if (and (or (not (digit-char-p char)) (eq end (length current))) (not (eq number 0)))
          (let ((s-arg (if (eq 0 start) start (1- start)))
                (e-arg (if (eq end (length current)) end (1+ end))))
            (if (is-number-in-engine
                 (concatenate 'string
                              (if (eq before nil) "" (subseq before s-arg e-arg))
                              (subseq current s-arg e-arg)
                              (if (eq after nil) "" (subseq after s-arg e-arg))))
                (setf res (cons number res))
                nil)
            (setf number 0)
            (setf start end)
            ))

      (if (not (digit-char-p char))
          (progn (setf start (1+ start))
                 (setf end (1+ end))))

      )
    res))

;; return list of numbers adj to star
;; in the following form : ((number star_x star_y) ...)
(defun get-numbers-with-gear (before current after pos)
  (let ((start 0)
        (end 0)
        (number 0)
        (res nil))
    (loop for char across current do
      (if (digit-char-p char)
          (progn (setf end (1+ end))
                 (setf number (+ (* number 10) (digit-char-p char)))))

      (if (and (or (not (digit-char-p char)) (eq end (length current))) (not (eq number 0)))
          (let* ((s-arg (if (eq 0 start) start (1- start)))
                (e-arg (if (eq end (length current)) end (1+ end)))
                (gears-pos (get-gear-pos
                 (concatenate 'string
                              (if (eq before nil) "" (subseq before s-arg e-arg))
                              (subseq current s-arg e-arg)
                              (if (eq after nil) "" (subseq after s-arg e-arg)))
                 s-arg)))

            (if (not (eq gears-pos nil))
                (loop for gear-x in gears-pos do
                (let ((x-arg (+ s-arg (mod gear-x (- e-arg s-arg))))
                      (y-arg (if (not (eq before nil))
                                 (if (< gear-x (- e-arg s-arg))
                                     (1- pos)
                                     (if (not (eq after nil))
                                         (if (>= gear-x (* 2 (- e-arg s-arg)))
                                             (1+ pos)
                                             pos)
                                         pos))
                                 (if (not (eq after nil))
                                     (if (>= gear-x (- e-arg s-arg))
                                         (1+ pos)
                                         pos)))))
                  (setf res (cons (list number x-arg y-arg) res))))
                nil)
            (setf number 0)
            (setf start end)
            ))

      (if (not (digit-char-p char))
          (progn (setf start (1+ start))
                 (setf end (1+ end))))

      )
    res))

(defun check-engine (list)
  (let ((pos 0))
    (loop for line in list
          when (not (eq (nth pos list) nil))
                collect (check-line (if (>= (1- pos) 0) (nth (1- pos) list) nil) line (nth (1+ pos) list))
          do (setf pos (1+ pos)))
    ))

(defun check-engine-2 (list)
  (let ((pos 0))
    (loop for line in list
          when (not (eq (nth pos list) nil))
                collect (get-numbers-with-gear (if (>= (1- pos) 0) (nth (1- pos) list) nil) line (nth (1+ pos) list) pos)
          do (setf pos (1+ pos)))
    ))

;; Create list of gear ratios
(defun get-gear-ratio (list)
  (if (endp list)
      nil
      (let ((first (car list))
            (second (cadr list)))
        (cons (* (nth 0 first) (nth 0 second)) (get-gear-ratio (cddr list))))
      ))

;; Delete elements with the same gear which are less or more than 2
(defun delete-if-not-two (list)
  (if (endp list)
      nil
      (let* ((head (car list))
            (tail (cdr list))
            (count (count head list :test #'(lambda (p1 p2) (and (eq (nth 1 p1) (nth 1 p2))
                                                             (eq (nth 2 p1) (nth 2 p2)))))))
        (if (= count 2)
            (cons head (cons (car tail) (delete-if-not-two (cdr tail))))
            (delete-if-not-two (nthcdr count list))))))



;; Build a list of lines (string)
(defun input-to-list (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        (cons line (input-to-list stream-in)))
    ))

(defun compute-result (file)
  (with-open-file (file-stream file)
    (reduce #'+ (apply 'append (check-engine (input-to-list file-stream))))
    ))

(defun compute-result-2 (file)
  (with-open-file (file-stream file)
    (reduce #'+
    (get-gear-ratio
     (delete-if-not-two
     (sort (sort (apply 'append (check-engine-2 (input-to-list file-stream)))
           #'(lambda (p1 p2) (< (nth 2 p1) (nth 2 p2))))
           #'(lambda (p1 p2) (and (< (nth 1 p1) (nth 1 p2)) (eq (nth 2 p1) (nth 2 p2)))))
     )))
  ))

(format T "Day 3 part 1: ~d~%" (compute-result "day3.txt"))
(format T "Day 3 part 2: ~d~%" (compute-result-2 "day3.txt"))
;; Part 2 idea:
