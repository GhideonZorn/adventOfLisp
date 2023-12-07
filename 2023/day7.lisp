(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defparameter *cards* '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))
(defparameter *part-2* nil)

(defun count-occurences (to-check str)
  (let ((occurence 0))
    (loop for char across str do
      (if (eq to-check char)
          (setf occurence (1+ occurence))))
    occurence))

(defun get-occurence-list (hand)
  (sort (loop for char in *cards*
              when (or (not *part-2*) (not (eq char #\J)))
                   collect (count-occurences char hand))
        '>
  ))


;; if l1 >= l2 -> 1
;; else nil
(defun compare-occurences (l1 l2)
  (if (eq l1 nil)
      t
      (if (> (car l1) (car l2))
          t
          (if (< (car l1) (car l2))
              nil
              (compare-occurences (cdr l1) (cdr l2))))
  ))

(defun list= (l1 l2)
  (if (and (eq l1 nil) (eq l2 nil))
      t
      (if (equal (car l1) (car l2))
          (list= (cdr l1) (cdr l2))
          nil
          )))

(defun get-value (char)
  (- (length *cards*) (position char *cards*)))

(defun compute-joker (occ-list str)
  (if *part-2*
      (cons (+ (car occ-list) (count-occurences #\J str))
            (cdr occ-list))
      occ-list))

(defun compare-one-by-one (l1 l2 i)
  (if (eq i (length l1))
      t
      (if (> (get-value (char l1 i)) (get-value (char l2 i)))
          t
          (if (< (get-value (char l1 i)) (get-value (char l2 i)))
              nil
              (compare-one-by-one l1 l2 (1+ i)))
          )))

(defun order-hands (game)
  (sort
   (loop for hand-bid in game collect
      (list (compute-joker (get-occurence-list (nth 0 hand-bid)) (nth 0 hand-bid)) (nth 0 hand-bid) (nth 1 hand-bid)))
    #'(lambda (l1 l2)
        (if (list= (nth 0 l1) (nth 0 l2))
            (compare-one-by-one (nth 1 l1) (nth 1 l2) 0)
            (compare-occurences (nth 0 l1) (nth 0 l2))))
    ))

(defun get-cards-bids (line)
  (let ((pair (remove-if #'(lambda (value) (equal (length value) 0))
        (cl-ppcre:split " " line))))
    (list (nth 0 pair) (parse-integer (nth 1 pair)))
    ))

(defun parse-game (stream-in)
    (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        (cons (get-cards-bids line) (parse-game stream-in))
        )))

(defun get-score (ordered-hands i)
  (if (eq ordered-hands nil)
      0
      (+ (* i (nth 2 (car ordered-hands))) (get-score (cdr ordered-hands) (1+ i)))
  ))

(defun compute-result (file)
  (with-open-file (file-stream file)
    (let ((game (parse-game file-stream)))
      (get-score (reverse (order-hands game)) 1)
      )))

(format T "Day 7 part 1: ~d~%"(compute-result "day7.txt"))
(setf *cards* '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))
(setf *part-2* t)
(format T "Day 7 part 2: ~d~%"(compute-result "day7.txt"))
