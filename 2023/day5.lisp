(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

;; Parse line of seeds and return a list of
;; all seeds
(defun get-seeds (seeds-line)
  (mapcar #'(lambda (value) (parse-integer value))
  (remove-if #'(lambda (value) (equal (length value) 0))
        (cl-ppcre:split " " (nth 1 (cl-ppcre:split ":" seeds-line)))
        )))

;; Return a list of seed and its range
;; like ((seed1 range1) (seed2 range2) ...)
(defun get-seeds-range (seeds)
  (if (eq seeds nil)
      nil
      (cons (cons (car seeds) (nth 1 seeds)) (get-seeds-range (cddr seeds)))
      ))

(defun is-skip-needed (line)
  (if (or (eq (length line) 0)
          (search "map" line :test 'char=))
      t
      nil
      ))

(defun skip-to-map-inputs (stream-in)
  (let ((line (parse-line stream-in)))
    (if (or (eq line nil) (not (is-skip-needed line)))
        line
        (skip-to-map-inputs stream-in))
    ))

;; Return a list of dst src range corresponding
;; to a line of the map
(defun parse-map-line (line)
  (mapcar #'(lambda (value) (parse-integer value))
  (remove-if #'(lambda (value) (equal (length value) 0))
        (cl-ppcre:split " " line))))

(defun build-map (line stream-in)
  (if (or (eq line nil) (eq (length line) 0))
      nil
      (cons (parse-map-line line) (build-map (parse-line stream-in) stream-in))
      ))

(defun get-next-map (stream-in)
    (build-map (skip-to-map-inputs stream-in) stream-in))

;; Map a number through the map
;; (like seed -> soil)
(defun compute-number-in-map (map number)
  (loop for member in map do
    (let ((dst (nth 0 member))
          (src (nth 1 member))
          (range (nth 2 member)))
      (if (and (>= number src)
               (<= number (1- (+ src range))))
          (return-from compute-number-in-map (+ dst (- number src))))
        ))
  number)

(defun parse-line (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        line
        )))

(defun get-location (map numbers stream-in)
  (if (not (eq map nil))
      (get-location
       (get-next-map stream-in)
       (compute-numbers map numbers) stream-in)
      numbers
      )
  )

(defun compute-numbers (map numbers)
  (mapcar #'(lambda (value) (compute-number-in-map map value)) numbers))

(defun compute-result (file)
  (with-open-file (file-stream file)
    (let ((seeds (get-seeds (parse-line file-stream))))
      (apply 'min
      (get-location (get-next-map file-stream) seeds file-stream)
       ))))


(defun reverse-compute-in-map (map reverse-num)
  (loop for member in map do
    (let ((dst (nth 0 member))
          (src (nth 1 member))
          (range (nth 2 member)))
      (if (and (>= reverse-num dst)
               (<= reverse-num (1- (+ dst range))))
          (return-from reverse-compute-in-map (+ src (- reverse-num dst))))
        ))
  reverse-num)

;; Get a list of all the maps
;; to start from the end
(defun get-all-maps (stream-in)
  (let ((new-map (get-next-map stream-in)))
    (if (eq nil new-map)
        nil
        (cons new-map (get-all-maps stream-in))
    )))

;; Check if the seed obtain by starting from x
;; location is valid
(defun is-seed-good (found-seed seeds-range)
  (loop for seed-range in seeds-range do
        (let* ((begin (nth 0 seed-range))
               (end (+ begin (1- (nth 1 seed-range)))))
          (if (and (>= found-seed begin) (<= found-seed end))
              (return-from is-seed-good found-seed))
          ))
  nil)

(defun sort-locations (location-map)
  (sort
   location-map
   #'(lambda(el1 el2) (< (nth 0 el1) (nth 0 el2)))
   ))

(defun get-seed (maps reverse-num)
  (if (eq maps nil)
      reverse-num
      (get-seed (cdr maps) (reverse-compute-in-map (car maps) reverse-num)))
  )

(defun seeds-to-pairs (seeds-range)
  (if (eq seeds-range nil)
      nil
      (cons (list (car seeds-range) (nth 1 seeds-range)) (seeds-to-pairs (cddr seeds-range)))
      ))

(defun find-valid-seed (maps seeds i)
  (if (is-seed-good (get-seed maps i) seeds)
      i
      (find-valid-seed maps seeds (1+ i))
      ))

(defun sort-by-location (maps)
  (cons (sort-locations (car maps)) (cdr maps)))

(defun start-from-lowest (seeds)
  (nth 0 (nth 0
    (sort seeds #'(lambda (el1 el2) (< (nth 0 el1) (nth 0 el2))))
  )))

(defun compute-result-2 (file)
  (with-open-file (file-stream file)
    (let ((seeds (seeds-to-pairs (get-seeds (parse-line file-stream))))
          (maps (sort-by-location (reverse (get-all-maps file-stream)))))
      (find-valid-seed maps seeds 0)
       )))

(format T "Day 5 part 1: ~d~%" (compute-result "day5.txt"))
(format T "Day 5 part 2: ~d~%" (compute-result-2 "day5.txt"))
