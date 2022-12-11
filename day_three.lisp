;; PART 1 :
;; for each line : create two sets after splitting the line
;; check intersection of each two sets (per line)
;; add value of intersection to sum

;; PART 2 :
;; create one set per line
;; check intersection of sets three per three
;; add value of intersection to sum

(defvar set1 ())
(defvar set2 ())
(defvar set3 ())
(defvar sumOfPriorities 0)
(defvar sumOfPrioritiesGroups 0)

(defun divide_string (line)
  (let ((fhalf ()) (shalf ()))
    (loop for c being the elements of (subseq line 0 (floor (length line) 2))
          do (pushnew c fhalf))
    (loop for c being the elements of (subseq line (floor (length line) 2) (length line))
          do (pushnew c shalf))

    (let ((priority 0) (letter (char-code (car (intersection fhalf shalf)))))
      (if (> letter 90)
          (setq priority (- letter 96))
          (setq priority (+ (- letter 64) 26)))
      (setq sumOfPriorities (+ sumOfPriorities priority))
      )
    )
  )

(defun three_elf_group(line)
  (if (eq set1 ())
      (loop for c being the elements of line
            do (pushnew c set1))
      (if (eq set2 ())
          (loop for c being the elements of line
                do (pushnew c set2))
          (loop for c being the elements of line
                do (pushnew c set3)))
      )
  (if (eq (eq set3 ()) nil)
      (let ((priority 0)
            (letter (char-code (car (intersection (intersection set1 set2) set3)))))
        (if (> letter 90)
            (setq priority (- letter 96))
            (setq priority (+ (- letter 64) 26)))
        (setq sumOfPrioritiesGroups (+ sumOfPrioritiesGroups priority))

        (setq set1 ())
        (setq set2 ())
        (setq set3 ())
        )
      )
  )

(defun parse_file(filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line do
            (divide_string line)
            (three_elf_group line)
          )
    )
  )

(defun get_results (file)
  (parse_file file)
  (format t  "Day 3 part 1 is : ~D~%" sumOfPriorities)
  (format t  "Day 3 part 2 is : ~D~%" sumOfPrioritiesGroups)
  )

(get_results "input_three")
