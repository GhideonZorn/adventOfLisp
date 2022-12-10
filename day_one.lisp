(defvar local_calories 0)
(defvar calories_group ())

(defun sum_of_groups(line)
  (if (string= line "")
      (and (setq calories_group (cons local_calories calories_group)) (setq local_calories 0))
      (setq local_calories (+ local_calories (parse-integer line))))
  )

(defun parse_file(filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
            while line do
              (sum_of_groups line)
          )
    )
  )

(defun get_results (file)
  (parse_file file)
  (setq calories_group (sort calories_group #'>))
  (format t  "Day 1 part 1 is : ~D~%" (nth 0 calories_group))
  (format t  "Day 1 part 2 is : ~D~%" (reduce '+ (subseq calories_group 0 3)))
  )

(get_results "input_one")
