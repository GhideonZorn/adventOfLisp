(defvar total_score 0)
(defvar total_score_part2 0)

(defun what_to_play (p1 state)
  (if (= state 2)
      (setq total_score_part2 (+ total_score_part2 p1 3))
      (if (= state 3)
          (let ((play (mod (+ p1 1) 3)))
            (if (= play 0)
                (setq play 3)
                (setq play play))
            (setq total_score_part2 (+ total_score_part2 play 6)))

          (let ((play (- p1 1)))
            (if (= play 0)
                (setq play 3)
                (setq play play))
            (setq total_score_part2 (+ total_score_part2 play 0)))
          )
      )
  )

(defun win_or_lose (p1 p2)
  (if (or (and (eq p2 1) (eq p1 3)) (and (eq (abs (- p1 p2)) 1) (< p1 p2)))
      (setq total_score (+ total_score p2 6))
      (if (eq p1 p2)
          (setq total_score (+ total_score p2 3))
          (setq total_score (+ total_score p2 0))))
  )

(defun get_scoreOfRound (line)

  (let ((token1 (- (char-code (char line 0)) 64))
        (token2 (- (char-code (char line 2)) 87)))

    (win_or_lose token1 token2)
    (what_to_play token1 token2))

  )

(defun parse_file(filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line do
            (get_scoreOfRound line)
          )
    )
  )

(defun get_results (file)
  (parse_file file)
  (format t  "Day 2 part 1 is : ~D~%" total_score)
  (format t  "Day 2 part 2 is : ~D~%"  total_score_part2)
  )

(get_results "input_two")
