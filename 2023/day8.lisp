(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-line (stream-in)
    (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        line
        )))

(defun parse-steps (stream-in)
  (parse-line stream-in))

(defun parse-value (value)
  (remove-if
   #'(lambda (value) (equal (length value) 0))
   (cl-ppcre:split " "
    (cl-ppcre:regex-replace-all "(\\,|\\)|\\()" value "")))
  )

(defun parse-network-lines (stream-in)
  (let ((line (parse-line stream-in)))
    (if (eq line nil)
        nil
        (let* ((kv-pair (cl-ppcre:split "=" line))
               (value (parse-value (nth 1 kv-pair))))
          (acons (remove #\Space (nth 0 kv-pair))
                 (list (nth 0 value) (nth 1 value))
                 (parse-network-lines stream-in)))
      )))

(defun parse-network (stream-in)
  (parse-line stream-in)
  (parse-network-lines stream-in))

(defun access (network key)
  (assoc key network :test #'string=))

(defun get-direction (pattern network pos i)
  (if (eq (char pattern i) #\L)
      (nth 1 (access network pos))
      (nth 2 (access network pos))
      ))

(defun steps-to-end (pattern network start i)
  (let ((direction (get-direction pattern network start i)))
    (if (string= "ZZZ" direction)
        1
        (+ 1 (steps-to-end
              pattern
              network
              direction
              (if (eq (1+ i) (length pattern))
                  0
                  (1+ i)))))
 ))

(defun step-to-z (pattern network pos i)
    (let ((direction (get-direction pattern network pos i)))
    (if (is-an-end direction)
        1
        (+ 1 (step-to-z
              pattern
              network
              direction
              (if (eq (1+ i) (length pattern))
                  0
                  (1+ i)))))
 ))

(defun is-a-start (key)
  (eq (char key 2) #\A))

(defun is-an-end (key)
  (eq (char key 2) #\Z))

(defun compute-result (file)
  (with-open-file (file-stream file)
      (let ((steps (parse-steps file-stream))
            (network (parse-network file-stream)))
        (steps-to-end steps network "AAA" 0)
    )))

(defun collect-starts (pattern network)
  (loop for pair in network
        when (is-a-start (nth 0 pair))
          collect (step-to-z pattern network (nth 0 pair) 0)
        ))

(defun compute-result-2 (file)
  (with-open-file (file-stream file)
      (let ((steps (parse-steps file-stream))
            (network (parse-network file-stream)))
        (print (collect-starts steps network))
        (reduce #'lcm (collect-starts steps network))
    )))

(format T "Day 8 part 1: ~d~%" (compute-result "day8.txt"))
(format T "Day 8 part 2: ~d~%" (compute-result "day8.txt"))
