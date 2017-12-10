;;;; high-entropy.lisp

;;; --- Day 4: High-Entropy Passphrases ---
;;;
;;; A new system policy has been put in place that requires all
;;; accounts to use a passphrase instead of simply a password. A
;;; passphrase consists of a series of words (lowercase letters)
;;; separated by spaces.
;;;
;;; To ensure security, a valid passphrase must contain no duplicate
;;; words.
;;;
;;; For example:
;;;
;;; - aa bb cc dd ee is valid.
;;; - aa bb cc dd aa is not valid - the word aa appears more than
;;;   once.
;;; - aa bb cc dd aaa is valid - aa and aaa count as different words.
;;;
;;; The system's full passphrase list is available as your puzzle
;;; input. How many passphrases are valid?

(ql:quickload "cl-ppcre")

(defun split-str-to-words (str)
  (loop for ss in (cl-ppcre:split "\\s+" str)
     collect ss))

(defun list-to-set (list)
  (reduce #'(lambda (acc val) (adjoin val acc :test #'string=)) list :initial-value nil))

(defun validp (pp)
  (equal (length pp) (length (list-to-set pp))))

(defun main ()
  (loop for line = (read-line *standard-input* nil nil)
     while line ; stop on EOF
     for phrase = (split-str-to-words line)
     for is-valid = (validp phrase)
     when is-valid
     summing 1 into valid
     unless is-valid
     summing 1 into invalid
     finally (format t "~D INVALID~%~D VALID~%" invalid valid)))

(main)
