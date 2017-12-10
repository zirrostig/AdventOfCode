;;; sum_consecutive.lisp

;; --- Day 1: Inverse Captcha ---
;; The night before Christmas, one of Santa's Elves calls you in a
;; panic. "The printer's broken! We can't print the Naughty or Nice
;; List!" By the time you make it to sub-basement 17, there are only a
;; few minutes until midnight. "We have a big problem," she says;
;; "there must be almost fifty bugs in this system, but nothing else
;; can print The List. Stand in this square, quick! There's no time to
;; explain; if you can convince them to pay you in stars, you'll be
;; able to--" She pulls a lever and the world goes blurry.
;;
;; When your eyes can focus again, everything seems a lot more
;; pixelated than before. She must have sent you inside the computer!
;; You check the system clock: 25 milliseconds until midnight. With
;; that much time, you should be able to collect all fifty stars by
;; December 25th.
;;
;; Collect stars by solving puzzles. Two puzzles will be made
;; available on each day millisecond in the advent calendar; the
;; second puzzle is unlocked when you complete the first. Each puzzle
;; grants one star. Good luck!
;;
;; You're standing in a room with "digitization quarantine" written in
;; LEDs along one wall. The only door is locked, but it includes a
;; small interface. "Restricted Area - Strictly No Digitized Users
;; Allowed."
;;
;; It goes on to explain that you may only leave by solving a captcha
;; to prove you're not a human. Apparently, you only get one
;; millisecond to solve the captcha: too fast for a normal human, but
;; it feels like hours to you.
;;
;; The captcha requires you to review a sequence of digits (your
;; puzzle input) and find the sum of all digits that match the next
;; digit in the list. The list is circular, so the digit after the
;; last digit is the first digit in the list.
;;
;; For example:
;; * 1122 produces a sum of 3 (1 + 2) because the first digit (1)
;;   matches the second digit and the third digit (2) matches the
;;   fourth digit.
;; * 1111 produces 4 because each digit (all 1) matches the next.
;; * 1234 produces 0 because no digit matches the next.
;; * 91212129 produces 9 because the only digit that matches the next
;;   one is the last digit, 9.

(defun sum-if-eql (a b &optional (sum 0))
  (if (eql a b) (+ sum a) sum))

(defun sum-acc (&optional (acc (list 0 0)) (v 0))
  (list (sum-if-eql (cadr acc) v (car acc)) v))

(defun sum-consecutive (lst)
  (car (reduce #'sum-acc lst :initial-value (list 0 (car (last lst))))))

(defun str-to-num-list (s)
  (map 'list #'(lambda (c) (- (char-int c) 48)) s))

(defun inverse-captcha (s)
  (sum-consecutive (str-to-num-list s)))

;; Test cases from above
;; (format t "~D~%" (inverse-captcha "1122"))
;; (format t "~D~%" (inverse-captcha "1111"))
;; (format t "~D~%" (inverse-captcha "1234"))
;; (format t "~D~%" (inverse-captcha "91212129"))

(defun main ()
  (loop (format t "~D~%" (inverse-captcha (read-line)))))

(main)
