;; Лістинг реалізації першої частини завдання
(defun exchange (pairs r test &optional (i 0))
  (cond ((= i r) pairs)
        ((funcall test (car (second pairs)) (car (first pairs)))
         (cons (second pairs) (exchange (cons (first pairs) (cddr pairs)) r test (1+ i))))
        (t (cons (first pairs) (exchange (rest pairs) r test (1+ i))))))

(defun sort-pairs (pairs test &optional (r (1- (length pairs))))
  (if (< r 1)
      pairs
      (sort-pairs (exchange pairs r test) test (1- r))))

(defun sort-1-functional (lst &key (key #'identity) (test #'<))
  (let* ((pairs (mapcar (lambda (x) (cons (funcall key x) x)) lst))
         (sorted (sort-pairs pairs test)))
    (mapcar #'cdr sorted)))

;; Тестові набори та утиліти першої частини
(defun check-sort-1-functional (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (sort-1-functional input :key key :test test) expected)
          name))

(defun test-sort-1-functional ()
  (check-sort-1-functional "test 1" '(5 3 8 1 2) '(1 2 3 5 8))
  (check-sort-1-functional "test 2" nil nil)
  (check-sort-1-functional "test 3" '(7) '(7))
  (check-sort-1-functional "test 4" '(1 2 3 4) '(1 2 3 4))
  (check-sort-1-functional "test 5" '(3 1 3 2 1) '(1 1 2 3 3))
  (check-sort-1-functional "test 6" '(1 4 2 3) '(4 3 2 1) :test #'>)
  (check-sort-1-functional "test 7" '(-3 1 -2 4) '(1 -2 -3 4) :key #'abs)
  (check-sort-1-functional "test 8" '((b 2) (a 1) (c 2)) '((a 1) (b 2) (c 2)) :key #'second)
  (check-sort-1-functional "test 9" '(-3 1 -2 4) '(4 -3 -2 1) :key #'abs :test #'>))

;; Тестування першої частини
(test-sort-1-functional)


;; Лістинг реалізації другої частини завдання
(defun replacer (what to &key (test #'eql) count)
  (lambda (elem result)
    (cond ((and (funcall test elem what) (or (null count) (> count 0)))
           (when count (decf count))
           (cons to result))
          (t (cons elem result)))))

;; Тестові набори та утиліти другої частини
(defun check-replacer (name input expected what to &key (test #'eql) count)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (reduce (replacer what to :test test :count count) input
                         :from-end t :initial-value nil)
                 expected)
          name))

(defun test-replacer ()
  (check-replacer "test 1" '(1 1 1 4) '(2 2 2 4) 1 2)
  (check-replacer "test 2" '(1 1 1 4) '(1 2 2 4) 1 2 :count 2)
  (check-replacer "test 3" nil nil 1 2)
  (check-replacer "test 4" '(3 4 5) '(3 4 5) 1 2)
  (check-replacer "test 5" '(1 1 1) '(1 1 1) 1 2 :count 0)
  (check-replacer "test 6" '(1 5 1) '(2 5 2) 1 2 :count 10)
  (check-replacer "test 7" '(1 5 2 4) '(0 5 0 4) 3 0 :test #'<)
  (check-replacer "test 8" '(a b a c) '(a b x c) 'a 'x :count 1))

;; Тестування другої частини
(test-replacer)
