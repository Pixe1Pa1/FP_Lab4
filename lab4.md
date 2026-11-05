<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Поляков Єгор Олегович КВ-33</p>
<p align="right"><b>Рік</b>: 2026</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
    * використати функції вищого порядку для роботи з послідовностями (де/якщо це доречно, в разі, якщо функції вищого порядку не були використані при реалізації л.р. №3);
    * додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: key та test , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями (р. 5.1). При цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за можливості, має бути зменшене до необхідного мінімуму.

### Варіант першої частини 2
Алгоритм сортування обміном №1 (без оптимізацій) за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
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
```

### Тестові набори та утиліти першої частини

```lisp
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
```

### Тестування першої частини

```lisp
(test-sort-1-functional)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
passed... test 7
passed... test 8
passed... test 9
NIL
```


## Варіант другої частини 12
Написати функцію `replacer`, яка має два основні параметри `what` і `to` та два ключові параметри - `test` та `count`. `replacer` має повернути функцію, яка при застосуванні в якості першого аргументу `reduce` робить наступне: при обході списку з кінця, кожен елемент списка-аргумента `reduce`, для якого функція `test`, викликана з цим елементом та значенням `what`, повертає значення `t` (або не `nil`), заміняється на значення `to`. Якщо `count` передане у функцію, заміна виконується `count` разів. Якщо `count` не передане тоді обмежень на кількість разів заміни немає. `test` має значення за замовчуванням `#'eql`. Обмеження, які накладаються на використання функції-результату `replacer` при передачі у `reduce` визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції `reduce` `from-end` та `initial-value`).

```lisp
CL-USER> (reduce (replacer 1 2)
'(1 1 1 4)
:from-end ...
:initial-value ...)
(2 2 2 4)
CL-USER> (reduce (replacer 1 2 :count 2)
'(1 1 1 4)
:from-end ...
:initial-value ...)
(1 2 2 4)
```

## Лістинг реалізації другої частини завдання
```lisp
(defun replacer (what to &key (test #'eql) count)
  (lambda (elem result)
    (cond ((and (funcall test elem what) (or (null count) (> count 0)))
           (when count (decf count))
           (cons to result))
          (t (cons elem result)))))
```

### Тестові набори та утиліти другої частини
```lisp
(defun check-replacer (name input expected what to &key (test #'eql) count)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (reduce (replacer what to :test test :count count) input 
          :from-end t :initial-value nil) expected)
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
```

### Тестування другої частини
```lisp
(test-replacer)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
passed... test 7
passed... test 8
NIL
```