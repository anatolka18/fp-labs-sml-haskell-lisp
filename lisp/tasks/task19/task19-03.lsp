(load "streams.lsp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Функция для извлечения n элементов из потока.
;; Эта функция создает обычный список из первых n элементов потока.
(defun take (n stream)
  (cond 
      ; Если n равно 0 или поток пуст, возвращаем пустой список.
      ((empty-stream-p stream) ())
      ;; Добавляем голову потока в список
      ;; И рекурсивно вызываем take для оставшейся части потока.
      ((> n 1) (cons (head stream) 
                     (take (- n 1) (tail stream))))
      ;; Иначе добавляем голову потока в список
      (T (cons (head stream) ()))))

;; Функция для удаления первых n элементов из потока.
;; Возвращает поток без первых n элементов.
(defun drop (n stream)
  (if (or (= n 0) (empty-stream-p stream))
      ; Если n равно 0 или поток пуст, возвращаем сам поток.
      TES
      ; Иначе рекурсивно удаляем элементы из потока, пока не удалим n элементов.
      (drop (- n 1) (tail stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Функция myDiv выполняет целочисленное деление num на den.
(defun myDiv (num den &optional (res 1))
  (if (= num den)
      res
      (if (< num den)
          (- res 1)
          (myDiv (- num den) den (+ res 1)))))

;; Функция countDig считает количество разрядов числа x.
;; Функция возвращает количество разрядов числа x.
;; Рекурсивно вызывает саму себя, деля x на 10 и умножая k на 10,
;; пока x не станет равным 0.
(defun countDig (x k)
  (if (not (= x 0)) 
      (countDig (myDiv x 10) (* k 10))
      k))

;; Эта вспомогательная функция для containsDig.
;; Если x становится равным 0, то проверяем, делится ли y на 10 без остатка.
;; Иначе, проверяем, равен ли остаток от деления y на (countDig x 1) числу x.
(defun containsDigHelp (x y)
  (if (= x 0)
      (= (mod y 10) 0)
      (= (mod y (countDig x 1)) x)))

;; Функция containsDig проверяет, содержит ли число y цифры из числа x.
;; Если y становится равным 0, то вся проверка завершается, и возвращается ложь.
;; Вызываем вспомогательную функцию для дальнейшей проверки.
;; Если containsDigHelp вернула истину, то возвращаем истину.
;; Иначе, рекурсивно вызываем ccontainsDig с уменьшенным y.
(defun containsDig (x)
  (lambda (y)
    (cond
      ;; Проверяем, является ли y NIL или 0
      ((or (null y) (= y 0)) NIL)
      ;; Проверяем, содержит ли y цифры из x
      ((containsDigHelp x y) T)
      ;; Рекурсивный вызов
      (T (funcall (containsDig x) (myDiv y 10))))))

;; функция - аналог iterate
(defun iterate (f x)
  (cons-stream x (iterate f (funcall f x))))

;; функция - аналог filter 
;; поток stream фильтруется в соответствии с предикатом p
(defun filter-stream (p stream)
  (cond ((empty-stream-p stream) TES)
        ((funcall p (head stream)) 
         (cons-stream (head stream) 
                      (filter-stream p (tail stream))))
        (T (filter-stream p (tail stream)))))

;; функция - аналог map
;; получает поток (бесконечный список)
;; формирует поток
(defun map-stream (f stream)
  (if (empty-stream-p stream) TES
      (cons-stream (funcall f (head stream))
                   (map-stream f (tail stream)))))

;; функция, аналог композиции (в Haskell - . )
;; предполагается, что второй аргумент - функция одного параметра
(defun o (f1 f2)
  (lambda (x) (funcall f1 (funcall f2 x))))

;; функция - аналог crossout (removeDig в моем 12 задании)
(defun removeDig (stream 
                 &aux 
                 (x (head stream))
                 (xs (tail stream)))
  (filter-stream (o 'not (containsDig x)) xs))

;; так как f19 зависит от входных параметров, определим её как
;; ф-ию с параметром a (начало последовательности)
(defun f19 (a)
  (map-stream 'head
              (iterate 'removeDig
                       (integers-starting-from a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (take 3 (f19 0)))
(print (take 15 (f19 5)))
(print (take 50 (f19 14)))
(print (take 80 (f19 29)))