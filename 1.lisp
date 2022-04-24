; 解释以下表达式求值后的结果:
; a. (+ (- 5 1) (+ 3 7))
; b. (list 1 (+ 2 3))

14
(1 5)


; 给出3种不同的能返回(a b c)的cons表达式

(cons 'a (cons 'b (cons 'c nil)))
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))


; 用car和cdr定义一个函数,它返回表的第四个元素.

(defun getForth (lst)
  (car (cdr (cdr (cdr lst)))))

(defun getForth (lst n)
    (let ((result lst)) 
        (do ((i 2 (+ i 1)))
            ((> i n) 'done)
            (setf result (cdr result))      
        )
        (car result)
    )
)

(getFour '(1 2 3 4 5 6 7 8 9 11) 5)


; 定义一个函数,它接受两个自变量,返回两个中较大的一个.

(defun getMax (a b)
    (if (> a b)
        (format t "~A" a)
        (format t "~A" b)
    )
)
(getMax 1 3)


; 这些函数做了什么?

(defun enigma (x) ; 判断 x 列表中是否有 nil 元素
     (and (not (null x))
          (or (null (car x))
              (enigma (cdr x)))))

(defun mystery (x y) ; 查找 x 在列表 y 中的下标，如果没有则为 nil
     (if (null y)
         nil
         (if (eql (car y) x)
             0
             (let ((z (mystery x (cdr y))))
               (and z (+ z 1))))))


; 在下面的表达式中,x处应该是什么可得出结果?

(car (x (cdr '(a (b c) d)))) ; B
; car
(x 13 (/ 1 0)) ; 13
; or
(x #'list 1 nil) ; (1)
; or '(1) 或 apply


; 只用本章介绍的操作符,定义一个函数,它接受一个表作为自变量,并返回t 如果表的元素中至少有一个类型是表.

(defun checkList (x)
    (and  x
          (or  (listp (car x))
                (checkList (cdr x)))))

(defun checkList (x)
    (if (null x)
        nil
        (if (listp (car x))
            t
            (checkList (cdr x)))))

; (checkList '(1 2)) // nil
; (checkList '(1 2 '(1))) // T


; 给出函数的迭代和递归版本:它 a. 接受一个正整数,并打印这么多数目的点. b. 接受一个表,返回符号a在表中出现的次数

; 接受一个正整数,并打印这么多数目的点---递归
(defun print-point (x)
    (if (= 0 x)
        nil
        (progn (format t "~%.")
            (print-point (- x 1)))))

; 接受一个正整数,并打印这么多数目的点---迭代
(defun print-point (x)
    (do ((i 0 (+ i 1)))
        ((= i x) 'done)
        (format t "~%.")))

; 接受一个列表，并返回 a 在列表里所出现的次数--递归
(defun count-a (lst)
    (if (null lst)
        0
        (+ (if (eql 'a (car lst)) 1 0)
            (count-a (cdr lst)))))

; 接受一个列表，并返回 a 在列表里所出现的次数--迭代
(defun count-a (lst)
    (let ((count 0))
        (dolist (n lst)
            (setf count
                (+ (if (eql 'a n) 1 0)
                    count)))
        count))


; 一位朋友想写一个函数,它返回表中所有非nil元素之和. 他写了此函数的 两个版本, 但没有一个能正确工作. 请指出错误在哪里,并给出正确的版本:

; (defun summit (lst)
;      (remove nil lst)
;      (apply #'+ lst))

; (defun summit (lst)
;      (let ((x (car lst)))
;        (if (null x)
;            (summit (cdr lst))
;            (+ x (summit (cdr lst))))))

; 因为 remove 并不会改变 lst 本身
(defun summit (lst)
    (setf nlst (remove nil lst))
        (apply #'+ nlst))

; 因为递归没有边界退出分支
(defun summit (lst)
    (if  (null lst)
          0
          (let ((x (car lst)))
                (if  (null x)
                    (summit (cdr lst))
                    (+ x (summit (cdr lst)))))))