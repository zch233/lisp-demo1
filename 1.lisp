; �������±��ʽ��ֵ��Ľ��:
; a. (+ (- 5 1) (+ 3 7))
; b. (list 1 (+ 2 3))

14
(1 5)


; ����3�ֲ�ͬ���ܷ���(a b c)��cons���ʽ

(cons 'a (cons 'b (cons 'c nil)))
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))


; ��car��cdr����һ������,�����ر�ĵ��ĸ�Ԫ��.

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


; ����һ������,�����������Ա���,���������нϴ��һ��.

(defun getMax (a b)
    (if (> a b)
        (format t "~A" a)
        (format t "~A" b)
    )
)
(getMax 1 3)


; ��Щ��������ʲô?

(defun enigma (x) ; �ж� x �б����Ƿ��� nil Ԫ��
     (and (not (null x))
          (or (null (car x))
              (enigma (cdr x)))))

(defun mystery (x y) ; ���� x ���б� y �е��±꣬���û����Ϊ nil
     (if (null y)
         nil
         (if (eql (car y) x)
             0
             (let ((z (mystery x (cdr y))))
               (and z (+ z 1))))))


; ������ı��ʽ��,x��Ӧ����ʲô�ɵó����?

(car (x (cdr '(a (b c) d)))) ; B
; car
(x 13 (/ 1 0)) ; 13
; or
(x #'list 1 nil) ; (1)
; or '(1) �� apply


; ֻ�ñ��½��ܵĲ�����,����һ������,������һ������Ϊ�Ա���,������t ������Ԫ����������һ�������Ǳ�.

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


; ���������ĵ����͵ݹ�汾:�� a. ����һ��������,����ӡ��ô����Ŀ�ĵ�. b. ����һ����,���ط���a�ڱ��г��ֵĴ���

; ����һ��������,����ӡ��ô����Ŀ�ĵ�---�ݹ�
(defun print-point (x)
    (if (= 0 x)
        nil
        (progn (format t "~%.")
            (print-point (- x 1)))))

; ����һ��������,����ӡ��ô����Ŀ�ĵ�---����
(defun print-point (x)
    (do ((i 0 (+ i 1)))
        ((= i x) 'done)
        (format t "~%.")))

; ����һ���б������� a ���б��������ֵĴ���--�ݹ�
(defun count-a (lst)
    (if (null lst)
        0
        (+ (if (eql 'a (car lst)) 1 0)
            (count-a (cdr lst)))))

; ����һ���б������� a ���б��������ֵĴ���--����
(defun count-a (lst)
    (let ((count 0))
        (dolist (n lst)
            (setf count
                (+ (if (eql 'a n) 1 0)
                    count)))
        count))


; һλ������дһ������,�����ر������з�nilԪ��֮��. ��д�˴˺����� �����汾, ��û��һ������ȷ����. ��ָ������������,��������ȷ�İ汾:

; (defun summit (lst)
;      (remove nil lst)
;      (apply #'+ lst))

; (defun summit (lst)
;      (let ((x (car lst)))
;        (if (null x)
;            (summit (cdr lst))
;            (+ x (summit (cdr lst))))))

; ��Ϊ remove ������ı� lst ����
(defun summit (lst)
    (setf nlst (remove nil lst))
        (apply #'+ nlst))

; ��Ϊ�ݹ�û�б߽��˳���֧
(defun summit (lst)
    (if  (null lst)
          0
          (let ((x (car lst)))
                (if  (null x)
                    (summit (cdr lst))
                    (+ x (summit (cdr lst)))))))