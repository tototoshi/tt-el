;; from alexandria
(defun tt:flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun tt:take (n lst)
  (cond ((or (null lst) (<= n 0)) nil)
        (t (cons (car lst) (tt:take (1- n) (cdr lst))))))

(defun tt:drop (n lst)
  (cond ((<= n 0) lst)
        (t (tt:drop (1- n) (cdr lst)))))

(defun tt:drop-right (n lst)
  (reverse (tt:drop n (reverse lst))))

(defun tt:take-while (fn lst)
  (cond ((or (null lst) (not (funcall fn (car lst)))) nil)
        (t (cons (car lst) (tt:take-while fn (cdr lst))))))

(defun tt:drop-while (fn lst)
  (cond ((not (funcall fn (car lst))) lst)
        (t (tt:drop-while fn (cdr lst)))))

(defun tt:split-at (n lst)
  (list (tt:take n lst) (tt:drop n lst)))

(defun tt:grouped (n lst)
  (cond ((or (<= n 0) (null lst)) nil)
        (t (cons (tt:take n lst) (tt:grouped n (tt:drop n lst))))))

(defun tt:init (lst)
  (reverse (cdr (reverse lst))))

(provide 'tt-list)
