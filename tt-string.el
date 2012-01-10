(require 'tt-list)

(defun tt:string-head  (str)
  (substring str 0 1))

(defun tt:string-tail (str)
  (substring str 1))

(defun tt:string-left-trim (character-bag string)
  (let ((c-list (tt:string-to-list string))
        (bag-list (tt:string-to-list character-bag)))
    (apply #'concat
           (mapcar #'char-to-string
                   (tt:drop-while #'(lambda (c) (find c bag-list)) c-list)))))

(defun tt:string-right-trim (character-bag string)
  (let ((c-list (reverse (tt:string-to-list string)))
        (bag-list (tt:string-to-list character-bag)))
    (apply #'concat
           (mapcar #'char-to-string
                   (reverse (tt:drop-while #'(lambda (c) (find c bag-list)) c-list))))))

(defun tt:string-trim (character-bag string)
  (tt:string-left-trim character-bag
                    (tt:string-right-trim character-bag string)))

(defun tt:string-chomp (str)
     (let ((s (if (symbolp str)(symbol-name str) str)))
        (save-excursion
          (while (and
                  (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
                  (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
            (setq s (replace-match "" t nil s)))
          (while (and
                  (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
                  (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
            (setq s (replace-match "" t nil s))))
        s))

(defun tt:string-starts-with (string start)
  (string= start (substring string 0 (length start))))

(defun tt:string-ends-with (string end)
  (string= end (substring string
                          (- (length string) (length end))
                          (length string))))

(defun tt:string-to-string-list (str)
  (mapcar #'char-to-string (string-to-list str)))

(defun tt:char-list-to-string (cs)
  (apply #'concat (mapcar #'char-to-string cs)))

(defun tt:string-contains (str substr)
  (cond ((< (length str) (length substr)) nil)
        ((tt:string-starts-with str substr) t)
        ((tt:string-contains (apply #'concat (rest (tt:string-to-string-list str))) substr))))

(defun tt:string-to-lines (string)
    (split-string string "\n"))

(defun tt:string-join (separator lst)
  (mapconcat 'identity lst separator))

(provide 'tt-string)

