(require 'tt-string)

(defun tt:file-basename (filename)
  (tt:char-list-to-string (reverse (tt:take-while #'(lambda (x) (/= ?/ x))
                       (reverse (tt:string-to-list filename))))))

(defun tt:file-path-join (&rest paths)
  (reduce #'(lambda (x y) (concat (file-name-as-directory x) y)) paths))

(defun tt:file-get-contents (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun tt:file-put-contents (file-path string)
  "put content to FILEPATH's file."
  (with-temp-buffer
    (erase-buffer)
    (insert string)
    (write-region (point-min) (point-max) file-path)))

(defun tt:file-read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun tt:file-write-lines (file lst)
  "Write string list to a file"
  (tt:file-put-contents file (tt:string-list-to-string "\n" lst)))

(defun tt:file-remove-extension (filename)
  (string-replace-match "\\.\\([0-9a-zA-Z]*\\)$" filename ""))

(defun tt:file-get-extension (filename)
  (car (last (split-string filename "\\."))))

(provide 'tt-file)
