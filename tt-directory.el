(defun tt:dirname (file)
  (chomp (shell-command-to-string (format "dirname %s" file))))

(defun tt:current-directory ()
  (let ((current-directory-or-file
         (or load-file-name buffer-file-name dired-directory "")))
    (if (file-directory-p current-directory-or-file) current-directory-or-file
      (tt:dirname current-directory-or-file))))

(provide 'tt-directory)
