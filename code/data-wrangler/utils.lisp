(defun tsv-to-list (path)
  "convert a tab-separated value file to a list.
   path: string path name"

  (labels ((text-to-list (text)
             (read-from-string
               (concatenate 'string "(" text ")")))
           (read-lines (str &optional acc)
             (let ((line (read-line str nil 'eof)))
               (if (equal line 'eof)
                   acc
                   (read-lines
                     str
                     (append
                       acc
                       (list (text-to-list line))))))))
    (with-open-file (input-stream
                      (if (typep path 'string)
                          (make-pathname :name path)
                          path)
                      :direction :input)
      (read-lines input-stream))))
