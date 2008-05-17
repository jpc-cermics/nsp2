
(defun job () 
  (goto-char (point-min))
  (replace-regexp "t4ht@95x" "_")
  (save-buffer)
)


