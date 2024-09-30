;;; Tree operations on trees that look like the following
;; (node-value &rest children-trees)

(defun mess/tree-get (tree path)
  "Path is a list that tells which child to jump to at each
branch. When the path is empty, we return the sub-tree."
  (let ((sub-path path)
        (sub-tree tree))
    (while (not (null (car sub-path)))
      (setf sub-tree (nth (car sub-path) (cdr sub-tree))
            sub-path (cdr sub-path)))
    sub-tree))

(defun mess/tree-add-child (tree sub-tree)
  (append tree (list sub-tree)))

(defun mess/tree-insert-branch (tree path sub-tree)
  (pcase (car path)
    ((pred null) (mess/tree-add-child tree sub-tree))
    (i (cons (car tree) (append (take i (cdr tree))
                                (list (mess/tree-insert-branch (nth (+ i 1) tree) (cdr path) sub-tree))
                                (drop (+ i 1) (cdr tree)))))))

(defun mess/tree-insert-value (tree path value)
  "Insert a node in the path without creating a new branch."
  (pcase (car path)
    ((pred null) (list (car tree) (cons value (cdr tree))))
    (i (cons (car tree) (append (take i (cdr tree))
                                (list (mess/tree-insert-value (nth (+ i 1) tree) (cdr path) value))
                                (drop (+ i 1) (cdr tree)))))))

;; Set up for writing
(use-package command-log-mode
  :config
  (setq clm/log-repeat t
        ;; Makes it easy to parse the log
        clm/log-command-indentation 30
        ;; We save the log file along with the post
        clm/logging-dir default-directory
        ;; Log every command
        clm/log-command-exceptions* '(nil)
        ;; We will use second granularity to start with and see if there are any
        ;; issues with this.
        clm/time-string "%Y-%m-%dT%H:%M:%S"))

(defun mess/parse-log-line (line)
  (let ((splits (cl-remove-if (lambda (str) (string= str "")) (string-split line " "))))
    (list :timestamp (encode-time (parse-time-string (substring (nth 0 splits) 1 (- (length (nth 0 splits)) 1))))
          :op (intern-soft (nth 2 splits))
          :key (nth 1 splits))))

(defun mess/parse-log (log-file)
  "Parse commands from `log-file'."
  (with-current-buffer (find-file-noselect log-file)
    (goto-char (point-min))
    (let ((commands))
      (while (not (eobp))
        (push (mess/parse-log-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              commands)
        (forward-line))
      (reverse commands))))

(defun mess/character-tree-go-right (tree path)
  "Return a new PATH advancing one char to the right in the TREE."
  (let ((children (cdr (mess/tree-get tree path))))
    (if (null children)
        ;; There are no children (the end of text) so we stay at the same point.
        path
      ;; In this case we need to choose the right child to move towards. We do
      ;; this by finding the child with the latest :timestamp. A proxy of that
      ;; is just taking the last children since the insertion of branch happens
      ;; in temporal order.
      (append path (list (- (length children) 1))))))

(defun mess/character-tree-go-left (tree path)
  (butlast path))

(defun mess/character-tree-go-left-till (tree path stop-char)
  (let ((current-value (car (mess/tree-get tree path))))
    (if current-value
        (if (string= (car current-value) stop-char)
            path
          (mess/character-tree-go-left-till tree (butlast path) stop-char))
      path)))

(defun mess/character-tree-go-right-till (tree path stop-char)
  (signal 'unsupported-cmd "right-till not supported yet."))

(defun mess/character-tree-go-left-word (tree path)
  (signal 'unsupported-cmd "left-word not supported yet."))

(defun mess/character-tree-go-left-line (tree path)
  (mess/character-tree-go-left-till tree path "\n"))

(defun mess/character-tree-ins-value (tree path char timestamp)
  (let ((new-tree (mess/tree-insert-value tree path (cons char timestamp))))
    ;; New char is always added as a fresh and only child after the path.
    (cons new-tree (append path '(0)))))

(defun mess/character-tree-ins-branch (tree path char timestamp)
  (let ((siblings (cdr (mess/tree-get tree path)))
        (new-tree (mess/tree-insert-branch tree path (list (cons char timestamp)))))
    ;; New char is added as the last child of the insertion node
    (cons new-tree (append path (list (length siblings))))))

(defun mess/character-tree-ins-char (tree path char timestamp &optional last-delete-p)
  "Return new TREE and PATH after inserting CHAR and TIMESTAMP.

If LAST-DELETE-P is true, insert a branch, else insert value."
  (if last-delete-p
      (mess/character-tree-ins-branch tree path char timestamp)
    (mess/character-tree-ins-value tree path char timestamp)))

(defun mess/build-character-tree (commands &optional tree ins-path last-delete-p)
  (if (null tree)
      ;; Start with a special token so we can handle our tree operations'
      ;; idiosyncrasies.
      (mess/build-character-tree commands (list (cons "START" 0)) ins-path)
    (pcase (plist-get (car commands) :op)
      ((pred null) tree)
      ('delete-line
       ;; Need to go back to start of the line and turn on delete flag
       (mess/build-character-tree (cdr commands) tree (mess/character-tree-go-left-line tree ins-path) t))
      ('org-delete-backward-char
       (mess/build-character-tree (cdr commands) tree (mess/character-tree-go-left tree ins-path) t))
      ('backward-delete-word
       (mess/build-character-tree (cdr commands) tree (mess/character-tree-go-left-word tree ins-path) t))
      ('org-return
       (let ((result (mess/character-tree-ins-char tree ins-path "\n" (plist-get (car commands) :timestamp) last-delete-p)))
         (mess/build-character-tree (cdr commands) (car result) (cdr result))))
      ('left-char
       ;; We lose the last item in the path in this case. To recover this, we will
       ;; rely on character typing timestamps.
       (mess/build-character-tree (cdr commands) tree (mess/character-tree-go-left tree ins-path)))
      ('right-char
       (mess/build-character-tree (cdr commands) tree (mess/character-tree-go-right tree ins-path)))
      ('right-word
       (signal 'unsupported-cmd "right-word not supported yet."))
      ('left-word
       (signal 'unsupported-cmd "left-word not supported yet."))
      ('org-self-insert-command
       (let ((result (mess/character-tree-ins-char tree
                                                   ins-path
                                                   (pcase (plist-get (car commands) :key)
                                                     ("SPC" " ")
                                                     (char char))
                                                   (plist-get (car commands) :timestamp)
                                                   last-delete-p)))
         (mess/build-character-tree (cdr commands) (car result) (cdr result))))
      (_ (signal 'unsupported-cmd (format "Unsupported command: %s" (car commands)))))))

(defun mess/recreate-original (tree)
  "Recreate text from character TREE in a regular editor way."
  (when tree
    (unless (string= (caar tree) "START")
      (insert (caar tree)))
    (mess/recreate-original (car (last (cdr tree))))))

(defun mess/recreate-messy (tree)
  "Recreate text from TREE preserving the mess."
  (when tree
    (unless (string= (caar tree) "START")
      (insert (caar tree)))
    ;; TODO: This is what needs to be implemented
    (mess/recreate-messy (cdr tree))))

(let ((commands (mess/parse-log "./2024-09-30")))
  (mess/recreate-original (mess/build-character-tree commands)))

(defun mess/start-org ()
  "Start writing in org mode. This sets up the buffer to ensure logs
are nice."
  (interactive)
  (command-log-mode t)
  (auto-fill-mode -1))
