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

;; TODO: Make non-recursive
(defun mess/tree-walk-path-apply (tree path function)
  "Go to PATH in TREE and apply FUNCTION there to inflict change."
  (pcase (car path)
    ((pred null) (funcall function tree))
    (i (cons (car tree) (append (take i (cdr tree))
                                (list (mess/tree-walk-path-apply (nth (+ i 1) tree) (cdr path) function))
                                (drop (+ i 1) (cdr tree)))))))

(defun mess/tree-insert-branch (tree path sub-tree)
  (mess/tree-walk-path-apply tree path
                             (lambda (tree-at-pos)
                               (append tree-at-pos (list sub-tree)))))

(defun mess/tree-insert-value (tree path value)
  "Insert a node in the path without creating a new branch."
  (mess/tree-walk-path-apply tree path
                             (lambda (tree-at-pos)
                               (list (car tree-at-pos) (cons value (cdr tree-at-pos))))))

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

(defun mess/build-character-tree (commands)
  "Read COMMANDS and return a tree for all the inputs and edits."
  (let ((rest-commands commands)
        ;; Start with a special token so we can handle our tree operations'
        ;; idiosyncrasies.
        (tree (list (cons "START" 0)))
        ins-path
        last-delete-p)
    (while rest-commands
      (pcase (plist-get (car rest-commands) :op)
        ('delete-line
         ;; Need to go back to start of the line and turn on delete flag
         (setf ins-path (mess/character-tree-go-left-line tree ins-path)
               last-delete-p t))
        ('org-delete-backward-char
         (setf ins-path (mess/character-tree-go-left tree ins-path)
               last-delete-p t))
        ('backward-delete-word
         (setf ins-path (mess/character-tree-go-left-word tree ins-path)
               last-delete-p t))
        ('org-return
         (let ((result (mess/character-tree-ins-char tree ins-path "\n" (plist-get (car rest-commands) :timestamp) last-delete-p)))
           (setf tree (car result)
                 ins-path (cdr result)
                 last-delete-p nil)))
        ('left-char
         ;; We lose the last item in the path in this case. To recover this, we will
         ;; rely on character typing timestamps.
         (setf ins-path (mess/character-tree-go-left tree ins-path)
               last-delete-p nil))
        ('right-char
         (setf ins-path (mess/character-tree-go-right tree ins-path)
               last-delete-p nil))
        ('right-word
         (signal 'unsupported-cmd "right-word not supported yet."))
        ('left-word
         (signal 'unsupported-cmd "left-word not supported yet."))
        ('org-self-insert-command
         (let ((result (mess/character-tree-ins-char tree
                                                     ins-path
                                                     (pcase (plist-get (car rest-commands) :key)
                                                       ("SPC" " ")
                                                       (char char))
                                                     (plist-get (car rest-commands) :timestamp)
                                                     last-delete-p)))
           (setf tree (car result)
                 ins-path (cdr result)
                 last-delete-p nil)))
        (_ (signal 'unsupported-cmd (format "Unsupported command: %s" (car rest-commands)))))
      (setf rest-commands (cdr rest-commands)))
    tree))

(defun mess/recreate-original (tree)
  "Recreate text from character TREE in a regular editor way."
  (let ((sub-tree tree))
    (while sub-tree
      (unless (string= (caar sub-tree) "START")
        (insert (caar sub-tree)))
      (setf sub-tree (car (last (cdr sub-tree)))))))

(defun mess/recreate-messy (tree)
  "Recreate text from TREE preserving the mess."
  (when tree
    (unless (string= (caar tree) "START")
      (insert (caar tree)))
    ;; TODO: This is what needs to be implemented
    (mess/recreate-messy (cdr tree))))

;; TODO
(defun mess/start-org ()
  "Start writing in org mode. This sets up the buffer to ensure logs
are nice."
  (interactive)
  (command-log-mode t)
  (auto-fill-mode -1))
