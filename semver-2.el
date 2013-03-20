; semver.el Semantic versioning 2 implementation in elisp
;
; See http://semver.org/ for the specification.

(let* ((g< "\\(")
       (g<? "\\(?:")
       (>g "\\)")
       (dot "\\.")
       (num+ "[[:digit:]]+")
       (alnumdash+ "[[:alnum:]-]+")
       (dotted-alnumddash (concat g<? alnumdash+
                                  g<? dot alnumdash+ >g "*" >g )))
  (defvar semver-2--re
    (concat "^" g< num+ dot num+ dot num+ >g
            g<? "-" g< dotted-alnumdash >g >g "?"
            g<? "+" g< dotted-alnumdash >g >g "?" "$")))

(defun semver-2-create (version &optional prerelease build)
  (list 'semver
        (semver-2--dotted-ids-parse-int version)
        (semver-2--dotted-ids-parse prerelease)
        (semver-2--dotted-ids-parse build)))

(defun semver-2-p (object)
  (and (listp object)
       (equal 'semver (nth 0 object))))

(defun semver-2-parse (string-or-semver)
  (if (semver-2-p string-or-semver)
      string-or-semver
    (when (string-match semver-2--re
                        string-or-semver)
      (let ((version
             (match-string 1 string-or-semver))
            (prerelease
             (match-string 2 string-or-semver))
            (build
             (match-string 3 string-or-semver)))
        (semver-2-create version
                       prerelease
                       build)))))

(defun semver-2--dotted-ids-parse (string-or-list)
  (mapcar 'semver-2--identifier-parse
          (if (stringp string-or-list)
              (split-string string-or-list "\\.")
            string-or-list)))

(defun semver-2--dotted-ids-parse-int (string-or-list)
  (mapcar 'semver-2--identifier-parse-int
          (if (stringp string-or-list)
              (split-string string-or-list "\\.")
            string-or-list)))

(defun semver-2--identifier-parse-int (string-or-int)
  (if (integerp string-or-int)
      string-or-int
    (string-to-number string-or-int)))

(defun semver-2--identifier-parse (string-or-int)
  (if (integerp string-or-int)
      string-or-int
    (if (string-match "^[[:digit:]]+$" string-or-int)
        (string-to-number string-or-int)
      string-or-int)))

(defun semver-2-format (semver)
  (setq semver (semver-2-parse semver))
  (let ((version (semver-2-version semver))
        (prerelease (semver-2-prerelease semver))
        (build (semver-2-build semver)))
    (concat (semver-2--dotted-ids-format version)
            (semver-2--dotted-ids-format prerelease "-")
            (semver-2--dotted-ids-format build "+"))))

(defun semver-2--dotted-ids-format (dotted-ids
                                  &optional prefix)
  (concat (if dotted-ids prefix "")
          (mapconcat 'identity
                     (mapcar 'semver-2--identifier-format
                             dotted-ids) ".")))
  
(defun semver-2--identifier-format (id)
  (if (integerp id)
      (format "%d" id)
    id))

(defun semver-2-version (semver)
  (setq semver (semver-2-parse semver))
  (nth 1 semver))

(defun semver-2-major (semver)
  (setq semver (semver-2-parse semver))
  (nth 0 (semver-2-version semver)))

(defun semver-2-set-major (major semver)
  (setq semver (semver-2-parse semver))
  (setcar (nthcdr 0 (semver-2-version semver))
          (semver-2--identifier-parse-int major))
  semver)

(defun semver-2-with-major (major semver)
  (setq semver (semver-2-parse semver))
  (semver-2-create (list major
                       (semver-2-minor semver)
                       (semver-2-patch semver))
                 (semver-2-prerelease semver)
                 (semver-2-build semver)))

(defun semver-2-inc-major (semver)
  (setq semver (semver-2-parse semver))
  (semver-2-set-major (1+ (semver-2-major semver)) semver)
  (semver-2-set-minor 0 semver)
  (semver-2-set-patch 0 semver))

(defun semver-2-minor (semver)
  (setq semver (semver-2-parse semver))
  (nth 1 (semver-2-version semver)))

(defun semver-2-set-minor (minor semver)
  (setq semver (semver-2-parse semver))
  (setcar (nthcdr 1 (semver-2-version semver))
          (semver-2--identifier-parse-int minor))
  semver)

(defun semver-2-with-minor (minor semver)
  (setq semver (semver-2-parse semver))
  (semver-2-create (list (semver-2-major semver)
                       minor
                       (semver-2-patch semver))
                 (semver-2-prerelease semver)
                 (semver-2-build semver)))

(defun semver-2-inc-minor (semver)
  (setq semver (semver-2-parse semver))
  (semver-2-set-minor (1+ (semver-2-minor semver)) semver)
  (semver-2-set-patch 0 semver))

(defun semver-2-patch (semver)
  (setq semver (semver-2-parse semver))
  (nth 2 (semver-2-version semver)))

(defun semver-2-set-patch (patch semver)
  (setq semver (semver-2-parse semver))
  (setcar (nthcdr 2 (semver-2-version semver))
          (semver-2--identifier-parse-int patch))
  semver)

(defun semver-2-with-patch (patch semver)
  (setq semver (semver-2-parse semver))
  (semver-2-create (list (semver-2-major semver)
                       (semver-2-minor semver)
                       patch)
                 (semver-2-prerelease semver)
                 (semver-2-build semver)))

(defun semver-2-inc-patch (semver)
  (setq semver (semver-2-parse semver))
  (semver-2-set-patch (1+ (semver-2-patch semver)) semver))

(defun semver-2-prerelease (semver)
  (setq semver (semver-2-parse semver))
  (nth 2 semver))

(defun semver-2-with-prerelease (prerelease semver)
  (setq semver (semver-2-parse semver))
  (semver-2-create (semver-2-version semver)
                 prerelease
                 (semver-2-build semver)))

(defun semver-2-set-prerelease (prerelease semver)
  (setq semver (semver-2-parse semver))
  (setcar (nthcdr 2 semver)
          (semver-2--dotted-ids-parse prerelease))
  semver)

(defun semver-2-build (semver)
  (setq semver (semver-2-parse semver))
  (nth 3 semver))

(defun semver-2-with-build (build semver)
  (setq semver (semver-2-parse semver))
  (semver-2-create (semver-2-version semver)
                 (semver-2-prerelease semver)
                 build))

(defun semver-2-set-build (build semver)
  (setq semver (semver-2-parse semver))
  (setcar (nthcdr 3 semver)
          (semver-2--dotted-ids-parse build))
  semver)

(defun semver-2-initial-p (semver)
  (setq semver (semver-2-parse semver))
  (= (semver-2-major semver) 0))

(defun semver-2-public-p (semver)
  (setq semver (semver-2-parse semver))
  (>= (semver-2-major semver) 1))

(defun semver< (semver with-respect-to)
  (setq semver (semver-2-parse semver))
  (setq with-respect-to (semver-2-parse with-respect-to))
  (< (semver-2--compare semver with-respect-to) 0))
        
(defun semver-2-satisfies-p (predicate semver)
  (setq predicate (semver-2-parse predicate))
  (setq semver (semver-2-parse semver))
  (<= (semver-2--compare predicate semver) 0))

(defun semver-2--compare (semver with-respect-to)
  (let ((res 0))
    (setq res 
          (semver-2--compare-dotted-ids
           (semver-2-version semver)
           (semver-2-version with-respect-to)))
    (if (zerop res)
        (setq res
              (semver-2--dotted-ids-over-nil
               (semver-2-prerelease with-respect-to)
               (semver-2-prerelease semver))))
    (if (zerop res)
        (setq res
              (semver-2--compare-dotted-ids
               (semver-2-prerelease semver)
               (semver-2-prerelease with-respect-to))))
    (if (zerop res)
        (setq res
              (semver-2--dotted-ids-over-nil
               (semver-2-build semver)
               (semver-2-build with-respect-to))))
    (if (zerop res)
        (setq res
              (semver-2--compare-dotted-ids
               (semver-2-build semver)
               (semver-2-build with-respect-to))))
    res))

(defun semver-2--dotted-ids-over-nil (dotted-ids
                                   with-respect-to)
  (cond ((and (not dotted-ids)
              with-respect-to)
         -1)
        ((and dotted-ids
              (not with-respect-to))
         1)
        (t 0)))
  
(defun semver-2--compare-dotted-ids (dotted-ids
                                   with-respect-to)
  (let ((res 0))
    (while (and (or dotted-ids with-respect-to)
                (zerop res))
      (setq res
            (cond ((and dotted-ids with-respect-to)
                   (semver-2--compare-identifiers
                    (car dotted-ids)
                    (car with-respect-to)))
                  ((and dotted-ids
                        (not with-respect-to))
                   1)
                  ((and (not dotted-ids)
                        with-respect-to)
                   -1)))
      (setq dotted-ids (cdr dotted-ids))
      (setq with-respect-to (cdr with-respect-to)))
    res))

(defun semver-2--compare-identifiers (identifier
                                   with-respect-to)
  (cond ((and (stringp identifier)
              (stringp with-respect-to))
         (let ((res (compare-strings identifier
                                     nil nil
                                     with-respect-to
                                     nil nil)))
           (if (equal res t)
               0
             res)))
         ((and (stringp identifier)
               (integerp with-respect-to))
          1)
         ((and (integerp identifier)
               (stringp with-respect-to))
          -1)
         ((and (integerp identifier)
               (integerp with-respect-to))
          (- identifier with-respect-to))))

(provide 'semver-2)
