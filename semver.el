; semver.el Semantic versioning implementation in elisp
;
; See http://semver.org/ for the specification.

(defvar semver--regexp
  (let* ((g< "\\(")
         (g<? "\\(?:")
         (>g "\\)")
         (dot "\\.")
         (num "[[:digit:]]+")
         (alnum "[[:alnum:]-]+")
         (alnums (concat g<? alnum g<? dot alnum >g "*" >g )))
    (concat "^" g< num dot num dot num >g
            g<? "-" g< alnums >g >g "?"
            g<? "+" g< alnums >g >g "?" "$")))

(defun semver-create (version &optional prerelease build)
  (list 'semver
        (semver--component-parse-int version)
        (semver--component-parse prerelease)
        (semver--component-parse build)))

(defun semver-p (object)
  (and (listp object)
       (equal 'semver (nth 0 object))))

(defun semver-parse (string-or-semver)
  (if (semver-p string-or-semver)
      string-or-semver
    (when (string-match semver--regexp
                        string-or-semver)
      (let ((version
             (match-string 1 string-or-semver))
            (prerelease
             (match-string 2 string-or-semver))
            (build
             (match-string 3 string-or-semver)))
        (semver-create version
                       prerelease
                       build)))))

(defun semver--component-parse (string-or-list)
  (mapcar 'semver--identifier-parse
          (if (stringp string-or-list)
              (split-string string-or-list "\\.")
            string-or-list)))

(defun semver--component-parse-int (string-or-list)
  (mapcar 'semver--identifier-parse-int
          (if (stringp string-or-list)
              (split-string string-or-list "\\.")
            string-or-list)))

(defun semver--identifier-parse-int (string-or-int)
  (if (integerp string-or-int)
      string-or-int
    (string-to-number string-or-int)))

(defun semver--identifier-parse (string-or-int)
  (if (integerp string-or-int)
      string-or-int
    (if (string-match "^[[:digit:]]+$" string-or-int)
        (string-to-number string-or-int)
      string-or-int)))

(defun semver-format (semver)
  (setq semver (semver-parse semver))
  (let ((version (semver-version semver))
        (prerelease (semver-prerelease semver))
        (build (semver-build semver)))
    (concat (semver--component-format version)
            (semver--component-format prerelease "-")
            (semver--component-format build "+"))))

(defun semver--component-format (component
                                &optional prefix)
  (concat (if component prefix "")
          (mapconcat 'identity
                     (mapcar 'semver--identifier-format
                             component) ".")))
  
(defun semver--identifier-format (id)
  (if (integerp id)
      (format "%d" id)
    id))

(defun semver-version (semver)
  (setq semver (semver-parse semver))
  (nth 1 semver))

(defun semver-major (semver)
  (setq semver (semver-parse semver))
  (nth 0 (semver-version semver)))

(defun semver-set-major (major semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 0 (semver-version semver))
          (semver--identifier-parse-int major))
  semver)

(defun semver-with-major (major semver)
  (setq semver (semver-parse semver))
  (semver-create (list major
                       (semver-minor semver)
                       (semver-patch semver))
                 (semver-prerelease semver)
                 (semver-build semver)))

(defun semver-inc-major (semver)
  (setq semver (semver-parse semver))
  (semver-set-major (1+ (semver-major semver)) semver)
  (semver-set-minor 0 semver)
  (semver-set-patch 0 semver))

(defun semver-minor (semver)
  (setq semver (semver-parse semver))
  (nth 1 (semver-version semver)))

(defun semver-set-minor (minor semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 1 (semver-version semver))
          (semver--identifier-parse-int minor))
  semver)

(defun semver-with-minor (minor semver)
  (setq semver (semver-parse semver))
  (semver-create (list (semver-major semver)
                       minor
                       (semver-patch semver))
                 (semver-prerelease semver)
                 (semver-build semver)))

(defun semver-inc-minor (semver)
  (setq semver (semver-parse semver))
  (semver-set-minor (1+ (semver-minor semver)) semver)
  (semver-set-patch 0 semver))

(defun semver-patch (semver)
  (setq semver (semver-parse semver))
  (nth 2 (semver-version semver)))

(defun semver-set-patch (patch semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 2 (semver-version semver))
          (semver--identifier-parse-int patch))
  semver)

(defun semver-with-patch (patch semver)
  (setq semver (semver-parse semver))
  (semver-create (list (semver-major semver)
                       (semver-minor semver)
                       patch)
                 (semver-prerelease semver)
                 (semver-build semver)))

(defun semver-inc-patch (semver)
  (setq semver (semver-parse semver))
  (semver-set-patch (1+ (semver-patch semver)) semver))

(defun semver-prerelease (semver)
  (setq semver (semver-parse semver))
  (nth 2 semver))

(defun semver-with-prerelease (prerelease semver)
  (setq semver (semver-parse semver))
  (semver-create (semver-version semver)
                 prerelease
                 (semver-build semver)))

(defun semver-set-prerelease (prerelease semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 2 semver)
          (semver--component-parse prerelease))
  semver)

(defun semver-build (semver)
  (setq semver (semver-parse semver))
  (nth 3 semver))

(defun semver-with-build (build semver)
  (setq semver (semver-parse semver))
  (semver-create (semver-version semver)
                 (semver-prerelease semver)
                 build))

(defun semver-set-build (build semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 3 semver)
          (semver--component-parse build))
  semver)

(defun semver-initial-p (semver)
  (setq semver (semver-parse semver))
  (= (semver-major semver) 0))

(defun semver-public-p (semver)
  (setq semver (semver-parse semver))
  (>= (semver-major semver) 1))

(defun semver< (semver with-respect-to)
  (setq semver (semver-parse semver))
  (setq with-respect-to (semver-parse with-respect-to))
  (< (semver--compare semver with-respect-to) 0))
        
(defun semver-satisfies-p (predicate semver)
  (setq predicate (semver-parse predicate))
  (setq semver (semver-parse semver))
  (<= (semver--compare predicate semver) 0))

(defun semver--compare (semver with-respect-to)
  (let ((res 0))
    (setq res 
          (semver--compare-components
           (semver-version semver)
           (semver-version with-respect-to)))
    (if (zerop res)
        (setq res
              (semver--component-over-nil
               (semver-prerelease with-respect-to)
               (semver-prerelease semver))))
    (if (zerop res)
        (setq res
              (semver--compare-components
               (semver-prerelease semver)
               (semver-prerelease with-respect-to))))
    (if (zerop res)
        (setq res
              (semver--component-over-nil
               (semver-build semver)
               (semver-build with-respect-to))))
    (if (zerop res)
        (setq res
              (semver--compare-components
               (semver-build semver)
               (semver-build with-respect-to))))
    res))

(defun semver--component-over-nil (component
                                   with-respect-to)
  (cond ((and (not component)
              with-respect-to)
         -1)
        ((and component
              (not with-respect-to))
         1)
        (t 0)))
  
(defun semver--compare-components (component
                                  with-respect-to)
  (let ((res 0))
    (while (and (or component with-respect-to)
                (zerop res))
      (setq res
            (cond ((and component with-respect-to)
                   (semver--compare-identifiers
                    (car component)
                    (car with-respect-to)))
                  ((and component
                        (not with-respect-to))
                   1)
                  ((and (not component)
                        with-respect-to)
                   -1)))
      (setq component (cdr component))
      (setq with-respect-to (cdr with-respect-to)))
    res))

(defun semver--compare-identifiers (identifier
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

(provide 'semver)
