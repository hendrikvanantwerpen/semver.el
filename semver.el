; semver.el Semantic versioning implementation in elisp
;
; See http://semver.org/ for the specification.

(let* ((g< "\\(")
       (g<? "\\(?:")
       (>g "\\)")
       (dot "\\.")
       (o "\\|")
       (al "[[:alpha:]]")
       (al. "[[:alpha:]\\.]")
       (al+ "[[:alpha:]]+")
       (num+ "[[:digit:]]+")
       (num* "[[:digit:]]*")
       (alnum* "[[:alnum:]]*")
       (alnumd+ "[[:alnum:]-\\.]+")
       (alnumd* "[[:alnum:]-\\.]*")
       (num*al+numd* (concat g<? num* al. alnumd* >g)))
  (setq semver--re
    (concat "^" g< num+ dot num+ dot num+ >g
            g<? "-" g< num+ >g >g "?"
            g<? g< al alnumd* >g o
                g<? "-" g< num*al+numd* >g >g
            >g "?" "$")))

(defun semver-create (version &optional build prerelease)
  (list 'semver
        (semver--version-parse version)
        prerelease
        (semver--int-parse build)))

(defun semver-p (object)
  (and (listp object)
       (equal 'semver (nth 0 object))))

(defun semver-parse (string-or-semver)
  (if (semver-p string-or-semver)
      string-or-semver
    (when (string-match semver--re
                        string-or-semver)
      (let ((version
             (match-string 1 string-or-semver))
            (build
             (match-string 2 string-or-semver))
            (prerelease
             (or (match-string 3 string-or-semver)
                 (match-string 4 string-or-semver))))
        (semver-create version
                       build
                       prerelease)))))

(defun semver--version-parse (string-or-list)
  (mapcar 'semver--int-parse
          (if (stringp string-or-list)
              (split-string string-or-list "\\.")
            string-or-list)))

(defun semver--int-parse (string-or-int)
  (when string-or-int
    (if (integerp string-or-int)
        string-or-int
      (string-to-number string-or-int))))

(defun semver-format (semver)
  (setq semver (semver-parse semver))
  (let ((version (semver-version semver))
        (build (semver-build semver))
        (prerelease (semver-prerelease semver)))
    (concat (semver--version-format version)
            (if build
              (concat "-" (number-to-string build))
              "")
            (if prerelease
              (concat "-" prerelease)
              ""))))

(defun semver--version-format (version)
  (mapconcat 'identity
             (mapcar 'semver--int-format
                     version) "."))
  
(defun semver-version (semver)
  (setq semver (semver-parse semver))
  (nth 1 semver))

(defun semver-major (semver)
  (setq semver (semver-parse semver))
  (nth 0 (semver-version semver)))

(defun semver-set-major (major semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 0 (semver-version semver))
          (semver--int-parse major))
  semver)

(defun semver-with-major (major semver)
  (setq semver (semver-parse semver))
  (semver-create (list major
                       (semver-minor semver)
                       (semver-patch semver))
                 (semver-build semver)
                 (semver-prerelease semver)))

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
          (semver--int-parse minor))
  semver)

(defun semver-with-minor (minor semver)
  (setq semver (semver-parse semver))
  (semver-create (list (semver-major semver)
                       minor
                       (semver-patch semver))
                 (semver-build semver)
                 (semver-prerelease semver)))

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
          (semver--int-parse patch))
  semver)

(defun semver-with-patch (patch semver)
  (setq semver (semver-parse semver))
  (semver-create (list (semver-major semver)
                       (semver-minor semver)
                       patch)
                 (semver-build semver)
                 (semver-prerelease semver)))

(defun semver-inc-patch (semver)
  (setq semver (semver-parse semver))
  (semver-set-patch (1+ (semver-patch semver)) semver))

(defun semver-prerelease (semver)
  (setq semver (semver-parse semver))
  (nth 2 semver))

(defun semver-with-prerelease (prerelease semver)
  (setq semver (semver-parse semver))
  (semver-create (semver-version semver)
                 (semver-build semver)
                 prerelease))

(defun semver-set-prerelease (prerelease semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 2 semver) prerelease)
  semver)

(defun semver-build (semver)
  (setq semver (semver-parse semver))
  (nth 3 semver))

(defun semver-with-build (build semver)
  (setq semver (semver-parse semver))
  (semver-create (semver-version semver)
                 build
                 (semver-prerelease semver)))

(defun semver-set-build (build semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 3 semver)
          (semver--int-parse build))
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
          (semver--compare-version
           (semver-version semver)
           (semver-version with-respect-to)))
    (if (zerop res)
        (setq res
              (semver--better-not-nil
               (semver-build semver)
               (semver-build with-respect-to))))
    (if (and (zerop res)
             (semver-build semver)
             (semver-build with-respect-to))
        (setq res
              (- (semver-build semver)
                 (semver-build with-respect-to))))
    (if (zerop res)
        (setq res
              (semver--better-not-nil
               (semver-prerelease with-respect-to)
               (semver-prerelease semver))))
    (if (and (zerop res)
             (semver-prerelease semver)
             (semver-prerelease with-respect-to))
        (setq res
              (let ((tmp
                     (compare-strings
                      (semver-prerelease semver)
                      nil nil
                      (semver-prerelease with-respect-to)
                      nil nil)))
                (if (equal tmp t)
                    0
                  tmp))))
    res))

(defun semver--better-not-nil (object
                               with-respect-to)
  (cond ((and (not object)
              with-respect-to)
         -1)
        ((and object
              (not with-respect-to))
         1)
        (t 0)))
  
(defun semver--compare-version (version
                                with-respect-to)
  (let ((res 0))
    (while (and (or version with-respect-to)
                (zerop res))
      (setq res
            (cond ((and version with-respect-to)
                   (- (car version)
                      (car with-respect-to)))
                  ((and version
                        (not with-respect-to))
                   1)
                  ((and (not version)
                        with-respect-to)
                   -1)))
      (setq version (cdr version))
      (setq with-respect-to (cdr with-respect-to)))
    res))

(provide 'semver)
