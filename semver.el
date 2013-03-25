; semver.el Semantic versioning 1 implementation in elisp
;
; See http://semver.org/ for the specification.

(require 's)

(let* ((g< "\\(")
       (g<? "\\(?:")
       (>g "\\)")
       (dot "\\.")
       (o "\\|")
       (ws* "[[:space:]]*")
       (al "[[:alpha:]]")
       (al. "[[:alpha:]\\.]")
       (al+ "[[:alpha:]]+")
       (num+ "[[:digit:]]+")
       (num* "[[:digit:]]*")
       (alnum* "[[:alnum:]]*")
       (alnumd+ "[[:alnum:]-\\.]+")
       (alnumd* "[[:alnum:]-\\.]*"))
  (setq semver--with-build-re
    (concat "^"
            g< num+ >g dot g< num+ >g dot g< num+ >g
            g<? "-" g< num+ >g
                    g<? "-" g< alnumd+ >g >g "?" >g "?"
            "$"))
  (setq semver--sans-build-re
    (concat "^"
            g< num+ >g dot g< num+ >g dot g< num+ >g
            g<? g< al alnumd* >g o
                g<? "-" g< num* al. alnumd* >g >g >g "?"
            "$"))
  (setq semver--range-*-re
        "^\\*$")
  (setq semver--range-major-re
        (concat "^" g< num+ >g
                g<? dot "x" g<? dot "x" >g "?" >g "?" "$"))
  (setq semver--range-major-minor-re
        (concat "^" g< num+ >g
                g<? dot g< num+ >g g<? dot "x" >g "?" >g "?" "$"))
  (setq semver--pred-comp-re
        (concat "^" ws* g< "" o "=" o ">" o ">=" o "<=" o "<" o "~" >g
                ws* g< g<? alnumd+ o "\\*" >g >g ws* "$")))

;;; Parse, create and format semvers

(defun semver-create (major minor patch &optional build prerelease)
  "Create a new semver."
  (semver--create (semver--int-parse major)
                  (semver--int-parse minor)
                  (semver--int-parse patch)
                  (when build (semver--int-parse build))
                  prerelease))

(defun semver-parse (string-or-semver)
  (if (semver-p string-or-semver)
      string-or-semver
    (cond ((string-match semver--with-build-re
                         string-or-semver)
           (let ((major (match-string 1 string-or-semver))
                 (minor (match-string 2 string-or-semver))
                 (patch (match-string 3 string-or-semver))
                 (build (match-string 4 string-or-semver))
                 (prerelease (match-string 5 string-or-semver)))
             (semver-create major minor patch
                            build prerelease)))
          ((string-match semver--sans-build-re
                         string-or-semver)
           (let ((major (match-string 1 string-or-semver))
                 (minor (match-string 2 string-or-semver))
                 (patch (match-string 3 string-or-semver))
                 (prerelease
                  (or (match-string 4 string-or-semver)
                      (match-string 5 string-or-semver))))
             (semver-create major minor patch
                            nil prerelease))))))

(defun semver-copy (semver)
  "Create a deep copy of a semver."
  (setq semver (semver-parse semver))
  (copy-tree semver))

(defun semver-p (object)
  (and (listp object)
       (equal 'semver (nth 0 object))))

(defun semver--create (major minor patch &optional build prerelease)
  "Create a new semver. Allows creation of degenerate versions where components are nil or t."
  (list 'semver (list major minor patch) build prerelease))

(defun semver--int-parse (string-or-int)
  (if (integerp string-or-int)
      string-or-int
    (string-to-number string-or-int)))

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
             (mapcar 'number-to-string
                     version) "."))
  
;;; Access version, major, minor, patch, build and prerelease

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

(defun semver-inc-patch (semver)
  (setq semver (semver-parse semver))
  (semver-set-patch (1+ (semver-patch semver)) semver))

(defun semver-prerelease (semver)
  (setq semver (semver-parse semver))
  (nth 3 semver))

(defun semver-set-prerelease (prerelease semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 3 semver) prerelease)
  semver)

(defun semver-build (semver)
  (setq semver (semver-parse semver))
  (nth 2 semver))

(defun semver-set-build (build semver)
  (setq semver (semver-parse semver))
  (setcar (nthcdr 2 semver)
          (semver--int-parse build))
  semver)

(defun semver-initial-p (semver)
  (setq semver (semver-parse semver))
  (= (semver-major semver) 0))

(defun semver-public-p (semver)
  (setq semver (semver-parse semver))
  (>= (semver-major semver) 1))

;;; Comparisons

(defun semver= (semver with-respect-to)
  (setq semver (semver-parse semver))
  (setq with-respect-to (semver-parse with-respect-to))
  (= (semver-compare semver with-respect-to) 0))

(defun semver< (semver with-respect-to)
  (setq semver (semver-parse semver))
  (setq with-respect-to (semver-parse with-respect-to))
  (< (semver-compare semver with-respect-to) 0))
        
(defun semver>= (semver with-respect-to)
  (not (semver< semver with-respect-to)))

(defun semver-compare (semver with-respect-to)
  (let ((res 0))
    (if (zerop res)
        (setq res
              (semver--compare-extremes
               (semver-major semver)
               (semver-major with-respect-to)
               '-)))
    (if (zerop res)
        (setq res
              (semver--compare-extremes
               (semver-minor semver)
               (semver-minor with-respect-to)
               '-)))
    (if (zerop res)
        (setq res
              (semver--compare-extremes
               (semver-patch semver)
               (semver-patch with-respect-to)
               '-)))
    (if (zerop res)
        (setq res
              (semver--compare-extremes
               (semver-build semver)
               (semver-build with-respect-to)
               '- )))
    (if (zerop res)
        (setq res
              (semver--compare-extremes
               (semver-prerelease with-respect-to)
               (semver-prerelease semver)
               (lambda (obj wrt)
                 (let ((tmp
                        (compare-strings
                         wrt nil nil
                         obj nil nil)))
                   (if (equal t tmp) 0 tmp))))))
    res))

(defun semver--compare-extremes (object
                                 with-respect-to
                                 &optional deep-compare)
  "Compare with extremes, nil < anything < t. Anythings are considered equal."
  (cond ((or (and (eq t object)
                  (eq t with-respect-to))
             (and (not object)
                  (not with-respect-to)))
         0)
        ((or (eq t object)
             (not with-respect-to))
         1)
        ((or (not object)
             (eq t with-respect-to))
         -1)
        (t
         (if deep-compare
             (funcall deep-compare object with-respect-to)
           0))))

;;; Predicates

(defun semver-pred-parse (string)
  (if (semver-pred-p string)
      string
    (list 'semver-pred
          (semver--pred-or-parse string))))

(defun semver-pred-p (string-or-pred)
  (when (listp string-or-pred)
    (equal 'semver-pred (nth 0 string-or-pred))))

(defun semver--pred-or-parse (string)
  "parse 'range || range' or just range"
  (let* ((parts (mapcar 's-trim
                        (split-string string "||" t))))
    (append '(|)
            (mapcar 'semver--pred-and-parse
                    parts))))

(defun semver--pred-and-parse (string)
  "Parse form 'range-version - range-version'. Returns a range."
  (let* ((parts (mapcar 's-trim
                        (split-string string " - " t))))
    (cond ((= (length parts) 2)
           (list '&
            (list '>= (car (semver--range-parse (nth 0 parts))))
            (list '<= (cdr (semver--range-parse (nth 1 parts))))))
          ((= (length parts) 1)
           (semver--pred-comp-parse (nth 0 parts)))
          (t
           (error "Invalid and syntax")))))

(defun semver--pred-comp-parse (string)
  "Parse forms {>,>=,<,<=,=,~}version or just version. Returns a predicate."
  (when (string-match semver--pred-comp-re string)
    (let* ((comp (match-string 1 string))
           (rangestr (match-string 2 string))
           (range (semver--range-parse (s-trim rangestr))))
      (when range
        (cond ((or (equal "=" comp)
                   (equal "" comp))
               (list '& (list '>= (car range))
                        (list '<= (cdr range))))
              ((equal ">" comp)
               (list '> (car range)))
              ((equal ">=" comp)
               (list '>= (car range)))
              ((equal "<" comp)
               (list '< (cdr range)))
              ((equal "<=" comp)
               (list '<= (cdr range)))
              ((equal "~" comp)
               (let* ((start (car range))
                      (minor (semver-minor start))
                      (prerelease (semver-prerelease start))
                      (newstart (semver--create (semver-major start)
                                                minor
                                                (semver-patch start)
                                                (semver-build start)
                                                (if prerelease
                                                    prerelease
                                                  t)))
                      (end (semver--create (semver-major start)
                                           (if minor
                                               minor
                                             t)
                                           t t nil)))
                 (list '&
                       (list '>= newstart)
                       (list '<= end))))
              (t
               (error "Unknown comparator")))))))
    
(defun semver--pred-satisfied (pred semver)
  (cond ((eq '| (nth 0 pred))
         (let ((result nil))
           (mapc (lambda (subpred)
                   (setq result
                         (or result
                             (semver--pred-satisfied subpred semver))))
                 (nthcdr 1 pred))
           result))
        ((eq '& (nth 0 pred))
         (and (semver--pred-satisfied (nth 1 pred) semver)
              (semver--pred-satisfied (nth 2 pred) semver)))
        (t
         (funcall (nth 0 pred)
                  (semver-compare semver (nth 1 pred))
                  0))))

(defun semver--range-parse (string)
  "Parse a version range, return (>= ver, <= ver)."
  (cond ((string-match semver--range-*-re string)
         (cons (semver--create nil nil nil nil t) (semver--create t t t t nil)))
        ((string-match semver--range-major-re string)
         (let* ((major (string-to-number (match-string 1 string)))
                (start (semver--create major nil nil nil t))
                (end (semver--create (1+ major) nil nil nil t)))
           (cons start end)))
        ((string-match semver--range-major-minor-re string)
         (let* ((major (string-to-number (match-string 1 string)))
                (minor (string-to-number (match-string 2 string)))
                (start (semver--create major minor nil nil t))
                (end (semver--create major (1+ minor) nil nil t)))
           (cons start end)))
        (t
         (let* ((ver (semver-parse string)))
           (cons ver ver)))))

(defun semver-satisfies-p (pred semver)
  (setq pred (semver-pred-parse pred))
  (setq semver (semver-parse semver))
  (semver--pred-satisfied (nth 1 pred) semver))

(provide 'semver)
