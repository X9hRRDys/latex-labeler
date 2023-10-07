;;; latex-labeler.el --- Simplify equation labeling in LaTeX -*- lexical-binding: t -*-

;; Copyright (C) 2023 X9hRRDys

;; Author: X9hRRDys
;; Keywords: tools

;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/X9hRRDys/latex-labeler

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Introduction

;; `latex-labeler.el' is an Emacs Lisp package designed to streamline
;; the process of labeling equations in LaTeX documents.  This package
;; enables numbering equations sequentially in a LaTeX file from top
;; to bottom.  The main purpose of `latex-labeler.el' is to
;; synchronize equation labels in a LaTeX file and a compiled
;; document.

;; Important note

;; While `latex-labeler.el' works seamlessly with AUCTeX and YaTeX, it
;; may encounter unexpected errors when used with the built-in
;; latex-mode, especially when labeling equations within subequations
;; environment.  We recommend using `latex-labeler.el' with AUCTeX or
;; YaTeX.

;; Usage

;; `latex-labeler.el' provides three commands for labeling equations
;; and updating references.

;; 1. `latex-labeler-update': Update equation labels and references
;; that match the predefined format in your LaTeX document.

;; 2. `latex-labeler-update-force': Forcefully update all labels and
;; references, regardless of the format.

;; 3. `latex-labeler-change-prefix-and-update': Change the label
;; prefix interactively and updates labels.  When you change the
;; prefix, LaTeX Labeler appends the necessary local variables
;; configuration to your LaTeX file.

;;; Code:
(defgroup latex-labeler nil
  "Simplify equation labeling in LaTeX."
  :group 'tools
  :prefix "latex-labeler-")

(defcustom latex-labeler-math-envs '("align" "equation" "eqnarray" "gather"
                                    "multline" "subequations" "alignat" "flalign")
  "List of math environments to be labeled."
  :type '(repeat string))

(defcustom latex-labeler-nonumber-at-linebreaks '("multline" "subequations")
  "Math environments where line breaks occur without numbering."
  :type '(repeat string))

(defcustom latex-labeler-refs '("eqref" "ref" "pageref")
  "List of reference command."
  :type '(repeat string))

(defcustom latex-labeler-initial-equation-number 1
  "Initial equation number."
  :type 'number
  :local t
  :safe (lambda (value)
          (and (numberp value) (<= 1 value))))

(defcustom latex-labeler-initial-subcounter "a"
  "Initial subcounter for subequations environments."
  :type '(choice (string :tag "String 'a' or 'A'" "a" "A")
                 (integer :tag "Integer greater than or equal to 1"))
  :local t
  :safe (lambda (value)
          (or (and (stringp value) (length= value 1))
              (and (integerp value) (<= 1 value)))))

(defcustom latex-labeler-prefix "eq"
  "Prefix for generated labels."
  :type 'string
  :local t
  :safe #'stringp)

(defcustom latex-labeler-prefix-separator ":"
  "Separator between prefix and counter."
  :type 'string
  :local t
  :safe #'stringp)

(defcustom latex-labeler-subprefix-separator ""
  "Separator between subprefix and counter."
  :type 'string
  :local t
  :safe #'stringp)

(defcustom latex-labeler-string-before-label " "
  "String inserted before the label."
  :type 'string
  :local t
  :safe #'stringp)

(defcustom latex-labeler-string-after-label ""
  "String inserted after the label."
  :type 'string
  :local t
  :safe #'stringp)

(defcustom latex-labeler-label-with-indent nil
  "If the value is t, perform indentation after labeling."
  :type 'boolean
  :local t
  :safe #'booleanp)

(defcustom latex-labeler-preserve-local-prefix t
  "If the value is t, the local prefix setting is added when updating."
  :type 'boolean
  :local t
  :safe #'booleanp)

(defcustom latex-labeler-update-reftex nil
  "Update RefTeX after labeling.
If the value is t and `reftex-mode' is enabled, update RefTeX
after labeling."
  :type 'boolern
  :local t
  :safe #'booleanp)

(require 'reftex-parse nil t)

(defun latex-labeler-re-search (regexp &optional limit noerror count)
  "A modified `re-search-forward' that skips comment regions.
Arguments REGEXP, LIMIT, NOERROR and COUNT are the same as those
in `re-search-forward'.  Return t if the search succeeds, and nil
otherwise."
  (let ((result nil))
    (while (and (not result)
                (re-search-forward regexp limit noerror count))
      (unless (nth 4 (syntax-ppss))
        (setq result t)))
    result))

(defun latex-labeler-collect-labels-in-buffer ()
  "Find old labels in a buffer.
Return a list whose element has a form (label-name
. label-position)."
  (let ((stack nil))
    (goto-char (point-min))
    (while (latex-labeler-re-search "\\\\label{\\([^{}]*\\)}" nil t)
      (push (cons (match-string-no-properties 1)
                  (match-beginning 1))
            stack))
    (nreverse stack)))

(defun latex-labeler-find-label-duplication ()
  "Find label duplication.
If duplication is found, return a list whose CAR is the duplicated
label name, and CDR is a list of label positions.  If there is
no duplication, return nil."
  (let ((lst (latex-labeler-collect-labels-in-buffer))
        (dup-label nil)
        (dup-pos nil))
    (while (and lst (not dup-label))
      (let* ((elm (pop lst))
             (dup-alist (assoc (car elm) lst)))
        (when dup-alist
          (setq dup-label (car elm)
                dup-pos (list (cdr elm)))
          (while dup-alist
            (push (cdr dup-alist) dup-pos)
            (setq lst (cdr (member dup-alist lst)))
            (setq dup-alist (assoc (car dup-alist) lst))))))
    (when dup-label
      (cons dup-label (nreverse dup-pos)))))

(defun latex-labeler-signal-label-duplication ()
  "Signal an error if a label duplication is found."
  (let ((duplist (latex-labeler-find-label-duplication)))
    (when duplist
      (error "Label \"%s\" is duplicated in lines %s"
             (car duplist)
             (mapconcat (lambda (x) (number-to-string (line-number-at-pos x)))
                        (cdr duplist) ", ")))))

(defun latex-labeler-find-corresponding-end (envname &optional limit)
  "Find a corresponding \\end{ENVNAME} from the current position.
The optional argument LIMIT is a position that bounds the search."
  (let ((level 1))
    (while (and (/= level 0)
                (latex-labeler-re-search
                 (concat "\\\\\\(begin\\|end\\){" envname "}")
                 limit t))
      (if (string= "begin" (match-string-no-properties 1))
          (setq level (1+ level))
        (setq level (1- level))))
    (when (= level 0)
      t)))

(defun latex-labeler-find-env (&optional limit)
  "Search a math environment from the current position.
Return a list that has a form ((beg . end) envname), where beg
and end are inner boundaries of a math environment.  The optional
argument LIMIT is a position that bounds the search."
  (when (latex-labeler-re-search
         (concat "\\\\begin{\\("
                 (mapconcat #'identity latex-labeler-math-envs "\\|") "\\)}")
         limit t)
    (let ((env-beg (match-end 0))
          (envname (match-string-no-properties 1)))
      (if (latex-labeler-find-corresponding-end envname limit)
          (list (cons env-beg (match-beginning 0)) envname)
        (error "L.%d: Corresponding \"\\end{%s}\" does not exist"
               (line-number-at-pos env-beg) envname)))))

(defun latex-labeler-collect-envs (region)
  "Collect math environments in REGION."
  (goto-char (car region))
  (let* ((stack nil)
         (end (cdr region))
         (lst (latex-labeler-find-env end)))
    (while lst
      (push lst stack)
      (setq lst (latex-labeler-find-env end)))
    (nreverse stack)))

(defun latex-labeler-find-nested-regions (region)
  "Find outer boundaries of nested environments in REGION.
REGION must have a form (beg . end), where beg and end are inner
boundaries of a math environment.  Return a list of nested
regions whose element have a form (nest-beg . nest-end)."
  (goto-char (car region))
  (let ((stack nil)
        (limit (cdr region)))
    (while (latex-labeler-re-search "\\\\begin{\\([^{}]*\\)}" limit t)
      (let ((nest-beg (match-beginning 0))
            (nest-env (match-string-no-properties 1)))
        (latex-labeler-find-corresponding-end nest-env limit)
        (push (cons nest-beg (match-end 0)) stack)))
    (nreverse stack)))

(defun latex-labeler-find-non-nested-regions (region)
  "Retern non-nested regions in REGION.
REGION must have a form (beg . end), where beg and end are inner
boundaries of a math environment."
  (let ((stack nil)
        (beg (car region))
        (nested-regions (latex-labeler-find-nested-regions region)))
    (while nested-regions
      (let ((elm (pop nested-regions)))
        (push (cons beg (car elm)) stack)
        (setq beg (cdr elm))))
    (push (cons beg (cdr region)) stack)
    (nreverse stack)))

(defun latex-labeler-point-inside-regions-p (regions pos)
  "Return t if POS is inside of an element of REGIONS.
Each element of REGIONS mutst have a form (beg . end)."
  (when regions
    (let ((elm (pop regions)))
      (if (<= (car elm) pos (cdr elm))
          t
        (latex-labeler-point-inside-regions-p regions pos)))))

(defun latex-labeler-find-subregions (region)
  "Separate REGION into subregions.
REGION must have a form (beg . end), where beg and end are points
on an inner boundary of a math environment.  Each subregion is
separated by \"\\\\\", but the \"\\\\\" inside of nested regions
are ignored.  Return a list of subregions whose element
is (sub-beg . sub-end) where sub-beg and sub-end are points on
the inner boundary of the subregion."
  (let ((stack nil)
        (sub-beg (car region))
        (limit (cdr region))
        (non-nested-regions
         (latex-labeler-find-non-nested-regions region)))
    (goto-char sub-beg)
    (while (latex-labeler-re-search "\\\\\\\\" limit t)
      (when (latex-labeler-point-inside-regions-p non-nested-regions
                                                  (match-beginning 0))
        (push (cons sub-beg (match-beginning 0)) stack)
        (setq sub-beg (match-end 0))
        (goto-char sub-beg)))
    (push (cons sub-beg limit) stack)))

(defun latex-labeler-remove-notag-subregions (subregions)
  "Remove unlabeled regions from SUBREGIONS.
Unlabeled regions have \"\\notag\" or \"\\nonumber\".  SUBREGIONS
is assumed to be the return value of `latex-labeler-find-subregions'."
  (let ((stack nil))
    (dolist (region subregions stack)
      (goto-char (car region))
      (unless (latex-labeler-re-search "\\\\\\(notag\\|nonumber\\)" (cdr region) t)
        (push region stack)))))

(defun latex-labeler-labelable-subregions (region)
  "Return labelable subregions in REGION.
REGION must have a form (beg . end) where beg and end are points
on an inner boundary of math environment.  Return a list of
subregions whose element is (sub-beg . sub-end).  One labelable
subregion may have one label.  The labelable subregion does not
have \"\\notag\" or \"\\nonumber\"."
  (latex-labeler-remove-notag-subregions
   (latex-labeler-find-subregions region)))

(defun latex-labeler-env (region-envname)
  "Return a list for a math environment.
REGION-ENVNAME is assumed to be a retuned value of
`latex-labeler-find-env', which has a form ((beg . end) envname)."
  (let ((region (car region-envname))
        (envname (cadr region-envname)))
    (list region
          envname
          (if (member envname latex-labeler-nonumber-at-linebreaks)
              (if (string= "subequations" envname)
                  (latex-labeler-find-non-nested-regions region)
                (latex-labeler-remove-notag-subregions (list region)))
            (latex-labeler-labelable-subregions region)))))

(defun latex-labeler-envs (region)
  "Collect data of math environments in REGION."
  (mapcar #'latex-labeler-env (latex-labeler-collect-envs region)))

(defun latex-labeler-find-old-label-in-region (region)
  "Find an old label in REGION.
REGION must have a form (beg . end).  Return a list of the old
label position and its label name.  Return nil if there are no
labels in REGION."
  (goto-char (car region))
  (if (latex-labeler-re-search "\\\\label{\\([^{}]*\\)}" (cdr region) t)
      (cons (match-beginning 1) (match-string-no-properties 1))
    (cons (cdr region) nil)))

(defun latex-labeler-find-old-label-in-regions (regions)
  "Find an old label in REGIONS.
Each element of REGIONS has a form (beg . end).  If the search
succeeds, return a list of the old label position and its label
name which has a form (position . labelname), and nil otherwise."
  (let ((result nil))
    (while (and regions (not result))
      (goto-char (caar regions))
      (if (latex-labeler-re-search "\\\\label{\\([^{}]*\\)}" (cdar regions) t)
          (setq result (cons (match-beginning 1)
                             (match-string-no-properties 1)))
        (if (cdr regions)
            (setq regions (cdr regions))
          (setq result (cons (cdar regions) nil)
                regions nil))))
    result))

(defun latex-labeler-labelable-subequations-region (region subregions)
  "Find labelable region in subequations.
REGION is a region of a inner boundary of a subequations
environments.  SUBREGIONS is assumed to be a list of subregions
that is generated by `latex-labeler-envs'."
  (let ((result nil))
    (while (and (not result) subregions)
      (let ((elm (pop subregions)))
        (goto-char (car elm))
        (when (latex-labeler-re-search "\\\\\\(notag\\|nonumber\\)" (cdr elm) t)
          (setq result (cons (car region) (match-beginning 0))))))
    (unless result
      (setq result region))
    result))

(defun latex-labeler-find-old-labels-in-env (env)
  "Find an old label in each subregion.
ENV must be a return value of `latex-labeler-env'.  Return a list
whose third element has a form (point . label-name)."
  (let ((region (car env))
        (envname (cadr env))
        (subregions (caddr env)))
    (if (string= "subequations" envname)
        (list region envname
              (list (latex-labeler-find-old-label-in-regions subregions))
              (latex-labeler-labelable-subequations-region region subregions))
      (list region envname
            (mapcar #'latex-labeler-find-old-label-in-region
                    subregions)))))

(defun latex-labeler-find-old-labels (region)
  "Find old labels in REGION."
  (mapcar #'latex-labeler-find-old-labels-in-env
          (latex-labeler-envs region)))

(defun latex-labeler-make-marker-list (oldlabels)
  "Make a list of marker and old label pairs.
Each element of OLDLABELS has a form (label-point
. oldlabel-name).  If there is no old label, oldlabel-name is
nil.  Return a list whose element has a form (marker
oldlabel-name)."
  (mapcar (lambda (x)
            (goto-char (car x))
            (list (point-marker) (cdr x)))
          oldlabels))

(defun latex-labeler-mark (pos)
  "Mark at POS."
  (goto-char pos)
  (point-marker))

(defun latex-labeler-mark-region (region)
  "Mark beginning and end points in REGION.
REGION has a form (beg . end)."
  (cons (latex-labeler-mark (car region))
        (latex-labeler-mark (cdr region))))

(defun latex-labeler-mark-subequations (labeldata)
  "Generate marker in a subequations environment.
LABELDATA is an element of a value of
`latex-labeler-find-old-labels'."
  (list (latex-labeler-make-marker-list (caddr labeldata))
        (latex-labeler-mark-region (cadddr labeldata))))

(defun latex-labeler-mark-env (labeldata)
  "Generate marker in a math environment except subequations.
LABELDATA is an element of a value of
`latex-labeler-find-old-labels'."
  (list (latex-labeler-make-marker-list (caddr labeldata))))

(defun latex-labeler-generate-marker-in-env (labeldata)
  "Make markers in a math environment.
LABELDATA is an element of a value of
`latex-labeler-find-old-labels'."
  (if (string= "subequations" (cadr labeldata))
      (latex-labeler-mark-subequations labeldata)
    (latex-labeler-mark-env labeldata)))

(defun latex-labeler-generate-marker-in-region (region)
  "Generate marker in REGION.
REGION has a form (beg . end)."
  (mapcar #'latex-labeler-generate-marker-in-env
          (latex-labeler-find-old-labels region)))

(defun latex-labeler-counter-to-string (counter)
  "Return string of COUNTER.
COUNTER is a number or one string."
  (when (numberp counter)
    (setq counter (number-to-string counter)))
  counter)

(defun latex-labeler-1+ (counter)
  "Increment COUNTER by one.
COUNTER is a number or a string.  If COUNTER is a number,
increment it by 1. If COUNTER is a string, convert it to the next
character."
  (if (numberp counter)
      (setq counter (1+ counter))
    (setq counter (char-to-string (1+ (string-to-char counter))))))

(defun latex-labeler-1- (counter)
  "Decrement COUNTER by one.
COUNTER is a number or a string.  If COUNTER is a number,
decrement it by 1. If COUNTER is a string, convert it to the
previous character."
  (if (numberp counter)
      (setq counter (1- counter))
    (setq counter (char-to-string (1- (string-to-char counter))))))

(defun latex-labeler-replaced-label-regexp (force)
  "Construct a regular expression to match auto-inserted labels.
If FORCE is non-nil, it will match any label, regardless of its
format.  If FORCE is nil, it will match labels following the
specific format defined by the `latex-labeler-prefix`."
  (if force
      "[^{}]*"
    (concat "^" latex-labeler-prefix latex-labeler-prefix-separator
            "[0-9]+\\(" latex-labeler-subprefix-separator "[a-z]\\)?$")))

(defun latex-labeler-replace-old-label (marker label regexp prefix counter
                                               changelist)
  "Replace an old label with a new one.
This function replaces an old label, represented by LABEL.  The
replacement occurs at the position indicated by the MARKER.  If
LABEL matches the format defined by REGEXP, it is replaced with a
new label constructed using PREFIX and COUNTER.  Additionally,
this function updates the CHANGELIST to keep a record of the
label replacements."
  (when (string-match regexp label)
    (let ((newlabel (concat prefix (latex-labeler-counter-to-string counter))))
      (unless (string= label newlabel)
        (progn
          (goto-char marker)
          (looking-at label)
          (replace-match newlabel t)
          (push (cons label newlabel) changelist)))))
  changelist)

(defun latex-labeler-insert-new-label (marker prefix counter)
  "Insert a new label at a position indicated by MARKER.
The new label is constructed using PREFIX and COUNTER."
  (goto-char marker)
  (skip-chars-backward " \t\n")
  (insert latex-labeler-string-before-label)
  (insert (concat "\\label{"
                  prefix (latex-labeler-counter-to-string counter) "}"))
  (when latex-labeler-label-with-indent
    (indent-according-to-mode))
  (insert latex-labeler-string-after-label))

(defun latex-labeler-replace-labels-in-env
    (marker-data regexp prefix counter changelist)
  "Replace labels within a math environment.
MARKER-DATA is an element of the list generated by
`latex-labeler-generate-marker-in-region'.  It contains markers
and old labels within a specific math environment.  REGEXP is a
regular expression used to match old labels.  The new label is
constructed using PREFIX and COUNTER.  CHANGELIST is a list of
label replacements, where each element has a form (old-label
. new-label)."
  (let ((marker-data-in-env (car marker-data))
        (subeq-labelable-region (cadr marker-data)))
    (dolist (marker-label marker-data-in-env)
      (let ((marker (car marker-label))
            (label (cadr marker-label)))
        (if label
            (setq changelist
                  (latex-labeler-replace-old-label
                   marker label regexp prefix counter changelist))
          (latex-labeler-insert-new-label marker prefix counter)))
      (setq counter (latex-labeler-1+ counter)))
    (when subeq-labelable-region
      (let* ((subprefix (concat prefix
                                (latex-labeler-counter-to-string
                                 (latex-labeler-1- counter))
                                latex-labeler-subprefix-separator))
             (subcounter latex-labeler-initial-subcounter)
             (subchangelist (latex-labeler-insert-labels
                             subeq-labelable-region
                             regexp subprefix subcounter nil)))
        (setq changelist (append subchangelist changelist))))
    (cons changelist counter)))

(defun latex-labeler-insert-labels (region regexp prefix counter changelist)
  "Insert labels in REGION.
REGEXP is a regular expression used to match old labels.  The new
label is constructed using PREFIX and COUNTER.  CHANGELIST is a
list of label replacements, where each element has a
form (old-label . new-label)."
  (let ((marker-data (latex-labeler-generate-marker-in-region region)))
    (dolist (marker-data-in-env marker-data changelist)
      (let ((result (latex-labeler-replace-labels-in-env
                     marker-data-in-env regexp prefix counter changelist)))
        (setq counter (cdr result))
        (setq changelist (car result))))))

(defun latex-labeler-update-ref (changelist)
  "Update referrence.
Each element of CHANGELIST has a form (old-label . new-label)."
  (goto-char (point-min))
  (while (latex-labeler-re-search
          (concat "\\\\\\(" (mapconcat #'identity latex-labeler-refs "\\|")
                  "\\){\\([^{}]*\\)}")
          nil t)
    (let ((newlabel (cdr (assoc (match-string-no-properties 2) changelist))))
      (when newlabel
        (replace-match newlabel t nil nil 2)))))

(defun latex-labeler-find-local-variables-region ()
  "Find a local variables region in the current buffer.
Return a list (region bol eol) if found, nil otherwise.  REGION
represents the boundaries of the local variables region.  BOL is
the beginning of line string, and EOL is the end of line string
found within the local variables region."
  (let ((case-fold-search t)
        (beg nil)
        (end nil)
        (bol nil)
        (eol nil))
    (goto-char (point-min))
    (when (re-search-forward
           "\\(^[ \t]*%+[ \t]*\\)local\  variables:[ \t]*\\(.*\\)" nil t)
      (setq beg (match-end 0))
      (setq bol (match-string-no-properties 1))
      (setq eol (match-string-no-properties 2))
      (when (re-search-forward (concat bol "end:[ \t]*" eol) nil t)
        (setq end (match-beginning 0))))
    (when (and beg end)
      (list (cons beg end) bol eol))))

(defun latex-labeler-insert-prefix-setting (region-data prefix)
  "Insert or update prefix setting with PREFIX.
REGION-DATA is a value of
`latex-labeler-find-local-variables-region'."
  (if region-data
      (let ((region (car region-data))
            (bol (cadr region-data))
            (eol (caddr region-data)))
        (goto-char (car region))
        (if (re-search-forward
             (concat bol "[ \t]*latex-labeler-prefix:[ \t]*\"\\(.*\\)\"[ \t]*" eol)
             (cdr region) t)
            (replace-match prefix t nil nil 1)
          (progn (goto-char (cdr region))
                 (insert (concat bol
                                 "latex-labeler-prefix: \"" prefix "\" "
                                 eol "\n")))))
    (progn (goto-char (point-max))
           (insert (concat "\n% local\  variables:\n% latex-labeler-prefix: \""
                           prefix "\"\n% end:")))))

;;; main
(defun latex-labeler-main (prefix force)
  "Update equation labels and corresponding referrences.
PREFIX is a string of the labels prefix.  If FORCE is non-nil,
update all modified labels.  If FORCE is nil, update labels
matching specific format."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search nil)
            (region (cons (point-min) (point-max)))
            (counter latex-labeler-initial-equation-number)
            (regexp (latex-labeler-replaced-label-regexp force))
            (changelist nil))
        (latex-labeler-signal-label-duplication)
        (latex-labeler-update-ref
         (latex-labeler-insert-labels
          region regexp prefix counter changelist)))
      (when (and latex-labeler-update-reftex
                 (bound-and-true-p reftex-mode))
        (reftex-parse-one)))))

(defun latex-labeler-update ()
  "Update labels and references that match the predefined format."
  (interactive)
  (let ((prefix (concat latex-labeler-prefix
                        latex-labeler-prefix-separator)))
    (latex-labeler-main prefix nil)))

(defun latex-labeler-update-force ()
  "Forcefully update all labels and references, regardless of the format."
  (interactive)
  (let ((prefix (concat latex-labeler-prefix
                        latex-labeler-prefix-separator)))
    (latex-labeler-main prefix t)))

(defun latex-labeler-change-prefix-and-update (newprefix)
  "Change the label prefix to NEWPREFIX interactively and update labels."
  (interactive (list (read-string
                      (concat "Enter a new prefix (current prefix: \""
                              latex-labeler-prefix "\"): "))))
  (latex-labeler-main (concat newprefix
                              latex-labeler-prefix-separator)
                      nil)
  (setq-local latex-labeler-prefix newprefix)
  (when latex-labeler-preserve-local-prefix
    (save-excursion
      (save-restriction
        (widen)
        (latex-labeler-insert-prefix-setting
         (latex-labeler-find-local-variables-region) newprefix)))))

(provide 'latex-labeler)
;;; latex-labeler.el ends here
