;;; tblfn.el --- Simple data table functions for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: lisp, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple library for processing data tables represented as
;; lists of lists in Emacs Lisp. I created it to allow occasional
;; aggregation tasks to be written with straightforward syntax and a
;; low learning curve.

;;; Code:

(require 'cl-lib)


;;;; Macros

;;;;; Pipeline Macro

(defmacro tblfn-process (table &rest processes)
  "Return a list with processing specified by PROCESSES applied to TABLE.

PROCESSES is a list specifying what processing to perform.

Each element of PROCESSES represents one processing step and is a list
in the form (PROCNAME . ARGS).

One processing step is executed by calling (tblfn-PROCNAME table ARGS...).
That is, it expands to code like this:

  (let* ((tbl TABLE)
         (tbl (tblfn-PROCNAME0 tbl ARGS0...))
         (tbl (tblfn-PROCNAME1 tbl ARGS1...))
         ...)
    tbl)

This is also equivalent to the following code:

  (thread-first
    TABLE
    (tblfn-FUNAME0 ARGS0...)
    (tblfn-FUNAME1 ARGS1...)
    ...)

This style is longer by the length of \"tblfn-\", but eldoc displays
argument descriptions.
However, the argument positions are shifted.

Of course, you can also write it like this:

  (...
    (tblfn-PROCNAME1
      (tblfn-PROCNAME0 TABLE ARGS0...)
      ARGS1...))

This style probably requires the least knowledge of Emacs Lisp.

Each style has its pros and cons, so choose the one you prefer.
I currently mainly use `thread-first'.
Eldoc works (even though the argument positions are shifted), and
writing let* directly is still a bit verbose."
  (declare (indent 1))
  (let ((table-sym (gensym "table")))
    `(let* ((,table-sym ,table)
            ,@(cl-loop for (funname . args) in processes
                       collect
                       `(,table-sym (,(intern (format "tblfn-%s" funname))
                                     ,table-sym ,@args))))
       ,table-sym)))

;;;;; Argument List Macro

(eval-and-compile
  (defun tblfn--parse-arglist (arglist)
    (let (sections
          (curr (list nil)))
      (dolist (x arglist)
        (when (memq x '(&rest &key))
          (push (nreverse curr) sections)
          (setq curr nil))
        (push x curr))
      (push (nreverse curr) sections)
      (nreverse sections)))

  (defun tblfn--let-args-make-bindings (arglist)
    (let ((arg-sections (tblfn--parse-arglist arglist)))
      (nconc
       (mapcar (lambda (argdef)
                 (if (symbolp argdef)
                     `(,argdef (pop positional-args))
                   `(,(car argdef)
                     (if positional-args (pop positional-args) ,(cadr argdef)))))
               (alist-get nil arg-sections))
       (mapcar (lambda (argdef)
                 `(,argdef positional-args))
               (alist-get '&rest arg-sections))
       (when (null (alist-get '&rest arg-sections))
         '((_ (when positional-args
                (signal 'wrong-number-of-arguments (list nil nil))))))
       (mapcar (lambda (argdef)
                 (if (symbolp argdef)
                     `(,argdef (plist-get
                                keyword-args
                                ,(intern (format ":%s" argdef))))
                   `(,(car argdef)
                     (cadr (or (plist-member
                                keyword-args
                                ,(intern (format ":%s" (car argdef))))
                               '(nil ,(cadr argdef)))))))
               (alist-get '&key arg-sections))))))

(defmacro tblfn--let-args (arglist expr &rest body)
  "Note: Use this macro when optional and rest arguments cannot be
specified with keywords."
  (declare (indent 2) (debug (sexp sexp &rest form)))

  (let ((args-var (gensym "args")))
    `(let* ((,args-var ,expr)
            (keyword-args (cl-member-if #'keywordp ,args-var))
            (positional-args (cl-ldiff ,args-var keyword-args)))
       ;; (message "keyword-args=%s positional-args=%s" keyword-args positional-args)
       (let (,@(tblfn--let-args-make-bindings arglist))
         ,@body))))


;;;; Horizontal Lines (hlines)


(defsubst tblfn-hline-p (row)
  "Return non-nil if ROW is a `hline'."
  (eq row 'hline))

(defun tblfn-hline-count (table)
  "Return the number of hlines in TABLE."
  (seq-count #'tblfn-hline-p table))

(defun tblfn-nth-hline-and-after (table index)
  "Return a cons cell whose car is the Nth hline in TABLE.

INDEX is the hline index number, where the first hline is 0.
Negative numbers represent a relative position from the end of the
table, and -1 represents the last hline.

Return nil if no Nth hline exists.

The returned cons cell points to the hline and all subsequent elements
of TABLE."
  (tblfn-find-nth-element-and-after table 'hline index))
;; TEST: (tblfn-nth-hline-and-after '(("A") hline (0) (1) hline (2) hline (3)) 0) => (hline (0) (1) hline (2) hline (3))
;; TEST: (tblfn-nth-hline-and-after '(("A") hline (0) (1) hline (2) hline (3)) 2) => (hline (3))
;; TEST: (tblfn-nth-hline-and-after '(("A") hline (0) (1) hline (2) hline (3)) 3) => nil
;; TEST: (tblfn-nth-hline-and-after '(("A") (0) (1) (2) (3)) 0) => nil
;; TEST: (tblfn-nth-hline-and-after '(("A") hline (0) (1) hline (2) hline (3)) -1) => (hline (3))
;; TEST: (tblfn-nth-hline-and-after nil 0) => nil

(defun tblfn-last-hline-and-after (table)
  "Return a cons cell whose car is the last hline in TABLE.
Return nil if no hline exists.

Related functions:
  - `tblfn-after-header'
  - `tblfn-footer-hline-and-after'"
  (when table
    (let ((rest table)
          (last-hline-cons-cell nil))
      (while rest
        (when (tblfn-hline-p (car rest))
          (setq last-hline-cons-cell rest))
        (setq rest (cdr rest)))
      last-hline-cons-cell)))
;; TEST: (tblfn-last-hline-and-after '(1 2 3 4)) => nil
;; TEST: (tblfn-last-hline-and-after '(1 2 hline 3 4)) => (hline 3 4)
;; TEST: (tblfn-last-hline-and-after '(1 2 hline 3 4 hline 5)) => (hline 5)

(defun tblfn-between-hlines (table start-hline-index
                                   &optional end-hline-index)
  "Return rows between two hlines in TABLE.

Returns all rows between the hlines specified by START-HLINE-INDEX and
END-HLINE-INDEX as-is.
Non-data rows (hlines and invalid rows) are not removed.
The specified hlines themselves are not included in the result.

START-HLINE-INDEX and END-HLINE-INDEX are index numbers where the first
hline is 0.
When START-HLINE-INDEX is nil, it is treated as 0.
When END-HLINE-INDEX is nil, it is treated as -1.
Negative numbers represent a relative position from the end of the table,
and -1 represents the last hline.

This function cannot return rows before the first hline or after the last
hline."
  (let ((nhlines (tblfn-hline-count table)))
    (unless start-hline-index (setq start-hline-index 0))
    (unless end-hline-index (setq end-hline-index -1))
    (setq start-hline-index (tblfn-normalize-index nhlines start-hline-index nil))
    (setq end-hline-index (tblfn-normalize-index nhlines end-hline-index nil))

    (when (< start-hline-index end-hline-index)
      (tblfn-take-until-cons-cell
       (cdr (tblfn-nth-hline-and-after table start-hline-index))
       (tblfn-nth-hline-and-after table end-hline-index)))))
;; TEST: (tblfn-between-hlines '(1 2 3 4) 1) => error
;; TEST: (tblfn-between-hlines '(1 2 hline 3 4) 0) => nil
;; TEST: (tblfn-between-hlines '(1 2 hline 3 4) 0 0) => nil
;; TEST: (tblfn-between-hlines '(1 hline 2 3 hline 4 5 hline 6 7) 0) => (2 3 hline 4 5)
;; TEST: (tblfn-between-hlines '(1 hline 2 3 hline 4 5 hline 6 7) 0 -1) => (2 3 hline 4 5)
;; TEST: (tblfn-between-hlines '(1 hline 2 3 hline 4 5 hline 6 7) 0 -2) => (2 3)
;; TEST: (tblfn-between-hlines '(1 hline 2 3 hline 4 5 hline 6 7) 0 1) => (2 3)
;; TEST: (tblfn-between-hlines '(1 hline 2 3 hline 4 5 hline 6 7) 1 0) => nil
;; TEST: (tblfn-between-hlines '(1 hline 2 3 hline 4 5 hline 6 7) 0 3) => error

(defun tblfn-before-first-hline (table)
  "Return rows before the first hline in TABLE.

Returns all rows before the first hline.
Non-data rows (invalid rows) are not removed.
The first hline itself is not included in the result.

If TABLE does not contain any hline, returns nil."
  (when-let* ((first-hline (memq 'hline table)))
    (tblfn-take-until-cons-cell table first-hline)))
;; TEST: (tblfn-before-first-hline nil) => nil
;; TEST: (tblfn-before-first-hline '(1 2 3)) => nil
;; TEST: (tblfn-before-first-hline '(hline 1 2 hline 3)) => nil
;; TEST: (tblfn-before-first-hline '(1 2 hline 3)) => (1 2)

(defun tblfn-insert-hline (table row-index)
  "Insert an hline at the position specified by ROW-INDEX in TABLE.

ROW-INDEX is the position of the newly added hline in the returned table.
It is counted from the beginning of TABLE, including hlines and invalid
rows.  When a negative number is specified, it represents a relative
position from the end.
When nil, the new hline is placed at the very end of the returned table."
  (tblfn-insert-nth-row table row-index 'hline))
;; TEST: (tblfn-insert-hline '(("A") hline (0) (1) (2) hline (3)) 3) => (("A") hline (0) hline (1) (2) hline (3))

(defun tblfn-add-hline (table)
  "Return a table with a hline added to the end of TABLE.

Same as (tblfn-add-row TABLE \\='hline).

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode."
  (tblfn-add-row table 'hline))


;;;; Row Types


(defsubst tblfn-non-data-row-p (row)
  "Return non-nil if ROW is a special row that cannot be treated as a
normal computational row."
  (or (tblfn-hline-p row)
      (tblfn-org-invalid-row-p row)))

(defsubst tblfn-data-row-p (row)
  "Return the opposite of `tblfn-non-data-row-p'."
  (not (tblfn-non-data-row-p row)))


;;;; Columns


;;;;; Column Metadata

(defun tblfn-column-count (table)
  "Return the number of columns in TABLE."
  (length (tblfn-column-names table)))

(defun tblfn-column-names (table)
  "Return a list of column names from TABLE."
  ;; TODO: org-modeのとき列名の定義("!")があるならそれを使う？
  ;; 列名の定義は無視して一貫して一行目のみを使うというのも一つの有効
  ;; な方針ではある。
  (when-let* ((first-valid-rows (tblfn-skip-org-invalid-rows table)))
    (when (tblfn-data-row-p (car first-valid-rows))
      (car first-valid-rows))))
;; TEST: (tblfn-column-names '()) => nil
;; TEST: (tblfn-column-names '(hline)) => nil
;; TEST: (tblfn-column-names '(hline ("A" "B" "C"))) => nil
;; TEST: (tblfn-column-names '(("A" "B" "C"))) => ("A" "B" "C")
;; TEST: (tblfn-column-names '(("A" "B" "C") ("1" "2" "3"))) => ("A" "B" "C")
;; TEST: (let ((tblfn-for-org t)) (tblfn-column-names '(("!" "a" "b") (" " "B" "C")))) => (" " "B" "C")

(defun tblfn-column-name (table colspec)
  "Return the column name of the column specified by COLSPEC in TABLE.

See `tblfn-column-index' for COLSPEC."
  (nth (tblfn-column-index table colspec) (tblfn-column-names table)))

(defun tblfn-column-index (table colspec &optional noerror)
  "Return the column index of the column specified by COLSPEC in TABLE.

COLSPEC can be either a column name string or a column index integer.

When COLSPEC is a negative integer, it is treated as if the sum of
TABLE's column count (returned by `tblfn-column-count') and COLSPEC was
specified.

The specified integer value must be a valid column index.

When an invalid name or invalid column index is specified, an error is
signaled.  However, when NOERROR is non-nil, nil is returned instead of
signaling an error."
  ;; TODO: TABLE中に列名の定義("!" "name1" "name2")があるならそれも検索したい？
  (cond
   ((integerp colspec)
    (let ((ncols (tblfn-column-count table)))
      (let ((col-index (if (< colspec 0) (+ ncols colspec) colspec)))
        (if (and (>= col-index 0) (< col-index ncols))
            col-index
          (unless noerror
            (error "Column index `%s' is out of range (number of columns: %s)"
                   colspec ncols))))))
   ((or (stringp colspec) (symbolp colspec))
    (or (seq-position (tblfn-column-names table) colspec)
        (unless noerror
          (error "Column name `%s' not found in table" colspec))))
   (t
    (unless noerror
      (signal 'wrong-type-argument (list 'stringp colspec))))))
;; TEST: (tblfn-column-index '(("a" "b" "c" "d")) "c") => 2
;; TEST: (tblfn-column-index '(("a" "b" "c" "d")) "z") => error
;; TEST: (tblfn-column-index '(("a" "b" "c" "d")) -1) => 3
;; TEST: (tblfn-column-index '(("a" "b" "c" "d")) -5) => error
;; TEST: (tblfn-column-index '(("a" "b" "c" "d")) -5 t) => nil

(defun tblfn-set-all-column-names (table &rest column-names)
  "Return a table with all column names changed to COLUMN-NAMES.

The original TABLE is not modified, but unchanged parts are shared with
the returned list."
  (let ((first-valid-rows (tblfn-skip-org-invalid-rows table)))
    ;; (car first-valid-rows) is one of the following:
    ;; - nil
    ;; - hline
    ;; - data-row
    ;; - unknown element
    (cond
     ((null first-valid-rows)
      (append
       table ;; empty or contains only ("!" ...)
       (if (tblfn-use-hlines-p)
           (list column-names 'hline)
         (list column-names))))
     ((tblfn-data-row-p (car first-valid-rows))
      (nconc
       (tblfn-take-until-cons-cell table first-valid-rows)
       ;; Replace the name row
       (list column-names)
       ;; Keep the hline usage style
       (cdr first-valid-rows)))
     ((tblfn-hline-p (car first-valid-rows))
      (nconc
       (tblfn-take-until-cons-cell table first-valid-rows)
       (list column-names)
       ;; hline and the rest
       first-valid-rows))
     (t
      (nconc
       (tblfn-take-until-cons-cell table first-valid-rows)
       (list column-names)
       ;; Unknown element and the rest
       first-valid-rows)))))
;; TEST: (tblfn-set-all-column-names '(("A" "B" "C" "D") (1 2 3 4)) "AA" "BB" "CC" "DD") => (("AA" "BB" "CC" "DD") (1 2 3 4))
;; TEST: (tblfn-set-all-column-names '(("A" "B" "C" "D") hline (1 2 3 4)) "AA" "BB" "CC" "DD") => (("AA" "BB" "CC" "DD") hline (1 2 3 4))
;; TEST: (tblfn-set-all-column-names nil "AA" "BB" "CC" "DD") => (("AA" "BB" "CC" "DD"))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-set-all-column-names nil "AA" "BB" "CC" "DD")) => (("AA" "BB" "CC" "DD") hline)
;; TEST: (tblfn-set-all-column-names '(hline) "AA" "BB" "CC" "DD") => (("AA" "BB" "CC" "DD") hline)
;; TEST: (let ((tblfn-for-org t)) (tblfn-set-all-column-names '(("!" "a" "b" "c")) "" "AA" "BB" "CC")) => (("!" "a" "b" "c") ("" "AA" "BB" "CC"))
;; TEST: (let ((tblfn-for-org t)) (tblfn-set-all-column-names '(("!" "a" "b" "c") hline) "" "AA" "BB" "CC")) => (("!" "a" "b" "c") ("" "AA" "BB" "CC") hline)
;; TEST: (let ((tblfn-for-org t)) (tblfn-set-all-column-names '(("!" "a" "b" "c") ("" "A" "B" "C") ("" "1" "2" "3")) "" "AA" "BB" "CC")) => (("!" "a" "b" "c") ("" "AA" "BB" "CC") ("" "1" "2" "3"))

(defun tblfn-rename-column (table &rest colspecs-and-new-names)
  "Return a table with column names renamed.

The original TABLE is not modified, but unchanged parts are shared with
the returned list.

COLSPECS-AND-NEW-NAMES specifies alternating column specifiers and new
name strings for the columns to rename.

Example:
  (tblfn-rename-column table \"Count\" \"Product Count\" \"Name\" \"Product\")

If you want to rename all columns, you can also use
`tblfn-set-all-column-names':

  (tblfn-set-all-column-names table \"Product\" \"Product Count\")"
  (or
   (when-let* ((first-valid-rows (tblfn-skip-org-invalid-rows table)))
     (when (tblfn-data-row-p (car first-valid-rows))
       (nconc
        (tblfn-take-until-cons-cell table first-valid-rows)
        ;; Replace name row
        (let ((new-name-row (copy-sequence (car first-valid-rows))))
          (cl-loop for (colspec new-name) on colspecs-and-new-names by #'cddr
                   do
                   (setf (nth (tblfn-column-index table colspec) new-name-row)
                         new-name))
          (list new-name-row))
        ;; After name row
        (cdr first-valid-rows))))
   (if colspecs-and-new-names
       (error "Table does not contain a row with column names")
     table)))
;; TEST: (tblfn-rename-column '(("" "B" "C" "D") hline) "C" "CC" "B" "BB") => (("" "BB" "CC" "D") hline)
;; TEST: (let ((tblfn-for-org t)) (tblfn-rename-column '(("!" "b" "c" "d") ("" "B" "C" "D") hline) "C" "CC" "B" "BB")) => (("!" "b" "c" "d") ("" "BB" "CC" "D") hline)

(defun tblfn--expand-column-references-in-sexp (sexp column-names row-var)
  "Expand column name variable references in SEXP to references to row data.

Column name variables in SEXP are replaced with references to the
corresponding positions in the row data (list of fields).

Additionally, `row-index' is replaced with a reference to
`tblfn-current-row-index'.

ROW-VAR is a symbol representing the variable name that holds the row
data (list of fields).
COLUMN-NAMES is a list of column names."
  (tblfn--expand-symbol-references-in-sexp
   sexp
   `(,@(cl-loop for i from 0
                for name in column-names
                when (stringp name)
                collect `(,(intern name) (nth ,i ,row-var)))
     (row-index tblfn-current-row-index))))

;;;;; Column Structure

(cl-defun tblfn-insert-column (table dst-colspec new-column-name initial-value
                                     &key (padding-value ""))
  "Return a table with a new column inserted.

After insertion, the new column will be at the position specified by
DST-COLSPEC.  After insertion, the column specified by DST-COLSPEC will
shift one position to the right.  When set to nil, the column is
appended as the last column.

NEW-COLUMN-NAME is the name of the new column.

INITIAL-VALUE is the initial value for the new column in all rows."
  (let* ((ncols (tblfn-column-count table))
         (dst-col-index (if (null dst-colspec)
                            ncols
                          (tblfn-column-index table dst-colspec)))
         (name-set nil))
    ;; TODO: Signal error if dst-col-index < 0 ?
    ;; TODO: Signal error if dst-col-index > ncols ?
    (mapcar
     (lambda (row)
       (if (listp row)
           (let ((rest-cols row))
             (nconc
              (cl-loop repeat dst-col-index
                       if rest-cols
                       collect (pop rest-cols)
                       else
                       collect padding-value)
              (list
               ;; TODO: Add header-initial-value?
               ;; TODO: Add footer-initial-value?
               (if (and (not name-set) (tblfn-data-row-p row))
                   (progn
                     (setq name-set t)
                     new-column-name)
                 initial-value))
              rest-cols))
         row))
     table)))
;; TEST: (tblfn-insert-column '(("A" "B" "C") hline (1 2 3) (4 5 6) nil (7)) 2 "X" "-") => (("A" "B" "X" "C") hline (1 2 "-" 3) (4 5 "-" 6) ("" "" "-") (7 "" "-"))
;; TEST: (tblfn-insert-column '(("A" "B" "C") hline (1 2 3) (4 5 6) nil (7)) 2 "X" "-" :padding-value -1) => (("A" "B" "X" "C") hline (1 2 "-" 3) (4 5 "-" 6) (-1 -1 "-") (7 -1 "-"))

(defun tblfn-add-column (table new-column-name initial-value)
  "Return a table with a new column added to the end.

NEW-COLUMN-NAME is the name of the new column.

INITIAL-VALUE is the initial value for the new column in all rows."
  (tblfn-insert-column table nil new-column-name initial-value))

(defun tblfn-append-columns (&rest tables-and-options)
  "Return a table with columns from TABLES concatenated horizontally.

TABLES can be zero or more tables, and is the portion of the argument
list before the first keyword symbol.

Everything from the first keyword symbol onward is treated as keyword
arguments.

The resulting table has all columns from all input tables concatenated
left to right.  The column names are also concatenated in the same order.

If tables have different numbers of rows, shorter tables are padded with
empty strings (or the value specified by :padding-value) to match the
longest table.

If a row in a table has fewer fields than the table's column count, it
is padded with the padding value.  If a row has more fields, only the
first fields up to the column count are used.

Keyword arguments:
  :padding-value VALUE  Value used for padding (default: \"\")

Example:
  (tblfn-append-columns table1 table2 :padding-value 0)
\n(fn &rest TABLES &key (PADDING-VALUE \"\"))"
  (tblfn--let-args (&rest tables &key (padding-value "")) tables-and-options
    (when tables
      (tblfn-add-header-row
       (let ((colcount-list (mapcar #'tblfn-column-count tables))
             (body-list (mapcar #'tblfn-body tables))
             (result nil))
         (while (seq-some #'identity body-list)
           (push (cl-loop
                  for colcount in colcount-list
                  for body in body-list
                  for row = (car body)
                  nconc (tblfn-take-padded row colcount padding-value))
                 result)
           (cl-loop for cons-cell on body-list
                    do (setcar cons-cell (cdr (car cons-cell)))))
         (nreverse result))
       (apply #'append (mapcar #'tblfn-column-names tables))
       (car tables)))))
;; TEST: (tblfn-append-columns) => nil
;; TEST: (tblfn-append-columns '(("A" "B") hline (1 2) (10 11) (12 13) hline (-1 -2))) => (("A" "B") hline (1 2) (10 11) (12 13))
;; TEST: (tblfn-append-columns '(("A" "B") hline (0 1) (10 11 12 13) (20 21) hline (-1 -2)) '(("C" "D" "E") (2 3) (12 13 14) nil) '(("F" "G" "H") nil (15 16 17) (25) (35 36 37))) => (("A" "B" "C" "D" "E" "F" "G" "H") hline (0 1 2 3 "" "" "" "") (10 11 12 13 14 15 16 17) (20 21 "" "" "" 25 "" "") ("" "" "" "" "" 35 36 37))
;; TEST: (tblfn-append-columns '(("A" "B") hline (0 1) (10 11 12 13) (20 21)) '(("C" "D" "E") (2 3) (12 13 14) nil) :padding-value -1) => (("A" "B" "C" "D" "E") hline (0 1 2 3 -1) (10 11 12 13 14) (20 21 -1 -1 -1))

(defun tblfn-remove-columns (table &rest colspecs)
  "Return a new table with the columns specified by COLSPECS removed.

COLSPECS is a list of column specifiers (column names or column indices).

Non-data rows such as hlines are preserved as-is."
  (let ((col-indices (cl-loop for colspec in colspecs
                              collect (tblfn-column-index table colspec))))
    (cl-loop for row in table
             if (consp row)
             collect (cl-loop for field in row
                              for col-index from 0
                              unless (memq col-index col-indices)
                              collect field)
             else
             ;; hline or any other
             collect row)))
;; TEST: (tblfn-remove-columns '(("" "A" "B" "C" "D") ("!" "a" "b" "c" "d") hline (1 2 3 4 5) (6 7 8 9 10) hline (11 12 13 14 15)) "D" "B") => (("" "A" "C") ("!" "a" "c") hline (1 2 4) (6 7 9) hline (11 12 14))

;;;;; Column Operations

(defun tblfn-column-values (table colspec)
  "Return a list of all field values in the column specified by COLSPEC.
Only rows in TABLE's body are included (header and footer rows are excluded)."
  (let ((col-index (tblfn-column-index table colspec))
        (result nil))
    (tblfn-mapc-body-row
     table
     (lambda (row)
       (push (nth col-index row) result)))
    (nreverse result)))
;; TEST: (tblfn-column-values '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (10 11 12)) "B") => (2 5 8)

(defun tblfn-column-sum (table colspec)
  "Return the sum of all values in the column specified by COLSPEC.
Only rows in TABLE's body are included (header and footer rows are excluded)."
  (tblfn-column-vcalc table colspec "vsum"))
;; TEST: (tblfn-column-sum '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (10 11 12)) "C") => "18"

(defun tblfn-column-vcalc (table colspec vfun)
  "Return the result of applying VFUN to the column specified by COLSPEC.

VFUN is a Calc vector function name (string), such as \"vsum\",
\"vmean\", \"vmax\", etc.

Only rows in TABLE's body are included (header and footer rows are excluded)."
  (tblfn-calc-vector-fun (or vfun "vsum")
                         (tblfn-column-values table colspec)))
;; TEST: (tblfn-column-vcalc '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (10 11 12)) "C" "vmean") => "6"

(defun tblfn-select-columns (table &rest colspecs)
  "Return a new table containing only the columns specified by COLSPECS.

COLSPECS is a list of column specifiers (column names or column indices).

The columns in the result appear in the order specified by COLSPECS.
Non-data rows such as hlines are preserved as-is."
  (let ((col-indices (cl-loop for colspec in colspecs
                              collect (tblfn-column-index table colspec))))
    (cl-loop for row in table
             if (consp row)
             collect (cl-loop for col in col-indices collect (nth col row))
             else
             ;; hline or any other
             collect row)))
;; TEST: (tblfn-select-columns '(("" "A" "B" "C" "D") ("!" "a" "b" "c" "d") hline (1 2 3 4 5) (6 7 8 9 10) hline (11 12 13 14 15)) "D" "B") => (("D" "B") ("d" "b") hline (5 3) (10 8) hline (15 13))


;;;; Fields (Cell Access)


(defun tblfn-body-field-at (table colspec row-index &optional noerror)
  "Return the value of the field at COLSPEC and ROW-INDEX in TABLE's body.

COLSPEC is a column specifier (column name or column index).

ROW-INDEX is the position in TABLE's body (0-based, counting only data rows).
Negative values count from the end.

When NOERROR is non-nil, return DEFAULT instead of signaling an error."
  (when-let* ((tri (tblfn-body-row-index-to-table-row-index
                    table row-index noerror)))
    (tblfn-field-at table colspec tri noerror)))
;; TEST: (tblfn-body-field-at '(("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 12) (13 14 15) hline (16 17 18)) "c" -5) => error
;; TEST: (tblfn-body-field-at '(("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 12) (13 14 15) hline (16 17 18)) "c" -2) => 12

(defun tblfn-field-at (table colspec row-index &optional noerror default)
  "Return the value of the field at COLSPEC and ROW-INDEX in TABLE.

COLSPEC is a column specifier (column name or column index).

ROW-INDEX is the position from the beginning of TABLE, counting hlines
and invalid rows.  When a negative number is specified, it represents a
relative position from the end.

When NOERROR is non-nil, return DEFAULT instead of signaling an error."
  (if-let* ((tri (tblfn-normalize-table-row-index
                  table row-index nil noerror)))
      (let ((row (nth tri table)))
        (if (consp row) ;; not hline
            (if-let* ((col-index (tblfn-column-index table colspec noerror)))
                (nth col-index row)
              default)
          (unless noerror
            (error "Row index `%s' is not a field list" row-index))
          default))
    default))
;; TEST: (tblfn-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 1 1) => error
;; TEST: (tblfn-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 1 1 t -1) => -1
;; TEST: (tblfn-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 1 2) => 5
;; TEST: (tblfn-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) -1 -1) => 9
;; TEST: (tblfn-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 0 -1) => 7
;; TEST: (tblfn-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) -1 -100) => error
;; TEST: (tblfn-field-at '(("a" "b" "c") hline (4 5 6) hline (7 8 9)) "a" 4) => 7

(defun tblfn-set-body-field-at (table colspec row-index value
                                      &optional noerror)
  "Return a table with the field at COLSPEC and ROW-INDEX in TABLE's body
set to VALUE.

COLSPEC is a column specifier (column name or column index).

ROW-INDEX is the position in TABLE's body (0-based, counting only data rows).
Negative values count from the end.

This function does not directly modify the contents of TABLE, but the
returned table shares some of its structure with the original TABLE.

When NOERROR is non-nil, return TABLE as-is instead of signaling an error."
  (when-let* ((tri (tblfn-body-row-index-to-table-row-index
                    table row-index noerror)))
    (tblfn-set-field-at table colspec tri value noerror)))
;; TEST: (tblfn-set-body-field-at '(("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 12) (13 14 15) hline (16 17 18)) "c" -5 "X") => error
;; TEST: (tblfn-set-body-field-at '(("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 12) (13 14 15) hline (16 17 18)) "c" -2 "X") => (("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 "X") (13 14 15) hline (16 17 18))

(defun tblfn-set-field-at (table colspec row-index value &optional noerror)
  "Return a table with the field at COLSPEC and ROW-INDEX set to VALUE.

COLSPEC is a column specifier (column name or column index).

ROW-INDEX is the position from the beginning of TABLE, counting hlines and
invalid rows.  When a negative number is specified, it represents a
relative position from the end.

This function does not directly modify the contents of TABLE, but the
returned table shares some of its structure with the original TABLE.

When NOERROR is non-nil, return TABLE as-is instead of signaling an
error."
  (or (when-let* ((tri (tblfn-normalize-table-row-index
                        table row-index nil noerror)))
        (let* ((row-rest (nthcdr tri table))
               (row (car row-rest)))
          (if (consp row) ;; not hline
              (when-let* ((col-index (tblfn-column-index table colspec
                                                         noerror)))
                (nconc
                 (seq-take table tri)
                 (cons
                  (nconc
                   (seq-take row col-index)
                   (cons value (nthcdr (1+ col-index) row)))
                  (cdr row-rest))))
            (unless noerror
              (error "Row index `%s' is not a data row" row-index)))))
      table))
;; TEST: (tblfn-set-field-at nil 1 1 999) => error
;; TEST: (tblfn-set-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 1 1 999) => error
;; TEST: (tblfn-set-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 1 1 999 t) => ((1 2 3) hline (4 5 6) hline (7 8 9))
;; TEST: (tblfn-set-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) 1 2 66) => ((1 2 3) hline (4 66 6) hline (7 8 9))
;; TEST: (tblfn-set-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) -1 -1 99) => ((1 2 3) hline (4 5 6) hline (7 8 99))
;; TEST: (tblfn-set-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) -1 -1 99) => ((1 2 3) hline (4 5 6) hline (7 8 99))
;; TEST: (tblfn-set-field-at '((1 2 3) hline (4 5 6) hline (7 8 9)) -1 -100 999) => error
;; TEST: (tblfn-set-field-at '(("a" "b" "c") hline (4 5 6) hline (7 8 9)) "a" 4 77) => (("a" "b" "c") hline (4 5 6) hline (77 8 9))


;;;; Sections


;;;;; Header

(defun tblfn-after-header (table)
  "Return TABLE without the header row.

The header is considered to be everything up to the first hline.

If there are no hlines, the first row is considered the header.
If you want to use a table without a header, add one using
`tblfn-add-header-row' before processing.

Hlines and special rows after the first hline are preserved as-is."
  (if-let* ((first-hline-cell (memq 'hline table)))
      (cdr first-hline-cell)
    ;; TODO: tableをそのまま返すようなオプションを追加する？
    (cdr table)))

(defun tblfn-insert-header-hline (table)
  "Insert an hline immediately after the first row of TABLE.

However, if TABLE already contains an hline, it is assumed that a header
already exists and TABLE is returned as-is without modification."
  (if (memq 'hline table)
      table
    (cons (car table) (cons 'hline (cdr table)))))

(defun tblfn-add-header-row (table column-names &optional ref-table)
  "Add a header row with COLUMN-NAMES to TABLE."
  ;; TODO: COLUMN-NAMESが列名の定義("!")ならどうする？
  (if (or (memq 'hline ref-table) (tblfn-use-hlines-p))
      (cons column-names (cons 'hline table))
    (cons column-names table)))

;;;;; Footer

(defun tblfn-footer-hline-and-after (table)
  "Return a cons cell containing the hline that marks the beginning of the
footer in TABLE.
Return nil if no hline marking the beginning of the footer exists.

The hline marking the beginning of the footer refers to the last hline
that appears after excluding the first hline that separates the header.

Generally, if TABLE contains no hlines, it is impossible to determine
whether the last row is a footer or not, as it depends on context, so
this function returns nil."
  (tblfn-last-hline-and-after (tblfn-after-header table)))

(defun tblfn-remove-footer (table)
  "Return a table with the footer removed from TABLE.

When TABLE has two or more hlines, remove the last hline and all
subsequent rows.
When TABLE has one hline, it is considered a separator between header
and body, and TABLE is returned as-is.
When TABLE has no hlines, TABLE is returned as-is."
  (if-let* ((footer (tblfn-footer-hline-and-after table)))
      (tblfn-take-until-cons-cell table footer)
    table))
;; TEST: (tblfn-remove-footer nil) => nil
;; TEST: (tblfn-remove-footer '(("A"))) => (("A"))
;; TEST: (tblfn-remove-footer '(("A") hline (0) (1) hline (9))) => (("A") hline (0) (1))
;; TEST: (tblfn-remove-footer '(("A") hline (0) (1) hline (2) (3) hline (8) (9))) => (("A") hline (0) (1) hline (2) (3))

(defun tblfn-remove-last-row (table)
  "Return TABLE with the last row removed.

If there is an hline immediately before the last row, it is also removed.
However, if the table ends with an hline, only that hline is removed.

If TABLE contains no hlines, this is equivalent to `butlast'."
  (if (memq 'hline table)
      (let ((rest table)
            result)
        (while (and
                ;; 最後の行ではない
                (cdr rest)
                ;; 最後の行(非hline)の直前のhlineではない
                (or (not (tblfn-hline-p (car rest)))
                    (cddr rest)
                    (tblfn-hline-p (cadr rest))))
          (push (car rest) result)
          (setq rest (cdr rest)))
        (nreverse result))
    (butlast table)))
;; TEST: (tblfn-remove-last-row '()) => nil
;; TEST: (tblfn-remove-last-row '(1)) => nil
;; TEST: (tblfn-remove-last-row '(1 2)) => (1)
;; TEST: (tblfn-remove-last-row '(hline)) => nil
;; TEST: (tblfn-remove-last-row '(1 hline 2 3)) => (1 hline 2)
;; TEST: (tblfn-remove-last-row '(1 hline 2 3 hline 4)) => (1 hline 2 3)
;; TEST: (tblfn-remove-last-row '(1 hline 2 3 hline)) => (1 hline 2 3)
;; TEST: (tblfn-remove-last-row '(1 hline 2 3 hline hline)) => (1 hline 2 3 hline)

;;;;; Body

(defvar tblfn-current-row-index 0)

(defun tblfn-mapc-body-row (table function)
  "Apply FUNCTION to each row in TABLE's body.

FUNCTION is a function that takes a row (a list of field values in one
row) as an argument.

When FUNCTION is called, the variable `tblfn-current-row-index' contains
the current row number, starting from 0 at the beginning of the body and
counting only valid data rows.

Headers, footers, hlines, special rows to be ignored for org-mode, and
other non-list elements are ignored, and only data rows in the body are
passed to FUNCTION."
  (when-let* ((table-wo-header (tblfn-after-header table)))
    (let ((rest table-wo-header)
          (last-hline-cons-cell nil)
          (processed table-wo-header)
          (tblfn-current-row-index 0))
      (while rest
        (when (tblfn-hline-p (car rest))
          ;; Process until the current hline
          (while (not (eq processed rest))
            (let ((row (pop processed)))
              (when (tblfn-data-row-p row)
                (funcall function row)
                (cl-incf tblfn-current-row-index))))
          (setq last-hline-cons-cell rest
                processed (cdr rest)))
        (setq rest (cdr rest)))

      ;; There are no hline
      (when (eq processed table-wo-header)
        (dolist (row table-wo-header)
          (when (tblfn-data-row-p row)
            (funcall function row)
            (cl-incf tblfn-current-row-index))))
      ;; Return footer hline
      last-hline-cons-cell)))

(defun tblfn-map-body-row (table function)
  "Apply FUNCTION to each row in TABLE's body and return a list of the
results.

FUNCTION is a function that takes a row (a list of field values in one
row) as an argument. The return value of that function is included in
the list returned by this function.

Headers, footers, hlines, special rows to be ignored for org-mode, and
other non-list elements are ignored, and only data rows in the body are
passed to FUNCTION."
  (let ((result nil))
    (tblfn-mapc-body-row
     table
     (lambda (row)
       (push (funcall function row) result)))
    (nreverse result)))
;; TEST: (tblfn-map-body-row nil (lambda (row) (apply #'+ row))) => nil
;; TEST: (tblfn-map-body-row '(("A" "B") ("a" "b") hline (1 2) (3 4) (5 6) hline (7 8)) (lambda (row) (apply #'+ row))) => (3 7 11)
;; TEST: (tblfn-map-body-row '(("A" "B") ("a" "b") hline (1 2) (3 4) (5 6) hline (7 8)) #'identity) => ((1 2) (3 4) (5 6))

(defun tblfn-body-row-count (table)
  "Return the number of valid data rows in TABLE's body.

The body is the remaining part after removing the header and footer.
Hlines and org-mode special rows are not counted."
  (let ((has-hline nil)
        (count-before-last-hline 0)
        (count 0))
    (dolist (row (tblfn-after-header table))
      (cond
       ((tblfn-hline-p row)
        (cl-incf count-before-last-hline count)
        (setq count 0
              has-hline t))
       ((tblfn-data-row-p row)
        (cl-incf count))))
    (if has-hline count-before-last-hline count)))
;; TEST: (tblfn-body-row-count nil) => 0
;; TEST: (tblfn-body-row-count '(1 hline 2 3)) => 2
;; TEST: (tblfn-body-row-count '(1 hline)) => 0
;; TEST: (tblfn-body-row-count '(1 hline 2 3 4 hline 5 6 7 8 hline 9 10)) => 7
;; TEST: (let ((tblfn-for-org t)) (tblfn-body-row-count '(2 ("!" "a") 3 4 hline 5 ("!" "a") 6 hline 7 8))) => 2

(defun tblfn-body-row-index-to-table-row-index (table row-index
                                                      &optional noerror)
  "Convert ROW-INDEX in TABLE's body to a row index in the entire TABLE.

When NOERROR is non-nil, return nil instead of signaling an error if
ROW-INDEX is out of range."
  (when (< row-index 0)
    ;; Count only when negative
    (cl-incf row-index (tblfn-body-row-count table)))
  (if (>= row-index 0)
      (let* ((found-hline nil)
             (rest (tblfn-after-header table))
             (index-in-table (tblfn-count-between table rest))
             (index-in-body 0))
        (catch 'tblfn-found
          (while rest
            (let ((row (pop rest)))
              (cond
               ((tblfn-data-row-p row)
                (when (= index-in-body row-index)
                  (throw 'tblfn-found
                         (if (or (memq 'hline rest)
                                 (not found-hline))
                             ;; Not in the footer
                             index-in-table
                           ;; In the footer
                           (unless noerror
                             (signal 'args-out-of-range (list row-index)))
                           nil)))
                (cl-incf index-in-body))
               ((tblfn-hline-p row)
                (setq found-hline t))))
            (cl-incf index-in-table))
          (unless noerror
            (signal 'args-out-of-range (list row-index)))
          nil))
    (unless noerror
      (signal 'args-out-of-range (list row-index)))))
;; TEST: (tblfn-body-row-index-to-table-row-index nil 1) => error
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 1 2 3 4) -5) => error
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 1 2 3 4) -4) => 1
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 1 2 3 4) -1) => 4
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 1 2 3 4) 3) => 4
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 1 2 3 4) 4) => error
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 1 2 3 4 hline 5) -5) => error
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 1 2 3 4 hline 5) -4) => 2
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 1 2 3 4 hline 5) -1) => 5
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 1 2 3 4 hline 5) 3) => 5
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 1 2 3 4 hline 5) 4) => error
;; TEST: (let ((tblfn-for-org t)) (tblfn-body-row-index-to-table-row-index '(0 hline 1 ("!") 2 3 4 hline 5) 2)) => 5
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 2 3 4 hline 6 7 8 9 hline 11 12) 1) => 3
;; TEST: (tblfn-body-row-index-to-table-row-index nil 1 t) => nil
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 1 2 3 4) -100 t) => nil
;; TEST: (tblfn-body-row-index-to-table-row-index '(0 hline 1 2 3 4 hline 5) 100 t) => nil

(defun tblfn-body (table &optional force-copy)
  "Return TABLE without column names or footer rows.

All hlines and special rows that should be ignored are removed."
  (let ((table-wo-header (tblfn-after-header table)))
    (if (seq-some #'tblfn-non-data-row-p table-wo-header)
        (let ((last-hline-body 'no-hline)
              (body-lines nil))
          (dolist (row table-wo-header)
            (cond
             ((tblfn-hline-p row)
              (setq last-hline-body body-lines))
             ((tblfn-data-row-p row)
              (push row body-lines))))
          (if (eq last-hline-body 'no-hline)
              (nreverse body-lines)
            (nreverse last-hline-body)))
      (if force-copy
          (copy-sequence table-wo-header)
        table-wo-header))))
;; TEST: (tblfn-body '(1 2 3)) => (2 3)
;; TEST: (tblfn-body '(1 hline 2 3)) => (2 3)
;; TEST: (tblfn-body '(1 hline)) => nil
;; TEST: (tblfn-body '(1 hline 2)) => (2)
;; TEST: (tblfn-body '(1 hline 2 hline 3)) => (2)
;; TEST: (tblfn-body '(1 hline 2 3 hline)) => (2 3)
;; TEST: (tblfn-body '(1 2 3 hline 4)) => (4)
;; TEST: (tblfn-body '(1 2 3 4 hline)) => nil
;; TEST: (tblfn-body '(2 ("!" "a") 3 4 hline)) => nil
;; TEST: (let ((tblfn-for-org t)) (tblfn-body '(2 ("!" "a") 3 4 5 ("!" "a") 6))) => (3 4 5 6)
;; TEST: (let ((tblfn-for-org t)) (tblfn-body '(2 ("!" "a") 3 4 hline 5 ("!" "a") 6))) => (5 6)
;; TEST: (let ((tblfn-for-org t)) (tblfn-body '(2 ("!" "a") 3 4 hline 5 ("!" "a") 6 hline 7 8))) => (5 6)

(defun tblfn-take-body-rows-and-rest (table count)
  "Take the first COUNT rows from TABLE's body.

Returns (TAKEN-ROWS . UNPROCESSED-ROWS).

TAKEN-ROWS is a list containing at most COUNT elements.
Always creates and returns a new list.

UNPROCESSED-ROWS is the remaining part of TABLE that was not used when
taking rows."
  (when-let* ((table-wo-header (tblfn-after-header table)))
    (let* ((result nil)
           (rest table-wo-header)
           (first-hline (memq 'hline rest))
           (next-hline (or first-hline
                           ;; No footer
                           t)))

      (while (and rest next-hline (> count 0))
        (let ((row (pop rest)))
          (cond
           ((tblfn-hline-p row)
            (setq next-hline (memq 'hline rest)))
           ((tblfn-data-row-p row)
            (push row result)
            (cl-decf count)))))

      (cons
       ;; Body only
       (nreverse result)
       ;; Unprocessed rows
       rest))))
;; TEST: (tblfn-take-body-rows-and-rest nil 10) => nil
;; TEST: (tblfn-take-body-rows-and-rest '(("A") hline (0) (1) (2) hline (3) (4) hline (5)) 2) => (((0) (1)) (2) hline (3) (4) hline (5))
;; TEST: (tblfn-take-body-rows-and-rest '(("A") hline (0) (1) (2) hline (3) (4) hline (5)) 3) => (((0) (1) (2)) hline (3) (4) hline (5))
;; TEST: (tblfn-take-body-rows-and-rest '(("A") hline (0) (1) (2) hline (3) (4) hline (5)) 10) => (((0) (1) (2) (3) (4)) (5))
;; TEST: (let ((lst '((0) (1) (2)))) (equal (car (tblfn-take-body-rows-and-rest lst 3)) (cdr lst))) => t
;; TEST: (let ((lst '((0) (1) (2)))) (eq (car (tblfn-take-body-rows-and-rest lst 3)) (cdr lst))) => nil

(defun tblfn-data-rows-before-last-hline (rows)
  "Return data rows before the last hline in ROWS.

If there is no hline, return all data rows.
In that case, if there are no invalid rows, return ROWS as is."
  (let ((result nil)
        (rest rows)
        (processed rows))
    (while rest
      (when (tblfn-hline-p (car rest))
        ;; Process until the current hline
        (while (not (eq processed rest))
          (let ((row (pop processed)))
            (when (tblfn-data-row-p row)
              (push row result))))
        (setq processed (cdr rest)))
      (setq rest (cdr rest)))

    (if (eq processed rows)
        ;; There are no hline
        (if (seq-some #'tblfn-non-data-row-p rows)
            ;; Contains non-data row
            (cl-loop for row in rows
                     when (tblfn-data-row-p row)
                     collect row)
          ;; No hline and data row only
          rows)
      ;; Return the rows before the last hline
      (nreverse result))))
;; TEST: (tblfn-data-rows-before-last-hline nil) => nil
;; TEST: (tblfn-data-rows-before-last-hline '((0))) => ((0))
;; TEST: (let ((rows '((0) (1)))) (eq (tblfn-data-rows-before-last-hline rows) rows)) => t
;; TEST: (tblfn-data-rows-before-last-hline '(hline (0) (1))) => nil
;; TEST: (tblfn-data-rows-before-last-hline '((0) hline (1))) => ((0))
;; TEST: (tblfn-data-rows-before-last-hline '((0) (1) hline)) => ((0) (1))
;; TEST: (let ((tblfn-for-org t)) (tblfn-data-rows-before-last-hline '((0) ("!") (1) (2) hline (3) (4)))) => ((0) (1) (2))
;; TEST: (let ((tblfn-for-org t)) (tblfn-data-rows-before-last-hline '((0) ("!") (1) (2) (3) (4)))) => ((0) (1) (2) (3) (4))


;;;; Row Operations


;;;;; Row Insertion

(defun tblfn-add-body-row (table row)
  "Return a table with ROW added to the end of TABLE's body.

Same as (tblfn-insert-nth-body-row TABLE nil ROW).

The returned table does not include the footer."
  (tblfn-insert-nth-body-row table nil row))
;; TEST: (tblfn-add-body-row '(("A")) '("X")) => (("A") ("X"))
;; TEST: (tblfn-add-body-row '(("A") hline (0) (1) (2)) '("X")) => (("A") hline (0) (1) (2) ("X"))

(defun tblfn-add-row (table &rest rows)
  "Return a table with ROWS added to the end of TABLE.

Same as (append TABLE ROWS).

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode.
To process only the body rows, use `tblfn-add-body-row' instead."
  (append table rows))
;; TEST: (tblfn-add-row nil '("1" "2" "3")) => (("1" "2" "3"))
;; TEST: (tblfn-add-row '(("A") hline) '("1")) => (("A") hline ("1"))

(defun tblfn-insert-nth-body-row (table row-index row)
  "Return a table with ROW inserted at ROW-INDEX in TABLE's body.

ROW-INDEX is the position of ROW in the returned table's body.
When a negative number is specified, it represents a relative position
from the end.
When nil, ROW is placed at the very end of the returned table's body.

The returned table does not include the footer."
  (tblfn-add-header-row
   (let* ((splitted (tblfn-take-body-rows-and-rest
                     table
                     (tblfn-normalize-index
                      (tblfn-body-row-count table) row-index
                      ;; Include the number of rows
                      t)))
          (first-body (car splitted)) ;; Always copy
          (rest (cdr splitted)))
     (nconc first-body
            (list row)
            (tblfn-data-rows-before-last-hline rest)))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-insert-nth-body-row '(("A") hline (0) (1) (2) hline (3)) 0 '("X")) => (("A") hline ("X") (0) (1) (2))
;; TEST: (tblfn-insert-nth-body-row '(("A") hline (0) (1) (2) hline (3)) -1 '("X")) => (("A") hline (0) (1) ("X") (2))
;; TEST: (tblfn-insert-nth-body-row '(("A") hline (0) (1) (2) hline (3)) nil '("X")) => (("A") hline (0) (1) (2) ("X"))
;; TEST: (tblfn-insert-nth-body-row '(("A") hline (0) (1) (2) hline (3)) -3 '("X")) => (("A") hline ("X") (0) (1) (2))
;; TEST: (tblfn-insert-nth-body-row '(("A") hline (0) (1) (2) hline (3)) -4 '("X")) => error
;; TEST: (tblfn-insert-nth-body-row '(("A") hline (0) (1) (2) hline (3)) 2 '("X")) => (("A") hline (0) (1) ("X") (2))

(defun tblfn-insert-nth-row (table row-index row)
  "Return a table with ROW inserted at ROW-INDEX in TABLE.

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode.
To process only the body rows, use `tblfn-insert-nth-body-row' instead.

ROW-INDEX is the position of ROW in the returned table.
It is counted from the beginning of TABLE, including hlines and invalid
rows.  When a negative number is specified, it represents a relative
position from the end.
When nil, ROW is placed at the very end of the returned table."
  (when-let* ((tri (tblfn-normalize-table-row-index table row-index t)))
    (nconc (take tri table)
           (list row)
           (nthcdr tri table))))
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) 0 '("X")) => (("X") ("A") hline (0) (1) (2) hline (3))
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) -1 '("X")) => (("A") hline (0) (1) (2) hline ("X") (3))
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) nil '("X")) => (("A") hline (0) (1) (2) hline (3) ("X"))
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) -7 '("X")) => (("X") ("A") hline (0) (1) (2) hline (3))
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) -8 '("X")) => error
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) 7 '("X")) => (("A") hline (0) (1) (2) hline (3) ("X"))
;; TEST: (tblfn-insert-nth-row '(("A") hline (0) (1) (2) hline (3)) 8 '("X")) => error

;;;;; Row Removal

(defun tblfn-remove-nth-body-row (table &rest row-indices)
  "Return a table with rows at ROW-INDICES removed from TABLE's body.

ROW-INDICES are row numbers from the beginning of the body, not counting
hlines or invalid rows.
Negative numbers represent relative positions from the end of the body.

The returned table does not include the footer."
  ;; Normalize and sort ROW-INDICES
  (setq row-indices
        (sort
         (cl-loop with nrows = nil
                  for i in row-indices
                  for x = (if (< i 0)
                              (+ i
                                 (or
                                  nrows
                                  (setq nrows (tblfn-body-row-count table))))
                            i)
                  when (>= x 0)
                  collect x)
         #'<))

  (tblfn-add-header-row
   (let ((count 0)
         (result nil))
     (tblfn-mapc-body-row table
                          (lambda (row)
                            (while (and row-indices
                                        (< (car row-indices) count))
                              (setq row-indices (cdr row-indices)))
                            (unless (eq count (car row-indices))
                              (push row result))
                            (cl-incf count)))
     (nreverse result))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-remove-nth-body-row '(("A") hline (0) (1) (2) (3) (4) (5) (6)) 4 2)
;; TEST: (tblfn-remove-nth-body-row '(("A") hline (0) (1) (2) (3) (4) (5) (6)) -1 -4 -100 -2 -4) => (("A") hline (0) (1) (2) (4))

(defun tblfn-remove-nth-row (table row-index)
  "Return a table with the row at ROW-INDEX removed from TABLE.

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode.
To process only the body rows, use `tblfn-remove-nth-body-row' instead.

ROW-INDEX is the position of the row to remove from the beginning of
TABLE.
It counts hlines and invalid rows as well.
When a negative number is specified, it represents a relative position
from the end of TABLE.
When -1, the last row is removed."
  (when-let* ((tri (tblfn-normalize-table-row-index table row-index nil)))
    (seq-remove-at-position table tri)))
;; TEST: (tblfn-remove-nth-row nil 0) => error
;; TEST: (tblfn-remove-nth-row '((0)) 0) => nil
;; TEST: (tblfn-remove-nth-row '((0) hline (1) (2) hline (3)) 0) => (hline (1) (2) hline (3))
;; TEST: (tblfn-remove-nth-row '((0) hline (1) (2) hline (3)) 1) => ((0) (1) (2) hline (3))
;; TEST: (tblfn-remove-nth-row '((0) hline (1) (2) hline (3)) -1) => ((0) hline (1) (2) hline)
;; TEST: (tblfn-remove-nth-row '((0) hline (1) (2) hline (3)) -2) => ((0) hline (1) (2) (3))
;; TEST: (tblfn-remove-nth-row '((0) hline (1) (2) hline (3)) -6) => (hline (1) (2) hline (3))
;; TEST: (tblfn-remove-nth-row '((0) hline (1) (2) hline (3)) -7) => error

(defun tblfn-remove-body-rows-between (table start-row-index
                                             &optional end-row-index)
  "Return a table with rows between START-ROW-INDEX and END-ROW-INDEX
removed from TABLE's body.

START-ROW-INDEX and END-ROW-INDEX are row numbers from the beginning of
the body, not counting hlines or invalid rows.
Negative numbers represent relative positions from the end of the body.

The returned table does not include the footer."
  (tblfn-add-header-row
   (let* ((body (tblfn-body table))
          (nrows (length body)))
     (unless start-row-index (setq start-row-index 0))
     (unless end-row-index (setq end-row-index nrows))
     (setq start-row-index (tblfn-normalize-index-clamp nrows start-row-index))
     (setq end-row-index (tblfn-normalize-index-clamp nrows end-row-index))
     (if (< start-row-index end-row-index)
         (nconc
          (tblfn-slice body 0 start-row-index)
          (nthcdr end-row-index body))
       body))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-remove-body-rows-between '(("A") hline (0) (1) (2) (3) (4) (5) (6) (7)) 0 8) => (("A") hline)
;; TEST: (tblfn-remove-body-rows-between '(("A") hline (0) (1) (2) (3) (4) (5) (6) (7)) 0 8) => (("A") hline)
;; TEST: (tblfn-remove-body-rows-between '(("A") hline (0) (1) (2) (3) (4) (5) (6) (7)) 1 -1) => (("A") hline (0) (7))
;; TEST: (tblfn-remove-body-rows-between '(("A") hline (0) (1) (2) (3) (4) (5) (6) (7)) -5 -2) => (("A") hline (0) (1) (2) (6) (7))
;; TEST: (tblfn-remove-body-rows-between '(("A") hline (0) (1) (2) (3) (4) (5) (6) (7)) 4) => (("A") hline (0) (1) (2) (3))

(defun tblfn-remove-if (table condition-sexp-or-colspec
                              &optional value binop)
  "Return a new table with rows matching the condition removed from TABLE's
body.

See `tblfn-make-row-predicate-from-condition-spec' for how to specify
the condition.

The returned table does not include the footer."
  (tblfn-add-header-row
   (let ((pred (tblfn-make-row-predicate-from-condition-spec
                table condition-sexp-or-colspec value binop))
         (result nil))
     (tblfn-mapc-body-row table
                          (lambda (row)
                            (unless (funcall pred row)
                              (push row result))))
     (nreverse result))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-remove-if '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) '(equal Category "Vegetable")) => (("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Beef" "Meat" "800"))

;;;;; Row Iteration

(defun tblfn-map-row (table function)
  "Return a list of results from applying FUNCTION to each row of TABLE.

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode.
To process only the body rows, use `tblfn-map-body-row' or
`tblfn-mapc-body-row' instead.

However, rows that are not lists of fields, such as hlines, are added to
the result list as-is without applying the function."
  (cl-loop for row in table
           collect (if (listp row)
                       (funcall function row)
                     ;; hline or unknown element
                     row)))

;;;;; Row Index Access

(defun tblfn-normalize-table-row-index (table row-index include-end
                                              &optional noerror nrows-precalc)
  (tblfn-normalize-index (or nrows-precalc (length table))
                         row-index include-end noerror))
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -1 nil) => 2
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -1 t) => 2
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) 3 nil) => error
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) 3 nil t) => nil
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) 3 t) => 3
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -3 nil) => 0
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -3 t) => 0
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -4 nil) => error
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -4 nil t) => nil
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -5 t) => error
;; TEST: (tblfn-normalize-table-row-index '(0 1 2) -5 t t) => nil

(defun tblfn-nth-body-row (table row-index &optional noerror)
  "Return the ROW-INDEX-th row in TABLE's body.

ROW-INDEX is the position in TABLE's body (0-based, counting only data rows).
Negative values count from the end.

When NOERROR is non-nil, return nil instead of signaling an error if
ROW-INDEX is out of range."
  (when-let* ((tri (tblfn-body-row-index-to-table-row-index
                    table row-index noerror)))
    (tblfn-nth-row table tri noerror)))
;; TEST: (tblfn-nth-body-row '(1 2 hline 3 4 5 6 hline 7 8) -1) => 6
;; TEST: (tblfn-nth-body-row '(1 2 hline 3 4 5 6 hline 7 8) -10) => error
;; TEST: (tblfn-nth-body-row '(1 2 hline 3 4 5 6 hline 7 8) -10 t) => nil
;; TEST: (tblfn-nth-body-row '(1 2 hline 3 4 5 6 hline 7 8) 0) => 3
;; TEST: (tblfn-nth-body-row '(1 2 hline 3 4 5 6 hline 7 8) 100) => error
;; TEST: (tblfn-nth-body-row '(1 2 hline 3 4 5 6 hline 7 8) 5) => error

(defun tblfn-nth-row (table row-index &optional noerror)
  "Return the ROW-INDEX-th row in TABLE.

ROW-INDEX is the position from the beginning of TABLE, counting hlines
and invalid rows.  Negative values count from the end.

When NOERROR is non-nil, return nil instead of signaling an error if
ROW-INDEX is out of range."
  (when-let* ((tri (tblfn-normalize-table-row-index
                    table row-index nil noerror)))
    (nth tri table)))
;; TEST: (tblfn-nth-row '(1 2 hline 3 4) -1) => 4
;; TEST: (tblfn-nth-row '(1 2 hline 3 4) -10) => error
;; TEST: (tblfn-nth-row '(1 2 hline 3 4) -10 t) => nil
;; TEST: (tblfn-nth-row '(1 2 hline 3 4) 0) => 1
;; TEST: (tblfn-nth-row '(1 2 hline 3 4) 5) => error

(defun tblfn-set-nth-body-row (table row-index new-row &optional noerror)
  "Return a table with the row at ROW-INDEX in TABLE's body set to NEW-ROW.

ROW-INDEX is the position in TABLE's body (0-based, counting only data rows).
Negative values count from the end.

This function does not directly modify the contents of TABLE, but the
returned table shares some of its structure with the original TABLE.

When NOERROR is non-nil, return TABLE as-is instead of signaling an error."
  (when-let* ((tri (tblfn-body-row-index-to-table-row-index
                    table row-index noerror)))
    (tblfn-set-nth-row table tri new-row noerror)))
;; TEST: (tblfn-set-nth-body-row '(("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 12) (13 14 15) hline (16 17 18)) -5 '("X" "Y" "Z")) => error
;; TEST: (tblfn-set-nth-body-row '(("a" "b" "c") hline (4 5 6) (7 8 9) (10 11 12) (13 14 15) hline (16 17 18)) -2 '("X" "Y" "Z")) => (("a" "b" "c") hline (4 5 6) (7 8 9) ("X" "Y" "Z") (13 14 15) hline (16 17 18))

(defun tblfn-set-nth-row (table row-index new-row &optional noerror)
  "Return a table with the row at ROW-INDEX set to NEW-ROW.

ROW-INDEX is the position from the beginning of TABLE, counting hlines and
invalid rows.  When a negative number is specified, it represents a
relative position from the end.

This function does not directly modify the contents of TABLE, but the
returned table shares some of its structure with the original TABLE.

When NOERROR is non-nil, return TABLE as-is instead of signaling an error."
  (or (when-let* ((tri (tblfn-normalize-table-row-index
                        table row-index nil noerror)))
        (let ((row-rest (nthcdr tri table)))
          (nconc
           (seq-take table tri)
           (cons
            new-row
            (cdr row-rest)))))
      table))
;; TEST: (tblfn-set-nth-row '((1 2 3) hline (4 5 6) hline (7 8 9))  1 '(10 11 12)) => ((1 2 3) (10 11 12) (4 5 6) hline (7 8 9))
;; TEST: (tblfn-set-nth-row '((1 2 3) hline (4 5 6) hline (7 8 9))  2 '(10 11 12)) => ((1 2 3) hline (10 11 12) hline (7 8 9))
;; TEST: (tblfn-set-nth-row '((1 2 3) hline (4 5 6) hline (7 8 9))  -1 '(10 11 12)) => ((1 2 3) hline (4 5 6) hline (10 11 12))
;; TEST: (tblfn-set-nth-row '((1 2 3) hline (4 5 6) hline (7 8 9))  100 '(10 11 12)) => error
;; TEST: (tblfn-set-nth-row '((1 2 3) hline (4 5 6) hline (7 8 9))  -100 '(10 11 12)) => error
;; TEST: (tblfn-set-nth-row '(("a" "b" "c") hline (4 5 6) hline (7 8 9)) 4 '(10 11 12)) => (("a" "b" "c") hline (4 5 6) hline (10 11 12))

(defun tblfn-slice-body (table start-body-row-index
                               &optional end-body-row-index)
  "Apply `tblfn-slice' to TABLE's body (rows excluding header, footer, and
non-data rows).

START-BODY-ROW-INDEX and END-BODY-ROW-INDEX are row numbers from the
beginning of the body.  When negative, they represent positions relative
to the end.  Non-data rows such as hlines and org-mode special rows are
excluded from the count.

The returned table does not include the footer."
  (tblfn-add-header-row
   (tblfn-slice (tblfn-body table) start-body-row-index end-body-row-index)
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-slice-body '(("A" "B" "C") hline (1 2 3) hline (4 5 6) (7 8 9) (10 11 12) hline (13 14 15)) 1 -1) => (("A" "B" "C") hline (4 5 6) (7 8 9))

;;;;; Row Counting

(defun tblfn-count-if (table condition-sexp-or-colspec
                             &optional value binop)
  "Return the number of rows in TABLE's body that match the condition.

See `tblfn-make-row-predicate-from-condition-spec' for how to specify
the condition."
  (let ((pred (tblfn-make-row-predicate-from-condition-spec
               table condition-sexp-or-colspec value binop))
        (count 0))
    (tblfn-mapc-body-row table
                         (lambda (row)
                           (when (funcall pred row)
                             (cl-incf count))))
    count))
;; TEST: (tblfn-count-if '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) '(equal Category "Vegetable")) => 2

;;;;; Row Extraction

(defun tblfn-sample (table count)
  "Return a table with COUNT randomly selected rows from TABLE's body.
The order of rows is randomized and does not reflect the original order.

The returned table does not include the footer."
  ;; TODO: Implement reservoir sampling.
  (when (> count 0)
    (tblfn-slice-body (tblfn-shuffle table) (- count))))
;; EXAMPLE: (tblfn-sample '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) hline (100 101 102)) -1) => nil
;; EXAMPLE: (tblfn-sample '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) hline (100 101 102)) 0) => nil
;; EXAMPLE: (tblfn-sample '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) hline (100 101 102)) 2)
;; EXAMPLE: (tblfn-sample '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) hline (100 101 102)) 10) => (("A" "B" "C") hline (7 8 9) (4 5 6) (1 2 3) (10 11 12))

(defun tblfn-filter (table condition-sexp-or-colspec &optional value binop)
  "Extract only rows that match the condition from TABLE.

See `tblfn-make-row-predicate-from-condition-spec' for how to specify
the condition.

The returned table does not include the footer."
  (tblfn-add-header-row
   (let ((pred (tblfn-make-row-predicate-from-condition-spec
                table condition-sexp-or-colspec value binop))
         (result nil))
     (tblfn-mapc-body-row table
                          (lambda (row)
                            (when (funcall pred row)
                              (push row result))))
     (nreverse result))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-filter '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) '(equal Category "Vegetable")) => (("Product" "Category" "Price") ("Tomato" "Vegetable" "200") ("Potato" "Vegetable" "80"))
;; TEST: (tblfn-filter '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) "Category" "Vegetable") => (("Product" "Category" "Price") ("Tomato" "Vegetable" "200") ("Potato" "Vegetable" "80"))
;; TEST: (tblfn-filter '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) '(equal row '("Cheese" "Dairy" "450"))) => (("Product" "Category" "Price") ("Cheese" "Dairy" "450"))

(defun tblfn-make-row-predicate-from-condition-spec
    (table condition-sexp-or-colspec value binop)
  "Create a matching function from a row matching condition specification.

Returns a function that takes a row (list of fields) as an argument and
returns non-nil if it matches.

TABLE is the table to be searched.

CONDITION-SEXP-OR-COLSPEC can be one of the following:
  - An S-expression
  - A column specifier (column name or column index integer)
  - A predicate function for rows

When CONDITION-SEXP-OR-COLSPEC is an S-expression, the expression is
evaluated for each row of TABLE.  When the evaluation result is non-nil,
the row is considered to match the condition.

When the S-expression is evaluated, the following variables are available:
  - Column names: variables with the same name as column names contain
    the corresponding column values
  - `row-index': the current row number
  - `row': the current row (list of fields)

Example: (tblfn-filter table \\='(equal Category \"Vegetable\"))

When CONDITION-SEXP-OR-COLSPEC is a column specifier, the value of the
specified column and VALUE are passed to the function BINOP, and rows
for which it returns non-nil are considered to match the condition.
When BINOP is nil, it is treated as if `equal' was specified.

Example: (tblfn-filter table \"Category\" \"Vegetable\" #\\='equal)

When CONDITION-SEXP-OR-COLSPEC is a function, the entire row (an array
of columns) is passed to that function, and rows for which it returns
non-nil are considered to match the condition."
  (cond
   ((consp condition-sexp-or-colspec)
    (tblfn-make-row-predicate-from-condition-sexp table
                                                  condition-sexp-or-colspec))
   ((stringp condition-sexp-or-colspec)
    (unless binop (setq binop #'equal))
    (unless (functionp binop)
      (signal 'wrong-type-argument (list 'functionp binop)))
    (let ((col (tblfn-column-index table condition-sexp-or-colspec)))
      (lambda (row) (funcall binop (nth col row) value))))
   ((functionp condition-sexp-or-colspec)
    condition-sexp-or-colspec)
   (t
    (error "Invalid condition-sexp-or-colspec in tblfn-filter"))))

(defun tblfn-make-row-predicate-from-condition-sexp (table condition-sexp)
  "See `tblfn-make-row-predicate-from-condition-spec'."
  ;; `(lambda (row)
  ;;    (let (,@(cl-loop for colname in (tblfn-column-names table)
  ;;                     collect (list (intern colname) '(pop row)))
  ;;          (row-index tblfn-current-row-index))
  ;;      ,condition-sexp))
  `(lambda (row)
     ,(tblfn--expand-column-references-in-sexp
       condition-sexp
       (tblfn-column-names table)
       'row)))

(defun tblfn-unique (table)
  "Return a table with duplicate rows removed from TABLE's body.

The returned table does not include the footer."
  (tblfn-add-header-row
   (seq-uniq (tblfn-body table))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-unique '(("A" "B" "C") hline (1 2 3) (4 5 6) (1 2 3) (7 8 9) (4 5 6) hline (4 5 6))) => (("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9))

;;;;; Row Ordering

(defun tblfn-sort (table key-colspec &optional order)
  "Sort TABLE.

KEY-COLSPEC specifies the column to use as the sorting key.

ORDER specifies how to compare column values and can be one of:
  - nil: Sort in ascending order.
  - t or `desc': Sort in descending (reverse) order.
  - A function (COL1 COL2): A function that returns non-nil if COL1
    should come before COL2.

The returned table does not include the footer."
  ;; TODO: KEY-COLSPECの代わりに行をまるごと比較する関数を指定できるようにする？
  ;; TODO: 複数の列を指定出来るようにする？
  (let* ((col (tblfn-column-index table key-colspec))
         (col-order-fun
          (pcase order
            ((pred functionp)
             order)
            ;; TODO: タイムスタンプや角度など様々なものを比較できるようにする。
            ((or 't 'desc)
             (lambda (a b) (> (tblfn-to-number a) (tblfn-to-number b))))
            (_
             (lambda (a b) (< (tblfn-to-number a) (tblfn-to-number b))))))
         (row-order-fun
          (lambda (row-a row-b)
            (funcall col-order-fun (nth col row-a) (nth col row-b)))))
    (tblfn-add-header-row
     (seq-sort row-order-fun
               (tblfn-body table))
     (tblfn-column-names table)
     table)))
;; TEST: (tblfn-sort '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) "Price") => (("Product" "Category" "Price") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Beef" "Meat" "800"))
;; TEST: (tblfn-sort '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) "Price" 'desc) => (("Product" "Category" "Price") ("Beef" "Meat" "800") ("Cheese" "Dairy" "450") ("Tomato" "Vegetable" "200") ("Apple" "Fruit" "150") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80"))
;; TEST: (tblfn-sort '(("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800")) "Price" #'value<) => (("Product" "Category" "Price") ("Apple" "Fruit" "150") ("Tomato" "Vegetable" "200") ("Cheese" "Dairy" "450") ("Banana" "Fruit" "80") ("Potato" "Vegetable" "80") ("Beef" "Meat" "800"))

(defun tblfn-reverse (table)
  "Return a table with all rows in TABLE's body reversed in order.

The returned table does not include the footer."
  (tblfn-add-header-row
   (let ((result nil))
     (tblfn-mapc-body-row table (lambda (row) (push row result)))
     result)
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-reverse '(("A"))) => (("A"))
;; TEST: (tblfn-reverse '(("A") (0) (1) (2) (3) (4))) => (("A") (4) (3) (2) (1) (0))
;; TEST: (tblfn-reverse '(("A") hline)) => (("A") hline)
;; TEST: (tblfn-reverse '(("A") hline hline (0))) => (("A") hline)
;; TEST: (tblfn-reverse '(("A") hline (0) (1) (2) (3) (4))) => (("A") hline (4) (3) (2) (1) (0))
;; TEST: (tblfn-reverse '(("A") hline (0) (1) (2) (3) (4) hline (10))) => (("A") hline (4) (3) (2) (1) (0))

;; TODO: defun tblfn-update-body-column (table condition-sexp colspec value-or-function-with-row)
;; TODO: defun tblfn-update-nth-row (table row-index function-with-row-or-colspec-change-specs)

(defun tblfn-shuffle (table)
  "Return a table with the rows in TABLE's body randomly shuffled.

The returned table does not include the footer."
  (tblfn-add-header-row
   (let* ((body (copy-sequence (tblfn-body table)))
          (rest body)
          (rest-len (length rest)))
     (while (cdr rest)
       (cl-rotatef (car rest) (nth (random rest-len) rest))
       (setq rest (cdr rest)
             rest-len (1- rest-len)))
     body)
   (tblfn-column-names table)
   table))
;; EXAMPLE: (tblfn-shuffle '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) hline (100 101 102)))

;;;; Table Transformation

(defun tblfn-update (table row-condition-sexp &rest row-transformer-spec)
  "Return a table with rows in TABLE's body matching ROW-CONDITION-SEXP
transformed.

The returned table does not include the footer.

ROW-CONDITION-SEXP is an S-expression evaluated for each row. Rows for
which it evaluates to non-nil are transformed according to
ROW-TRANSFORMER-SPEC.

Within ROW-CONDITION-SEXP, column values can be referenced by variables
with the same name as column names, and `row-index' contains the current
row number (0-indexed from the beginning of the body).

ROW-TRANSFORMER-SPEC can be one of:

1. A single function that takes a row and returns a transformed row:
   FUNCTION

2. A single S-expression that can reference column names, `row-index',
   and `row' and returns a transformed row.

3. One or more column specifications and change specifications:
   COLSPEC CHANGESPEC [COLSPEC CHANGESPEC]...

   Each CHANGESPEC can be:
   - A value (string or number) to set directly
   - A function that takes the old column value and returns a new value
   - An S-expression that can reference column names, `row-index', and
     `row'

Examples:
  ;; Set column B to \"bb\" in all rows
  (tblfn-update table t \"B\" \"bb\")

  ;; Double column C values where A equals 4
  (tblfn-update table \\='(equal A 4) \"C\" (lambda (c) (* 2 (tblfn-to-number c))))

  ;; Set C to sum of A and B
  (tblfn-update table t \"C\" \\='(+ A B))

  ;; Transform entire row with custom function
  (tblfn-update table \\='(> row-index 5)
                (lambda (row) (mapcar #\\='upcase row)))"
  (tblfn-add-header-row
   (let ((row-pred (tblfn-make-row-predicate-from-condition-sexp
                    table row-condition-sexp))
         (row-transformer (tblfn-make-row-transformer
                           table row-transformer-spec))
         (result nil))
     (tblfn-mapc-body-row table
                          (lambda (row)
                            (push
                             (if (funcall row-pred row)
                                 (funcall row-transformer row)
                               row)
                             result)))
     (nreverse result))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) t "B" "bb") => (("A" "B" "C") hline (1 "bb" 3) (4 "bb" 6) (7 "bb" 9))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) t "C" (lambda (col) (* 2 col))) => (("A" "B" "C") hline (1 2 6) (4 5 12) (7 8 18))
;; TEST: (tblfn-update '(("A A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) t "C" '(+ A\ A B)) => (("A A" "B" "C") hline (1 2 3) (4 5 9) (7 8 15))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) '(equal A 4) "C" 666) => (("A" "B" "C") hline (1 2 3) (4 5 666) (7 8 9))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) '(equal row-index 1) "B" "bb") => (("A" "B" "C") hline (1 2 3) (4 "bb" 6) (7 8 9))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) '(equal row-index 1) (lambda (row) (mapcar (lambda (x) (+ x (* 10 tblfn-current-row-index))) row))) => (("A" "B" "C") hline (1 2 3) (14 15 16) (7 8 9))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) t '(append (cdr row) (list (car row)))) => (("A" "B" "C") hline (2 3 1) (5 6 4) (8 9 7))
;; TEST: (tblfn-update '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) hline (-1 -2 -3)) t "C" '(apply #'+ row)) => (("A" "B" "C") hline (1 2 6) (4 5 15) (7 8 24))
;; TEST: (tblfn-update '(("A" "B" "C") hline ("1,000" "2,000" "3,000") ("4,000" "5,000" "6,000") ("7,000" "8,000" "9,000") hline ("-1" "-2" "-3")) t "C" #'tblfn-to-number) => (("A" "B" "C") hline ("1,000" "2,000" 3000) ("4,000" "5,000" 6000) ("7,000" "8,000" 9000))

(defun tblfn-make-row-transformer (table row-transformer-spec)
  (cond
   ;; ( FUNCTION )
   ((functionp (car row-transformer-spec))
    (unless (cdr row-transformer-spec)
      (car row-transformer-spec)))
   ;; ( SEXP )
   ((consp (car row-transformer-spec))
    (unless (cdr row-transformer-spec)
      (let ((sexp (car row-transformer-spec)))
        `(lambda (row)
           (setq row (copy-sequence row))
           ,(tblfn--expand-column-references-in-sexp
             sexp (tblfn-column-names table) 'row)))))
   ;; ( COLSPEC CHANGESPEC [ COLSPEC CHANGESPEC ]... )
   (t
    (let ((col-change-specs row-transformer-spec)
          (column-names (tblfn-column-names table)))
      `(lambda (row)
         (setq row (copy-sequence row))
         ,@(let (col-exps)
             (while (cdr col-change-specs)
               (let* ((colspec (pop col-change-specs))
                      (changespec (pop col-change-specs))
                      (col-index (tblfn-column-index table colspec))
                      (change-sexp
                       ;; CHANGESPEC
                       (pcase changespec
                         ;; FUNCTION(COL)
                         ((and (pred functionp) fun)
                          `(setf (nth ,col-index row)
                                 (funcall (function ,fun) (nth ,col-index row))))
                         ;; STRING or NUMBER
                         ((and (or (pred stringp) (pred numberp)) value)
                          `(setf (nth ,col-index row)
                                 ,value))
                         ;; SEXP
                         ((and (pred consp) sexp)
                          `(setf (nth ,col-index row)
                                 ,(tblfn--expand-column-references-in-sexp
                                   sexp column-names 'row))))))
                 (when change-sexp
                   (push change-sexp col-exps))))
             col-exps)
         row)))))

(defun tblfn-map-fields (table field-function)
  "Return a table with FIELD-FUNCTION applied to all fields in TABLE.

FIELD-FUNCTION is a function that takes a field value as an argument.
The returned value becomes the new field value.

The header portion of TABLE is not included in processing.
The body and footer are included in processing.
Non-data rows are not included in processing.
Field values not included in processing are preserved as-is."
  (tblfn-add-header-row
   (mapcar (lambda (row)
             (if (tblfn-data-row-p row)
                 (mapcar field-function row)
               row))
           (tblfn-after-header table))
   (tblfn-column-names table)
   table))
;; TEST: (tblfn-map-fields '(("1" "2") ("1" "2") ("3" "4") ("5" "6") ("A" "B")) #'tblfn-to-number-if-possible) => (("1" "2") (1 2) (3 4) (5 6) ("A" "B"))
;; TEST: (tblfn-map-fields '(("1" "2") (3 4) hline ("1" "2") ("3" "4") ("5" "6") ("A" "B") hline ("7" "8")) #'tblfn-to-number-if-possible) => (("1" "2") hline (1 2) (3 4) (5 6) ("A" "B") hline (7 8))

(defun tblfn-numberize (table)
  "Convert all fields (except header) in TABLE to numbers where possible.

Same as (tblfn-map-fields table #\\='tblfn-to-number-if-possible)."
  (tblfn-map-fields table #'tblfn-to-number-if-possible))


;;;; Multi-Table Operations


;;;;; Row Concatenation

(defun tblfn-append-body-rows (&rest tables)
  "Return a table with the bodies of TABLES concatenated.

The column names of the resulting table are taken from the first table.

Column name or column count matching is not considered.

The returned table does not include the footer."
  (when tables
    (tblfn-add-header-row
     (let ((result-rows nil)
           (rest-tables tables))
       (while (cdr rest-tables)
         (setq result-rows (nconc result-rows
                                  ;; Copy
                                  (tblfn-body (pop rest-tables) t))))
       (nconc result-rows (tblfn-body (car rest-tables))))
     (tblfn-column-names (car tables))
     (car tables))))
;; TEST: (tblfn-append-body-rows) => nil
;; TEST: (tblfn-append-body-rows '(("A" "B" "C"))) => (("A" "B" "C"))
;; TEST: (tblfn-append-body-rows '(("A" "B" "C") hline (1 2 3) (4 5 6)) '(("A" "B" "C") hline (7 8 9) (10 11 12) hline (100 101 102))) => (("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12))

(defalias 'tblfn-union-all 'tblfn-append-body-rows)

(defun tblfn-append-rows (&rest tables)
  "Return a table with all rows from TABLES concatenated.

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode.
To process only the body rows, use `tblfn-append-body-rows' instead."
  (apply #'append tables))
;; TEST: (tblfn-append-rows '(("A" "B" "C") hline (1 2 3) (4 5 6)) '(("A" "B" "C") hline (7 8 9) (10 11 12) hline (100 101 102))) => (("A" "B" "C") hline (1 2 3) (4 5 6) ("A" "B" "C") hline (7 8 9) (10 11 12) hline (100 101 102))

(defun tblfn-insert-body-rows-at (target-table target-start source-table
                                               &optional
                                               source-start source-end)
  "Return a table with rows from SOURCE-TABLE's body inserted at TARGET-START in
TARGET-TABLE's body.

The range from SOURCE-START to SOURCE-END in SOURCE-TABLE's body is
inserted at TARGET-START in TARGET-TABLE's body.

TARGET-START is an index from the beginning of TARGET-TABLE's body.

SOURCE-START and SOURCE-END are indices from the beginning of
SOURCE-TABLE's body.

When an index is negative, it represents a relative position from the
end of the table body.

When an index is nil, SOURCE-START represents the beginning of the table
body, and TARGET-END and SOURCE-END represent the end of the table body.

The returned table does not include the footer."
  (let* ((target-body (tblfn-body target-table))
         (source-body (tblfn-body source-table))
         (target-nrows (length target-body))
         (source-nrows (length source-body)))

    (unless target-start (setq target-start target-nrows))
    (unless source-start (setq source-start 0))
    (unless source-end (setq source-end source-nrows))
    (setq target-start (tblfn-normalize-index
                        target-nrows target-start t nil))
    (setq source-start (tblfn-normalize-index
                        source-nrows source-start t nil))
    (setq source-end (tblfn-normalize-index
                      source-nrows source-end t nil))

    (tblfn-add-header-row
     (if (< source-start source-end)
         (nconc
          (seq-take target-body target-start)
          (seq-subseq source-body source-start source-end)
          (nthcdr target-start target-body))
       target-body)
     (tblfn-column-names target-table)
     target-table)))
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14)) => (0 1 2 3 4 5 6 11 12 13 14)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) -1 '(10 11 12 13 14)) => (0 1 2 3 4 5 11 12 13 14 6)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) 2 '(10 11 12 13 14) -1) => (0 1 2 14 3 4 5 6)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) 2 '(10 11 12 13 14)) => (0 1 2 11 12 13 14 3 4 5 6)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 2) => (0 1 2 3 4 5 6 13 14)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 3) => (0 1 2 3 4 5 6 14)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 4) => (0 1 2 3 4 5 6)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 5) => error
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 1 -1) => (0 1 2 3 4 5 6 12 13)
;; TEST: (tblfn-insert-body-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) -1 1) => (0 1 2 3 4 5 6)
;; TEST: (tblfn-insert-body-rows-at '(0 hline 1 2 3 4 5 6 hline 7 8) 1 '(10 hline 11 12 13 14 15 hline 16) 1 -1) => (0 hline 1 12 13 14 2 3 4 5 6)

(defun tblfn-insert-rows-at (target-table target-start source-table
                                          &optional source-start source-end)
  "Return a table with rows from SOURCE-TABLE inserted at TARGET-START in
TARGET-TABLE.

This function processes all rows including headers, footers, and special
rows to be ignored for org-mode.  To process only the body rows, use
`tblfn-insert-body-rows-at' instead.

The range from SOURCE-START to SOURCE-END in SOURCE-TABLE is inserted at
TARGET-START in TARGET-TABLE.

TARGET-START is an index from the beginning of TARGET-TABLE.

SOURCE-START and SOURCE-END are indices from the beginning of SOURCE-TABLE.

When an index is negative, it represents a relative position from the
end of the table.

When an index is nil, SOURCE-START represents the beginning of the
table, and TARGET-END and SOURCE-END represent the end of the table."
  (let ((target-nrows (length target-table))
        (source-nrows (length source-table)))

    (unless target-start (setq target-start target-nrows))
    (unless source-start (setq source-start 0))
    (unless source-end (setq source-end source-nrows))
    (setq target-start (tblfn-normalize-table-row-index
                        target-table target-start t nil target-nrows))
    (setq source-start (tblfn-normalize-table-row-index
                        source-table source-start t nil source-nrows))
    (setq source-end (tblfn-normalize-table-row-index
                      source-table source-end t nil source-nrows))

    (if (< source-start source-end)
        (nconc
         (seq-take target-table target-start)
         (seq-subseq source-table source-start source-end)
         (nthcdr target-start target-table))
      target-table)))
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14)) => (0 1 2 3 4 5 6 10 11 12 13 14)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) -1 '(10 11 12 13 14)) => (0 1 2 3 4 5 10 11 12 13 14 6)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) 2 '(10 11 12 13 14) -1) => (0 1 14 2 3 4 5 6)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) 2 '(10 11 12 13 14)) => (0 1 10 11 12 13 14 2 3 4 5 6)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 2) => (0 1 2 3 4 5 6 12 13 14)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 4) => (0 1 2 3 4 5 6 14)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 5) => (0 1 2 3 4 5 6)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 6) => error
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) 1 -1) => (0 1 2 3 4 5 6 11 12 13)
;; TEST: (tblfn-insert-rows-at '(0 1 2 3 4 5 6) nil '(10 11 12 13 14) -1 1) => (0 1 2 3 4 5 6)

;;;;; Table Joins

(cl-defun tblfn-merge (target-table source-table on-colspec-or-pred
                                    &key (merge-rows-fun nil) )
  "Return a table with rows in TARGET-TABLE's body updated or inserted from
SOURCE-TABLE.

The returned table does not include the footer.

For each row in SOURCE-TABLE's body:
  - If matching row(s) exist in TARGET-TABLE's body, replace all of them
    with the SOURCE-TABLE row (UPDATE)
  - If no matching row exists, append the SOURCE-TABLE row to the result
    (INSERT)

Rows in TARGET-TABLE that have no match in SOURCE-TABLE remain unchanged.

ON-COLSPEC-OR-PRED determines how to match rows:
  - A column specifier (string): Match rows where the specified column
    values are equal in both tables
  - A function: Takes (target-row source-row) and returns non-nil if they
    match

MERGE-ROWS-FUN: Optional function to customize how matching rows are merged.
  - If nil (default), the SOURCE-TABLE row completely replaces the
    TARGET-TABLE row
  - If provided, called with (target-row source-row) and should return
    the merged row
  - Useful for partial updates when SOURCE-TABLE has fewer columns than
    TARGET-TABLE
  - Example: Update only specific columns while preserving others
    (lambda (target source)
      (let ((result (copy-sequence target)))
        (setf (nth 1 result) (nth 1 source))  ; Update column 1 only
        result))

Column names and column counts are not checked for consistency between
tables. The resulting table uses TARGET-TABLE's column names."
  (when-let* ((target-body (copy-sequence (tblfn-body target-table))))
    (let ((pred (cond
                 ;; FUNCTION (target-row source-row) : boolean
                 ((functionp on-colspec-or-pred)
                  on-colspec-or-pred)
                 ;; COLSPEC
                 ((stringp on-colspec-or-pred)
                  (let ((target-col-index (tblfn-column-index
                                           target-table on-colspec-or-pred))
                        (source-col-index (tblfn-column-index
                                           source-table on-colspec-or-pred)))
                    (lambda (target-row source-row)
                      (equal (nth target-col-index target-row)
                             (nth source-col-index source-row)))))
                 (t (error
                     "Invalid on-colspec-or-pred argument in tblfn-merge: %s"
                     on-colspec-or-pred)))))
      (tblfn-mapc-body-row
       source-table
       (lambda (source-row)
         (cl-loop
          with matched = nil
          for target-cons-cell on target-body
          for target-row = (car target-cons-cell)
          do (when (funcall pred target-row source-row)
               ;; Matched
               ;; TODO: Support delete?
               (setcar target-cons-cell
                       (if merge-rows-fun
                           (funcall merge-rows-fun target-row source-row)
                         source-row))
               (setq matched t))
          finally (unless matched
                    ;; Not matched
                    (nconc target-body (list source-row))))))

      (tblfn-add-header-row
       target-body
       (tblfn-column-names target-table)
       target-table))))
;; TEST: (tblfn-merge '(("Product" "Price") hline ("Apple" 300) ("Banana" 200) ("Orange" 100) ("Strawberry" 400)) '(("Product" "New Price") hline ("Orange" 250) ("Apple" 500) ("Strawberry" "1000") ("Melon" 2000)) "Product") => (("Product" "Price") hline ("Apple" 500) ("Banana" 200) ("Orange" 250) ("Strawberry" "1000") ("Melon" 2000))
;; TEST: (tblfn-merge '(("Product" "Price") hline ("Apple" 300) ("Banana" 200) ("Orange" 100) ("Strawberry" 400)) '(("Product" "New Price") hline ("Mango" 750) ("Orange" 250) ("Melon" 2000) ("Apple" 500) ("Strawberry" "1000") ("Melon" 3000)) "Product") => (("Product" "Price") hline ("Apple" 500) ("Banana" 200) ("Orange" 250) ("Strawberry" "1000") ("Mango" 750) ("Melon" 3000))
;; TEST: (tblfn-merge '(("Product" "Price") hline ("Apple" 300) ("Banana" 200) ("Orange" 100) ("Apple" 100) ("Strawberry" 400)) '(("Product" "New Price") hline ("Orange" 250) ("Apple" 500) ("Strawberry" "1000") ("Melon" 2000) ("Orange" 800)) "Product") => (("Product" "Price") hline ("Apple" 500) ("Banana" 200) ("Orange" 800) ("Apple" 500) ("Strawberry" "1000") ("Melon" 2000))
;; TEST: (tblfn-merge nil '(("Product" "New Price") hline ("Orange" 250) ("Apple" 500) ("Strawberry" "1000") ("Melon" 2000) ("Orange" 800)) "Product") => nil
;; TEST: (tblfn-merge '(("Product" "Price") hline ("Apple" 300) ("Banana" 200) ("Orange" 100) ("Apple" 100) ("Strawberry" 400)) nil "Product") => error
;; TEST: (tblfn-merge '(("Product" "Price") hline ("Apple" 300) ("Banana" 200) ("Orange" 100) ("Apple" 100) ("Strawberry" 400)) '(("Product" "Price")) "Product") => (("Product" "Price") hline ("Apple" 300) ("Banana" 200) ("Orange" 100) ("Apple" 100) ("Strawberry" 400))

(defun tblfn-join (left-table right-table left-colspec &rest rest-args)
  "Return a new table with matching rows from RIGHT-TABLE joined to each
row of LEFT-TABLE.

Note: Arguments before the first keyword are treated as optional
arguments, and everything from the first keyword onward is treated as
keyword arguments.

A matching row from RIGHT-TABLE is determined by whether the values in
column LEFT-COLSPEC of LEFT-TABLE and column RIGHT-COLSPEC of
RIGHT-TABLE are equal.

Rows from LEFT-TABLE that have no matching row in RIGHT-TABLE are
removed by default.
However, if INCLUDE-MISMATCH is non-nil, columns with PADDING-VALUE are
concatenated.

By default, the column specified by RIGHT-COLSPEC is removed, but if
KEEP-RIGHT-KEY-COL is non-nil, it is not removed.

When RIGHT-COLSPEC is nil, it is treated as the same column name as
LEFT-COLSPEC.

When RIGHT-SINGLE-ROW is non-nil, at most one row from RIGHT-TABLE
matches each row of LEFT-TABLE.

The returned table does not include the footer.
\n(fn LEFT-TABLE RIGHT-TABLE LEFT-COLSPEC &optional RIGHT-COLSPEC \
&key INCLUDE-MISMATCH KEEP-RIGHT-KEY-COL (PADDING-VALUE \"\") RIGHT-SINGLE-ROW)"
  (tblfn--let-args (right-colspec
                    &key include-mismatch keep-right-key-col
                    (padding-value "") right-single-row)
      rest-args
    (let* ((left-colnames (tblfn-column-names left-table))
           (right-colnames (tblfn-column-names right-table))
           (left-colcount (length left-colnames))
           (left-body (tblfn-body left-table))
           (right-body (tblfn-body right-table))
           (left-key-col (tblfn-column-index left-table left-colspec))
           (right-key-col (tblfn-column-index right-table (or right-colspec
                                                              left-colspec))))
      (tblfn-add-header-row
       (cl-loop for left-row in left-body
                for left-row-padded = (tblfn-take-padded left-row left-colcount
                                                         padding-value)
                for key = (nth left-key-col left-row)
                for pred = (lambda (row) (equal (nth right-key-col row) key))
                for right-rows
                = (if right-single-row
                      (when-let* ((right-row (seq-find pred right-body)))
                        (list right-row))
                    (seq-filter pred right-body))
                if right-rows
                nconc
                (cl-loop for right-row in right-rows
                         collect
                         (append
                          left-row-padded
                          (if keep-right-key-col
                              right-row
                            (seq-remove-at-position right-row right-key-col))))
                else
                if include-mismatch
                collect (append
                         left-row-padded
                         (make-list
                          (if keep-right-key-col
                              (length right-colnames)
                            (1- (length right-colnames)))
                          padding-value)))
       (append left-colnames
               (if keep-right-key-col
                   right-colnames
                 (seq-remove-at-position right-colnames right-key-col)))
       left-table))))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3 4)) '((a c) (1 10) (3 30)) 'a)) => ((a b c) hline (1 2 10) (3 4 30))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3) (5 6)) '((a c) (1 10) (3 30)) 'a)) => ((a b c) hline (1 2 10) (3 "" 30))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3 4) (5 6)) '((a c) (1 10) (3 30)) 'a)) => ((a b c) hline (1 2 10) (3 4 30))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1) (3 4) (5 6)) '((a c) (1 10) (3 30) (1 11) (6 20)) 'a :padding-value -1)) => ((a b c) hline (1 -1 10) (1 -1 11) (3 4 30))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3 4) (5 6)) '((a c) (1 10) (3 30)) 'a :include-mismatch t :keep-right-key-col nil :padding-value -1 t)) => ((a b c) hline (1 2 10) (3 4 30) (5 6 -1))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3 4) (5 6)) '((a c) (1 10) (3 30)) 'a :include-mismatch t :keep-right-key-col t :padding-value  -1 t)) => ((a b a c) hline (1 2 1 10) (3 4 3 30) (5 6 -1 -1))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3 4) (5 6)) '((a c) (1 10) (1 11) (3 30)) 'a :include-mismatch t :keep-right-key-col t :padding-value -1 :right-single-row t)) => ((a b a c) hline (1 2 1 10) (3 4 3 30) (5 6 -1 -1))
;; TEST: (let ((tblfn-use-hlines t)) (tblfn-join '((a b) (1 2) (3 4) (5 6)) '((a c) (1 10) (1 11) (3 30)) 'a :include-mismatch t :keep-right-key-col t :padding-value -1)) => ((a b a c) hline (1 2 1 10) (1 2 1 11) (3 4 3 30) (5 6 -1 -1))

(cl-defun tblfn-cross-join (left-table right-table &key (padding-value ""))
  "Return a new table containing the Cartesian product of LEFT-TABLE and
RIGHT-TABLE.

Each row in LEFT-TABLE is combined with every row in RIGHT-TABLE.

If a row in LEFT-TABLE has fewer fields than its column count, it is
padded with PADDING-VALUE (default: \"\").

The resulting table has all columns from LEFT-TABLE followed by all
columns from RIGHT-TABLE.

The returned table does not include the footer."
  (let* ((left-colnames (tblfn-column-names left-table))
         (right-colnames (tblfn-column-names right-table))
         (left-colcount (length left-colnames))
         (left-body (tblfn-body left-table))
         (right-body (tblfn-body right-table)))
    (tblfn-add-header-row
     (cl-loop for left-row in left-body
              for left-row-padded = (tblfn-take-padded left-row left-colcount
                                                       padding-value)
              nconc
              (cl-loop for right-row in right-body
                       collect (append left-row-padded right-row)))
     (append left-colnames right-colnames)
     left-table)))
;; TEST: (tblfn-cross-join '(("Name" "Price") ("Apple" "150") ("Banana" "80") ("Orange") ("Strawberry" "400")) '(("Name" "Price") ("Chocolate" "10") ("Cake" "20") ("Pudding" "30"))) => (("Name" "Price" "Name" "Price") ("Apple" "150" "Chocolate" "10") ("Apple" "150" "Cake" "20") ("Apple" "150" "Pudding" "30") ("Banana" "80" "Chocolate" "10") ("Banana" "80" "Cake" "20") ("Banana" "80" "Pudding" "30") ("Orange" "" "Chocolate" "10") ("Orange" "" "Cake" "20") ("Orange" "" "Pudding" "30") ("Strawberry" "400" "Chocolate" "10") ("Strawberry" "400" "Cake" "20") ("Strawberry" "400" "Pudding" "30"))

;;;;; Set Operations

(defun tblfn-reduce (initial-table function &rest rest-tables)
  "Reduce the bodies of tables using FUNCTION, starting with INITIAL-TABLE.

FUNCTION is called with two arguments: an accumulated list of rows and the
body of the next table.  It should return a new list of rows.

The column names of the resulting table are taken from INITIAL-TABLE.

Column name or column count matching is not considered.

The returned table does not include the footer.

This function can be used to implement set operations on tables:
  (tblfn-reduce table1 \\='seq-union table2 table3)
  (tblfn-reduce table1 \\='seq-intersection table2 table3)
  (tblfn-reduce table1 \\='seq-difference table2 table3)"
  (tblfn-add-header-row
   (seq-reduce (lambda (rows table2)
                 (funcall function rows (tblfn-body table2)))
               rest-tables
               (tblfn-body initial-table))
   (tblfn-column-names initial-table)
   initial-table))

(defun tblfn-union (&rest tables)
  "Return a table containing the union of all rows in the bodies of TABLES.

The column names of the resulting table are taken from the first table.

Column name or column count matching is not considered.

The returned table does not include the footer."
  (when tables
    (apply #'tblfn-reduce (car tables) #'seq-union (cdr tables))))
;; TEST: (tblfn-union) => nil
;; TEST: (tblfn-union '(("A" "B" "C"))) => (("A" "B" "C"))
;; TEST: (tblfn-union '(("A" "B" "C") hline (1 2 3) hline (101 102 103)) '(("A" "B" "C") hline (4 5 6) (7 8 9) hline (201 202 203)) '(hline (10 11 12) (13 14 15) hline (301 302 303))) => (("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))

(defun tblfn-intersection (&rest tables)
  "Return a table containing the intersection of all rows in the bodies of
TABLES.

The column names of the resulting table are taken from the first table.

Column name or column count matching is not considered.

The returned table does not include the footer."
  (when tables
    (apply #'tblfn-reduce (car tables) #'seq-intersection (cdr tables))))
;; TEST: (tblfn-intersection) => nil
;; TEST: (tblfn-intersection '(("A" "B" "C"))) => (("A" "B" "C"))
;; TEST: (tblfn-intersection '(("A" "B" "C") hline (1 2 3) hline (4 5 6))) => (("A" "B" "C") hline (1 2 3))
;; TEST: (tblfn-intersection '(("A" "B" "C") hline ("1" "2" "3")) '(("1" "2" "2") hline ("A" "B" "C"))) => (("A" "B" "C") hline)
;; TEST: (tblfn-intersection '(("A" "B" "C") hline (1 2 3) hline (101 102 103)) '(("A" "B" "C") hline (4 5 6) (7 8 9) hline (201 202 203)) '(hline (10 11 12) (13 14 15) hline (301 302 303))) => (("A" "B" "C") hline)
;; TEST: (tblfn-intersection '(("A" "B" "C") hline (1 2 3) (7 8 9) (13 14 15) hline (101 102 103)) '(("A" "B" "C") hline (4 5 6) (13 14 15) (7 8 9) hline (201 202 203)) '(hline (10 11 12) (13 14 15) (7 8 9)  hline (301 302 303))) => (("A" "B" "C") hline (7 8 9) (13 14 15))

(defun tblfn-difference (&rest tables)
  "Return a table containing rows in the first table that are not in any
of the remaining TABLES.

The column names of the resulting table are taken from the first table.

Column name or column count matching is not considered.

The returned table does not include the footer."
  (when tables
    (apply #'tblfn-reduce (car tables) #'seq-difference (cdr tables))))
;; TEST: (tblfn-difference) => nil
;; TEST: (tblfn-difference '(("A" "B" "C"))) => (("A" "B" "C"))
;; TEST: (tblfn-difference '(("A" "B" "C") hline ("1" "2" "3")) '(("1" "2" "2") hline ("A" "B" "C"))) => (("A" "B" "C") hline ("1" "2" "3"))
;; TEST: (tblfn-difference '(("A" "B" "C") hline (1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) hline (101 102 103)) '(("A" "B" "C") hline (4 5 6) (7 8 9) hline (201 202 203)) '(hline (10 11 12) (13 14 15) hline (301 302 303))) => (("A" "B" "C") hline (1 2 3) (16 17 18))


;;;; Table Reshaping


(cl-defun tblfn-transpose (table &key
                                 (exclude-header nil)
                                 (exclude-footer nil)
                                 (column-names 'number)
                                 (padding-value ""))
  "Transpose TABLE (swap rows and columns).

EXCLUDE-HEADER: If non-nil, exclude the header row from transposition.
EXCLUDE-FOOTER: If non-nil, exclude the footer rows from transposition.
PADDING-VALUE: Value used to pad short rows (default: \"\").

COLUMN-NAMES determines the column names of the transposed table:
  \\='number (default) - Sequential numbers: \"0\", \"1\", \"2\", ...
  \\='first-column - Use the first column of TABLE as column names
  \\='empty-string - All column names are empty strings
  \\='no-header - Return table without header row
  String (e.g., \"c%d\") - Format string with column index
  Function - Called with column index, returns column name
  List - Specify column names individually:
    - Strings are used as-is
    - Functions are called with column index
    - nil uses default numbering for that position
    - After list is exhausted, uses cdr if it's a string/function,
      otherwise uses default numbering

The returned table does not include the footer.

Examples:
  :column-names \\='number          => (\"0\" \"1\" \"2\" ...)
  :column-names \\='first-column    => Uses first row as headers
  :column-names \"col%d\"          => (\"col0\" \"col1\" \"col2\" ...)
  :column-names \\='(\"A\" \"B\" nil)  => (\"A\" \"B\" \"2\" \"3\" ...)
  :column-names \\='(\"A\" . \"c%d\")  => (\"A\" \"c1\" \"c2\" ...)"
  (let ((result (make-list (tblfn-column-count table) nil))
        (end (if exclude-footer (tblfn-footer-hline-and-after table) nil))
        (rest (if exclude-header (tblfn-after-header table) table)))
    (while (not (eq rest end))
      (let ((row (pop rest)))
        (when (tblfn-data-row-p row)
          (cl-loop for result-rows on result
                   for field = (if row (pop row) padding-value)
                   do (push field (car result-rows))))))
    (cl-loop for result-rows on result
             do (setcar result-rows (nreverse (car result-rows))))

    ;; Complete the column name row
    (when-let* ((result-body-and-column-names
                 (tblfn-transpose--generate-column-names result column-names)))
      (setq result
            (tblfn-add-header-row
             (car result-body-and-column-names)
             (cdr result-body-and-column-names)
             table)))

    result))
;; TEST: (tblfn-transpose '(("A" "B" "C" "D") hline (1 2) (11 12 13 14 15) (21 22 23 24) () (41) (42 43 44 45) hline (101 102 103 104)) :padding-value 0 :column-names 'number) => (("0" "1" "2" "3" "4" "5" "6" "7") hline ("A" 1 11 21 0 41 42 101) ("B" 2 12 22 0 0 43 102) ("C" 0 13 23 0 0 44 103) ("D" 0 14 24 0 0 45 104))
;; TEST: (tblfn-transpose '(("Name" "A" "B" "C" "D") hline ("a" 1 2) ("b" 11 12 13 14 15) ("c" 21 22 23 24) () ("e" 41) ("f" 42 43 44 45) hline ("g" 101 102 103 104)) :exclude-header t :padding-value 0 :column-names 'first-column) => (("a" "b" "c" 0 "e" "f" "g") hline (1 11 21 0 41 42 101) (2 12 22 0 0 43 102) (0 13 23 0 0 44 103) (0 14 24 0 0 45 104))
;; TEST: (tblfn-transpose '(("Name" "A" "B" "C" "D") hline ("a" 1 2) ("b" 11 12 13 14 15) ("c" 21 22 23 24) () ("e" 41) ("f" 42 43 44 45) hline ("g" 101 102 103 104)) :exclude-header t :exclude-footer t :padding-value 0 :column-names 'first-column) => (("a" "b" "c" 0 "e" "f") hline (1 11 21 0 41 42) (2 12 22 0 0 43) (0 13 23 0 0 44) (0 14 24 0 0 45))

(defun tblfn-transpose--generate-column-names (result-table column-names)
  "See `tblfn-transpose'."
  (pcase column-names
    ('number
     (cons result-table
           (cl-loop for i from 0 below (length (car result-table))
                    collect (format "%d" i))))
    ('first-column
     (cons (cdr result-table)
           (car result-table)))
    ('empty-string
     (cons result-table
           (cl-loop for i from 0 below (length (car result-table))
                    collect "")))
    ((pred stringp)
     (cons result-table
           (cl-loop for i from 0 below (length (car result-table))
                    collect (format column-names i))))
    ((pred functionp)
     (cons result-table
           (cl-loop for i from 0 below (length (car result-table))
                    collect (funcall column-names i))))
    ((pred listp)
     (cons result-table
           (cl-loop for i from 0 below (length (car result-table))
                    collect
                    (pcase column-names
                      ((and (pred consp) cell)
                       (pop column-names)
                       (pcase (car cell)
                         ((and (pred stringp) str) str)
                         ((and (pred functionp) fun) (funcall fun i))
                         (_ (format "%d" i))))
                      ((pred null)
                       (format "%d" i))
                      ((and (pred stringp) str)
                       (format str i))
                      ((and (pred functionp) fun)
                       (funcall fun i))))))
    ('no-header nil)
    (_ nil)))
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3 4 5 6) (7 8 9 10 11 12)) nil) => (((1 2 3 4 5 6) (7 8 9 10 11 12)) "0" "1" "2" "3" "4" "5")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3) (4 5 6)) 'number) => (((1 2 3) (4 5 6)) "0" "1" "2")
;; TEST: (tblfn-transpose--generate-column-names '(("A" "B" "C") (1 2 3) (4 5 6)) 'first-column) => (((1 2 3) (4 5 6)) "A" "B" "C")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3) (4 5 6)) 'empty-string) => (((1 2 3) (4 5 6)) "" "" "")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3) (4 5 6)) "c%d") => (((1 2 3) (4 5 6)) "c0" "c1" "c2")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3) (4 5 6)) (lambda (i) (format "v%d" i))) => (((1 2 3) (4 5 6)) "v0" "v1" "v2")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3) (4 5 6)) #'number-to-string) => (((1 2 3) (4 5 6)) "0" "1" "2")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3 4 5 6) (7 8 9 10 11 12)) '("A" number-to-string nil)) => (((1 2 3 4 5 6) (7 8 9 10 11 12)) "A" "1" "2" "3" "4" "5")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3 4 5 6) (7 8 9 10 11 12)) `("A" ,(lambda (i) (format "v%d" i)) nil)) => (((1 2 3 4 5 6) (7 8 9 10 11 12)) "A" "v1" "2" "3" "4" "5")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3 4 5 6) (7 8 9 10 11 12)) `("A" ,(lambda (i) (format "v%d" i)) . "c%d")) => (((1 2 3 4 5 6) (7 8 9 10 11 12)) "A" "v1" "c2" "c3" "c4" "c5")
;; TEST: (tblfn-transpose--generate-column-names '((1 2 3 4 5 6) (7 8 9 10 11 12)) `("A" ,(lambda (i) (format "v%d" i)) . ,(lambda (i) (format "x%d" i)))) => (((1 2 3 4 5 6) (7 8 9 10 11 12)) "A" "v1" "x2" "x3" "x4" "x5")


;;;; Table Aggregation


(defun tblfn-aggregate (table by-colspec-or-function
                              value-colspec-or-function
                              &optional
                              vfun new-value-colname new-key-colname)
  "Return a table with values aggregated by grouping key.

BY-COLSPEC-OR-FUNCTION determines the grouping key:
  - Column specifier: Group by values in that column
  - S-expression: Group by the result of evaluating the expression for
    each row (e.g., \\='(concat Region \"=\" Category))
  - Function: Group by the result of calling the function with each row

VALUE-COLSPEC-OR-FUNCTION determines the values to aggregate:
  - Column specifier: Aggregate values from that column
  - S-expression: Group by the result of evaluating the expression for
    each row (e.g., \\='(* Quantity Price))
  - Function: Aggregate the result of calling the function with each row

The result is a two-column table.  The first column contains unique
grouping keys.  The second column contains aggregated values for rows
with the same grouping key.

VFUN specifies how to aggregate values:

  - Calc vector function name (string, e.g., \"vsum\", \"vmean\"):
      Apply that function
  - Function: Apply that function to the list of values
  - nil (default): Use \"vsum\"

NEW-VALUE-COLNAME specifies the name of the aggregated value column.
When nil, uses the original column name if VALUE-COLSPEC-OR-FUNCTION is
a column specifier, or \"Value\" if it is a function.

NEW-KEY-COLNAME specifies the name of the grouping key column.
When nil, uses the original column name if BY-COLSPEC-OR-FUNCTION is
a column specifier, or \"Key\" if it is a function.

Example:
  (tblfn-aggregate
   \\='((\"Product\" \"Category\" \"Price\")
      (\"Apple\" \"Fruit\" \"150\")
      (\"Tomato\" \"Vegetable\" \"200\")
      (\"Cheese\" \"Dairy\" \"450\")
      (\"Banana\" \"Fruit\" \"80\")
      (\"Potato\" \"Vegetable\" \"80\")
      (\"Beef\" \"Meat\" \"800\"))
   \"Category\" \"Price\" \"vsum\" \"Total\")
  => ((\"Category\" \"Total\")
      (\"Fruit\" \"230\")
      (\"Vegetable\" \"280\")
      (\"Dairy\" \"450\")
      (\"Meat\" \"800\"))"
  (unless new-key-colname
    (setq new-key-colname (tblfn--aggregate-default-column-name
                           table by-colspec-or-function "Key")))
  (unless new-value-colname
    (setq new-value-colname (tblfn--aggregate-default-column-name
                             table value-colspec-or-function "Value")))

  (let ((key-function (tblfn-make-row-to-value-function
                       table by-colspec-or-function))
        (value-function (tblfn-make-row-to-value-function
                         table value-colspec-or-function))
        (alist nil))

    (tblfn-mapc-body-row
     table
     (lambda (row)
       (push (funcall value-function row)
             (alist-get (funcall key-function row) alist nil nil #'equal))))
    (setq alist (nreverse alist))

    (cond
     ((functionp vfun)
      (dolist (kv alist)
        (let ((values (cdr kv)))
          (setcdr kv (cons (funcall vfun values) nil)))))
     (t
      (dolist (kv alist)
        (let* ((values (cdr kv))
               (aggregated (tblfn-calc-vector-fun (or vfun "vsum") values)))
          (setcdr kv (cons aggregated nil))))))

    (tblfn-add-header-row
     alist
     (list new-key-colname new-value-colname)
     table)))
;; TEST: (tblfn-aggregate '(("name" "class" "price") ("apple" "fruits" "12.3") ("onion" "vegetables" "34.1") ("banana" "fruits" "23.4") ("orange" "fruits" "34.5") ("cabbage" "vegetables" "23.1") ("tomato" "vegetables" "45.1")) "class" "price" "vsum") => (("class" "price") ("fruits" "70.2") ("vegetables" "102.3"))
;; TEST: (tblfn-aggregate '(("name" "class" "price") ("apple" "fruits" "150") ("onion" "vegetables" "100") ("banana" "fruits" "300") ("orange" "fruits" "100") ("cabbage" "vegetables" "400") ("tomato" "vegetables" "100")) (lambda (row) (if (>= (tblfn-to-number (nth 2 row)) 200) "High" "Low")) (lambda (row) 1)) => (("Key" "Value") ("Low" "4") ("High" "2"))
;; TEST: (tblfn-aggregate '(("name" "class" "price") ("apple" "fruits" "150") ("onion" "vegetables" "100") ("banana" "fruits" "300") ("orange" "fruits" "100") ("cabbage" "vegetables" "400") ("tomato" "vegetables" "100")) '(if (>= (tblfn-to-number price) 200) "High" "Low") (lambda (row) 1)) => (("Key" "Value") ("Low" "4") ("High" "2"))
;; TEST: (tblfn-aggregate '(("Product" "Category" "Quantity" "Price") ("apple" "fruits" 2 150) ("onion" "vegetables" 3 100) ("banana" "fruits" 1 300) ("orange" "fruits" 10 100) ("cabbage" "vegetables" 1 400) ("tomato" "vegetables" 4 100)) "Category" '(* Quantity Price) nil "Total") => (("Category" "Total") ("fruits" "1600") ("vegetables" "1100"))

(defun tblfn-make-row-to-value-function (table colspec-or-function)
  (cond
   ;; FUNCTION
   ((functionp colspec-or-function)
    colspec-or-function)
   ;; SEXP
   ((consp colspec-or-function)
    (let ((sexp colspec-or-function))
      `(lambda (row)
         (setq row (copy-sequence row))
         ,(tblfn--expand-column-references-in-sexp
           sexp (tblfn-column-names table) 'row))))
   ;; COLSPEC (string or integer)
   (t
    (let ((colspec colspec-or-function))
      (let ((col-index (tblfn-column-index table colspec)))
        (lambda (row) (nth col-index row)))))))

(defun tblfn--aggregate-default-column-name (table colspec-or-function default)
  (cond
   ;; FUNCTION
   ;; SEXP
   ((or (functionp colspec-or-function)
        (consp colspec-or-function))
    default)
   ;; COLSPEC (string or integer)
   (t
    (tblfn-column-name table colspec-or-function))))

(defun tblfn-count-by (table by-colspec-or-function new-count-colname
                             &optional new-key-colname)
  "Return a table with row counts grouped by grouping key.

BY-COLSPEC-OR-FUNCTION determines the grouping key:
  - Column specifier: Group by values in that column
  - Function: Group by the result of calling the function with each row

The resulting table has two columns:
- The grouping column (named by NEW-KEY-COLNAME)
- A count column named NEW-COUNT-COLNAME

Each unique grouping key becomes a row in the result, with its count in
the NEW-COUNT-COLNAME column.

NEW-KEY-COLNAME specifies the name of the grouping key column.
When nil, uses the original column name if BY-COLSPEC-OR-FUNCTION is
a column specifier, or \"Key\" if it is a function.

This is similar to SQL's \"SELECT col, COUNT(*) FROM table GROUP BY col\"."
  (unless new-key-colname
    (setq new-key-colname (tblfn--aggregate-default-column-name
                           table by-colspec-or-function "Key")))
  (let ((key-function (tblfn-make-row-to-value-function
                       table by-colspec-or-function)))
    (tblfn-add-header-row
     (let ((alist nil))
       (tblfn-mapc-body-row
        table
        (lambda (row)
          (let* ((key (funcall key-function row))
                 (cons-cell (or (assoc key alist)
                                (car (push (list key 0) alist)))))
            (cl-incf (cadr cons-cell)))))
       (nreverse alist))
     (list new-key-colname new-count-colname)
     table)))
;; TEST: (tblfn-count-by '(("name" "class") ("apple" "fruits") ("onion" "vegetables") ("cheese" "dairy") ("banana" "fruits") ("orange" "fruits") ("cabbage" "vegetables")) "class" "count") => (("class" "count") ("fruits" 3) ("vegetables" 2) ("dairy" 1))
;; TEST: (tblfn-count-by '(("name" "class" "price") ("apple" "fruits" "150") ("onion" "vegetables" "100") ("banana" "fruits" "300") ("orange" "fruits" "100") ("cabbage" "vegetables" "400") ("tomato" "vegetables" "100")) (lambda (row) (if (>= (tblfn-to-number (nth 2 row)) 200) "High" "Low")) "Count") => (("Key" "Count") ("Low" 4) ("High" 2))


;;;; Table Calculation


(defun tblfn-add-percentage-column (table value-colspec percentage-colname
                                          &optional
                                          change-footer
                                          calc-option-plist)
  "Add a column containing percentages to the right of TABLE.

First, sum all values in the column specified by VALUE-COLSPEC in
TABLE\\='s body.  Then, for each row in TABLE, add a column containing
the value from the VALUE-COLSPEC column converted to a percentage.

The name of the added column is specified by PERCENTAGE-COLNAME.

When CHANGE-FOOTER is non-nil, percentage addition is also applied to
footer rows.  In that case, the column specified by VALUE-COLSPEC in
the footer row must be a valid number.
Even when CHANGE-FOOTER is non-nil, the denominator for percentages
\(the sum of values in the VALUE-COLSPEC column) is limited to the body.

CALC-OPTION-PLIST is a property list holding options to be applied
during calculation.  See the contents of the `calc-do-calc-eval'
function for valid properties.  The default is
\\='(calc-float-format (fix -2)).

When CALC-OPTION-PLIST is a natural number, it is equivalent to
specifying \\='(calc-float-format (fix CALC-OPTION-PLIST)).
When it is 0, the result will be an integer."
  (let ((total (tblfn-column-sum table value-colspec))
        (value-col (tblfn-column-index table value-colspec)))
    (tblfn-add-header-row
     (let ((footer-hline-cons-cell (if change-footer
                                       nil
                                     (tblfn-footer-hline-and-after table)))
           (in-footer nil)
           (rest (tblfn-after-header table))
           (result nil))
       (while rest
         (when (eq rest footer-hline-cons-cell)
           (setq in-footer t))
         (push
          (let ((row (car rest)))
            (cond
             (in-footer
              row)
             ((tblfn-data-row-p row)
              (append row
                      (list (tblfn-add-percentage-column--calc
                             (nth value-col row) total
                             calc-option-plist))))
             (t
              row)))
          result)
         (setq rest (cdr rest)))
       (nreverse result))
     ;; Column names
     (append (tblfn-column-names table)
             (list percentage-colname))
     table)))
;; TEST: (tblfn-add-percentage-column '(("name" "class" "price") ("apple" "fruits" "12.3") ("onion" "vegetables" "34.1") ("banana" "fruits" "23.4") ("orange" "fruits" "34.5") ("cabbage" "vegetables" "23.1") ("tomato" "vegetables" "45.1")) "price" "%") => (("name" "class" "price" "%") ("apple" "fruits" "12.3" "7.13") ("onion" "vegetables" "34.1" "19.77") ("banana" "fruits" "23.4" "13.57") ("orange" "fruits" "34.5" "20.00") ("cabbage" "vegetables" "23.1" "13.39") ("tomato" "vegetables" "45.1" "26.14"))
;; TEST: (tblfn-add-percentage-column '(("name" "class" "price") hline ("apple" "fruits" "12.3") ("onion" "vegetables" "34.1") ("banana" "fruits" "23.4") ("orange" "fruits" "34.5") ("cabbage" "vegetables" "23.1") ("tomato" "vegetables" "45.1") hline ("" "" "")) "price" "%") => (("name" "class" "price" "%") hline ("apple" "fruits" "12.3" "7.13") ("onion" "vegetables" "34.1" "19.77") ("banana" "fruits" "23.4" "13.57") ("orange" "fruits" "34.5" "20.00") ("cabbage" "vegetables" "23.1" "13.39") ("tomato" "vegetables" "45.1" "26.14") hline ("" "" ""))
;; TEST: (tblfn-add-percentage-column '(("name" "class" "price") hline ("apple" "fruits" "12.3") ("onion" "vegetables" "34.1") ("banana" "fruits" "23.4") ("orange" "fruits" "34.5") ("cabbage" "vegetables" "23.1") ("tomato" "vegetables" "45.1") hline ("" "" "172.5")) "price" "%" t) => (("name" "class" "price" "%") hline ("apple" "fruits" "12.3" "7.13") ("onion" "vegetables" "34.1" "19.77") ("banana" "fruits" "23.4" "13.57") ("orange" "fruits" "34.5" "20.00") ("cabbage" "vegetables" "23.1" "13.39") ("tomato" "vegetables" "45.1" "26.14") hline ("" "" "172.5" "100.00"))
;; TEST: (tblfn-add-percentage-column '(("name" "class" "price") ("apple" "fruits" "12.3") ("onion" "vegetables" "34.1")) "price" "%" nil 0) => (("name" "class" "price" "%") ("apple" "fruits" "12.3" "27") ("onion" "vegetables" "34.1" "73"))

(defvar tblfn-default-percentage-calc-properties
  '(calc-float-format (fix -2)))

(defun tblfn-add-percentage-column--calc (value total calc-option-plist)
  ;; TODO: calc-option-plistをもう少し指定しやすくする。
  ;;       文字列は`org-table-eval-formula'の書式指定と同じにしてみたり。
  (calc-eval
   (cons
    (if (eq calc-option-plist 0)
        (format "round(100.0*(%s)/(%s))" value total)
      (format "100.0*(%s)/(%s)" value total))
    (cond
     ((integerp calc-option-plist)
      `(calc-float-format (fix ,(- calc-option-plist))))
     ((consp calc-option-plist)
      calc-option-plist)
     (t
      tblfn-default-percentage-calc-properties)))))

(defun tblfn-add-footer-sum (table &rest sum-colspecs)
  "Add a footer row with sum values to TABLE.

SUM-COLSPECS is a list specifying the columns to add sum values to."
  (apply #'tblfn-add-footer-vcalc table "vsum" sum-colspecs))

(defun tblfn-add-footer-vcalc (table vfun &rest calc-colspecs)
  "Add a footer row with calculated values to TABLE.

VFUN is a Calc vector function name (string), such as \"vsum\",
\"vmean\", \"vmax\", etc.

CALC-COLSPECS is a list specifying the columns to calculate values for.
The calculated values are placed in the corresponding columns of the
footer row.  Other columns in the footer row are filled with empty
strings."
  (when (stringp calc-colspecs)
    (setq calc-colspecs (list calc-colspecs)))
  (let ((footer-row (make-list (tblfn-column-count table) "")))
    (dolist (colspec calc-colspecs)
      (let ((col (tblfn-column-index table colspec))
            (result (tblfn-column-vcalc table colspec vfun)))
        (setf (nth col footer-row) result)))

    (append table
            (if (memq 'hline table) ;; (tblfn-use-hlines-p)
                (list 'hline footer-row)
              ;; tableにhlineが一つも無いなら、hlineを追加するとそれは
              ;; ヘッダー区切りになってしまうので入れない。
              (list footer-row)))))

;; TODO: defun tblfn-add-column-calc table


;;;; Org-mode Support


(defvar tblfn-use-hlines 'auto
  "Variable controlling the value returned by `tblfn-use-hlines-p'.
  - nil: Returns nil.
  - t: Returns t.
  - auto: Returns non-nil if the current buffer's major mode is org-mode.")

(defun tblfn-use-hlines-p ()
  "Return non-nil if hlines should be output as separators for headers and
footers.

The result depends on the value of the variable `tblfn-use-hlines'.

This library is generally designed to work correctly even when a table
contains elements that are just `hline'.
However, this alone is not enough to determine whether to insert `hline'
when outputting results, so this function is called to make that
judgment.

Note that the presence of `hline' in the table being processed should
generally take priority over this function's return value.  This is
because adding a footer with `hline' to a table that contains no hlines
at all would make it a header separator, and conversely, adding a footer
without `hline' to a table that already contains `hline' would increase
the likelihood of that footer being recognized as part of the body."
  (if (eq tblfn-use-hlines 'auto)
      (derived-mode-p 'org-mode)
    tblfn-use-hlines))

(defvar tblfn-for-org 'auto
  "Variable controlling the value returned by `tblfn-for-org-p'.
  - nil: Returns nil.
  - t: Returns t.
  - auto: Returns non-nil if the current buffer's major mode is org-mode.")

(defun tblfn-for-org-p ()
  "Return non-nil if the current processing is for an org-mode table.

The result depends on the value of the variable `tblfn-for-org'.

The result of this function primarily affects whether Recalc marks
\(See Info node `(org)Advanced features') should be ignored.
When not for org-mode tables, Recalc marks should not be treated specially."
  (if (eq tblfn-for-org 'auto)
      (derived-mode-p 'org-mode)
    tblfn-for-org))

(defconst tblfn-org-invalid-row-recalc-marks
  '("!" "^" "_" "$" "/") ;; #と*は一応残しておく。
  "List of recalc marks indicating rows that this library should ignore.

References:
- Info node `(org)Advanced features'
- URL `https://orgmode.org/manual/Advanced-features.html'
- `org-recalc-marks'")

(defun tblfn-org-invalid-row-p (row)
  "Return non-nil if ROW is an org-mode row that this library should ignore."
  (and (tblfn-for-org-p)
       (member (car-safe row) tblfn-org-invalid-row-recalc-marks)))

(defun tblfn-skip-org-invalid-rows (table)
  "Skip rows at the beginning of TABLE that should be ignored in org-mode
tables.
Return everything from the first non-ignorable row onwards."
  (while (and table (tblfn-org-invalid-row-p (car table)))
    (setq table (cdr table)))
  table)


;;;; Calc Integration


(defvar tblfn-calc-result-number-type 'string)

(defun tblfn-calc-result-convert (result)
  (if (tblfn-number-string-p result)
      (pcase tblfn-calc-result-number-type
        ('number
         (tblfn-to-number result))
        ('float
         (float (tblfn-to-number result)))
        ('integer
         (round (tblfn-to-number result)))
        ('string
         result)
        (_
         result))
    result))

(defun tblfn-calc-vector-fun (fun values)
  (tblfn-calc-result-convert
   (calc-eval
    (concat fun
            "(["
            (mapconcat (lambda (x) (format "%s" x)) values ",")
            "])"))))


;;;; String/Number Conversion


(defun tblfn-number-string-p (string)
  "Return t if STRING can be recognized as a number.

Commas within digits are allowed.
Leading and trailing whitespace and tabs are ignored.
Returns nil if STRING contains any other non-numeric characters."
  (not
   (null
    (string-match-p
     "\\`[ \t]*[-+]?\
\\(?:[0-9,]+\\(?:\\.[0-9]*\\)?\\|\\(?:\\.[0-9]+\\)\\)\
\\(?:e[-+]?[0-9]+\\)?[ \t]*\\'"
     string))))
;; TEST: (tblfn-number-string-p "") => nil
;; TEST: (tblfn-number-string-p "1") => t
;; TEST: (tblfn-number-string-p "1.") => t
;; TEST: (tblfn-number-string-p "1.2") => t
;; TEST: (tblfn-number-string-p ".2") => t
;; TEST: (tblfn-number-string-p "1e2") => t
;; TEST: (tblfn-number-string-p "1e-2") => t
;; TEST: (tblfn-number-string-p "1.e-2") => t
;; TEST: (tblfn-number-string-p "1.2e-2") => t
;; TEST: (tblfn-number-string-p "-12,345.6e-7") => t
;; TEST: (tblfn-number-string-p "-12.345.6e-7") => nil
;; TEST: (tblfn-number-string-p " \t123 \t") => t
;; TEST: (tblfn-number-string-p " \t\n123 \t\n") => nil

(defun tblfn-string-to-number (string)
  "Convert STRING to a number.

Commas in STRING are removed beforehand."
  (string-to-number (string-replace "," "" string)))

(defun tblfn-to-number (object &optional noerror default)
  "Convert OBJECT to a number.

Signals an error if OBJECT cannot be interpreted as a number.
When NOERROR is non-nil, returns DEFAULT instead of signaling an error.

When OBJECT is a string, `tblfn-number-string-p' is used to determine
whether it can be interpreted as a number.
Commas in strings are removed beforehand."
  (cond
   ((numberp object) object)
   ((stringp object)
    (if (tblfn-number-string-p object)
        (tblfn-string-to-number object)
      (unless noerror
        (error "String cannot be recognized as a number: `%s'" object))
      default))
   (t
    (unless noerror
      (error "Type cannot be converted to a number: `%s'" object))
    default)))
;; TEST: (tblfn-to-number 123) => 123
;; TEST: (tblfn-to-number "123") => 123
;; TEST: (tblfn-to-number "123,456.78") => 123456.78
;; TEST: (tblfn-to-number "123,456.78a") => error
;; TEST: (tblfn-to-number "123,456.78a") => error
;; TEST: (tblfn-to-number " \t-12,345.6e-7 \t") => -0.00123456
;; TEST: (tblfn-to-number 'abc) => error
;; TEST: (tblfn-to-number "") => error
;; TEST: (tblfn-to-number "" t) => nil
;; TEST: (tblfn-to-number "" t 0) => 0

(defun tblfn-to-number-forced (object &optional default)
  "Convert OBJECT to a number, always returning a numeric value.

Returns DEFAULT if OBJECT cannot be interpreted as a number.
If DEFAULT is nil, returns 0.

When OBJECT is a string, `tblfn-number-string-p' is used to determine
whether it can be interpreted as a number.
Commas in strings are removed beforehand."
  (tblfn-to-number object t (or default 0)))

(defun tblfn-to-number-if-possible (object)
  "Convert OBJECT to a number if it is a numeric string, otherwise return
as-is.

If OBJECT is a string that can be interpreted as a number (as determined
by `tblfn-number-string-p'), convert it to a number using
`tblfn-to-number'.  Otherwise, return OBJECT unchanged.

This function is useful for conditionally converting string
representations of numbers while preserving non-numeric values."
  (if (and (stringp object)
           (tblfn-number-string-p object))
      (tblfn-to-number object)
    object))


;;;; CSV I/O


(autoload 'csv-parse-current-row "csv-mode")

(defun tblfn-read-csv-file (file &optional keep-empty-row-column)
  "Read a CSV FILE.

Empty rows in the CSV file become nil by default, but when
KEEP-EMPTY-ROW-COLUMN is non-nil, they become a list containing
a single empty string."
  (with-temp-buffer
    (insert-file-contents file)
    (let (lines)
      (while (not (eobp))
        (if (and (not keep-empty-row-column) (eolp))
            ;; Make empty rows simply nil.
            ;; csv-parse-current-row makes them (""), which is not bad,
            ;; but since there are many places that want to check for
            ;; empty rows and it's rare for empty rows to be valid data,
            ;; we use nil.
            (push nil lines)
          (push (csv-parse-current-row) lines))
        (forward-line))
      (nreverse lines))))

(defun tblfn-write-csv-file (table file)
  "Write TABLE to FILE in CSV format."
  (with-temp-file file
    (dolist (row table)
      (when (tblfn-data-row-p row)
        (insert (mapconcat
                 (lambda (field)
                   (concat "\""
                           (replace-regexp-in-string
                            "\"" "\"\"" (format "%s" field))
                           "\""))
                 row ",")
                "\n"))))
  table)

;; TODO: defun tblfn-insert-csv-file table file row-index


;;;; S-expression Utilities


(defun tblfn--expand-symbol-references-in-sexp (sexp bindings)
  "Expand symbol references in SEXP using BINDINGS with `cl-symbol-macrolet'."
  (macroexpand
   `(cl-symbol-macrolet
        (,@bindings)
      ,sexp)))


;;;; List Utilities


(defun tblfn-take-padded (list count &optional padding-value)
  "Take COUNT elements from LIST, padding with PADDING-VALUE if needed."
  (cl-loop repeat count
           if list
           collect (pop list)
           else
           collect padding-value))
;; TEST: (tblfn-take-padded '(1 2 3) -1) => nil
;; TEST: (tblfn-take-padded '(1 2 3) 0) => nil
;; TEST: (tblfn-take-padded '(1 2 3) 3) => (1 2 3)
;; TEST: (tblfn-take-padded '(1 2 3) 5 -1) => (1 2 3 -1 -1)

(defun tblfn-take-until-cons-cell (list cons-cell)
  "Return elements from LIST until CONS-CELL is reached.

CONS-CELL should be a cons cell that is part of LIST's structure.

This function always returns a new list (even if CONS-CELL is nil)."
  (cl-ldiff list cons-cell))
;; TEST: (tblfn-take-until-cons-cell nil nil) => nil
;; TEST: (tblfn-take-until-cons-cell '(1 2 3) nil) => (1 2 3)
;; TEST: (let ((lst '(1 2 3 4))) (tblfn-take-until-cons-cell lst (nthcdr 2 lst))) => (1 2)
;; TEST: (let ((lst '(1 2 3 4))) (eq (tblfn-take-until-cons-cell lst nil) lst)) => nil

(defun tblfn-count-between (start end)
  "Return the number of cons cells between START and END (END not included).

If END is not found in the list starting from START, return the total
number of elements from START to the end of the list."
  (let ((count 0))
    (while (and start (not (eq start end)))
      (cl-incf count)
      (setq start (cdr start)))
    count))
;; TEST: (let ((lst '(1 2 3 4 5))) (tblfn-count-between lst lst)) => 0
;; TEST: (let ((lst '(1 2 3 4 5))) (tblfn-count-between lst (cdr lst))) => 1
;; TEST: (let ((lst '(1 2 3 4 5))) (tblfn-count-between lst nil)) => 5
;; TEST: (let ((lst '(1 2 3 4 5))) (tblfn-count-between lst '(other list))) => 5

(defun tblfn-find-element-and-after (list element)
  (member element list))

(defun tblfn-find-nth-element-and-after (list element index)
  (when (< index 0)
    (cl-incf index (cl-count element list :test #'equal)))

  (while (and list
              (setq list (cl-member element list :test #'equal))
              (> index 0))
    (setq list (cdr list))
    (cl-decf index))
  list)

(defun tblfn-slice (list start-index &optional end-index)
  "Return a sublist of LIST.

Return a new list containing elements in the range from START-INDEX to
END-INDEX.  END-INDEX is not included in the range.

When START-INDEX or END-INDEX is negative, it represents a position
relative to the end of LIST (the length of the list is added).

When START-INDEX or END-INDEX points before the beginning or after the
end, it is clamped to the beginning or end.  No error occurs.

Return nil if END-INDEX is less than START-INDEX."
  (let ((len (length list)))
    (unless start-index (setq start-index 0))
    (unless end-index (setq end-index len))
    (setq start-index (tblfn-normalize-index-clamp len start-index))
    (setq end-index (tblfn-normalize-index-clamp len end-index))
    (when (< start-index end-index)
      (take (- end-index start-index)
            (nthcdr start-index list)))))
;; TEST: (tblfn-slice '(1 2 3 4 5) -6) => (1 2 3 4 5)
;; TEST: (tblfn-slice '(1 2 3 4 5) -5) => (1 2 3 4 5)
;; TEST: (tblfn-slice '(1 2 3 4 5) -1) => (5)
;; TEST: (tblfn-slice '(1 2 3 4 5) 0) => (1 2 3 4 5)
;; TEST: (tblfn-slice '(1 2 3 4 5) 1) => (2 3 4 5)
;; TEST: (tblfn-slice '(1 2 3 4 5) 5) => nil
;; TEST: (tblfn-slice '(1 2 3 4 5) 6) => nil
;; TEST: (tblfn-slice '(1 2 3 4 5) 2 4) => (3 4)
;; TEST: (tblfn-slice '(1 2 3 4 5) 4 2) => nil
;; TEST: (tblfn-slice '(1 2 3 4 5) 1 -1) => (2 3 4)
;; TEST: (tblfn-slice '(1 2 3 4 5) 2 -2) => (3)
;; TEST: (tblfn-slice '(1 2 3 4 5) 2 -3) => nil
;; TEST: (tblfn-slice '(1 2 3 4 5) 2 -10) => nil

(defun tblfn-head (list count)
  "Return a new list with COUNT elements from the beginning of LIST.

When COUNT is greater than the length of the list, simply return a copy
of LIST.

Return nil if COUNT is 0 or negative."
  (when (> count 0)
    (tblfn-slice list 0 count)))
;; TEST: (tblfn-head '(1 2 3 4 5) -1) => nil
;; TEST: (tblfn-head '(1 2 3 4 5) 0) => nil
;; TEST: (tblfn-head '(1 2 3 4 5) 2) => (1 2)
;; TEST: (tblfn-head '(1 2 3 4 5) 10) => (1 2 3 4 5)

(defun tblfn-tail (list count)
  "Return a new list with COUNT elements from the end of LIST.

When COUNT is greater than the length of the list, simply return a copy
of LIST.

Return nil if COUNT is 0 or negative."
  (when (> count 0)
    (tblfn-slice list (- count))))
;; TEST: (tblfn-tail '(1 2 3 4 5) -1) => nil
;; TEST: (tblfn-tail '(1 2 3 4 5) 0) => nil
;; TEST: (tblfn-tail '(1 2 3 4 5) 2) => (4 5)
;; TEST: (tblfn-tail '(1 2 3 4 5) 10) => (1 2 3 4 5)

(defun tblfn-normalize-index (len index include-end &optional noerror)
  (let ((original-index index))
    (unless index
      (setq index (if include-end len 0)))

    (when (< index 0)
      (cl-incf index len))
    (if (and (>= index 0)
             (if include-end
                 (<= index len)
               (< index len)))
        index
      (if noerror
          nil
        (signal 'args-out-of-range (list original-index))))))

(defun tblfn-normalize-index-clamp (len index)
  (unless index (setq index len))
  (when (< index 0)
    (cl-incf index len))
  (cond
   ((< index 0) 0)
   ((< index len) index)
   (t len)))


;;;; For Maintenance


(defun tblfn--generate-function-list-for-readme ()
  "Generate headings for README.org.
The result is stored in the kill ring if called by interactively."
  (interactive)
  (let ((level 0)
        (result-text "")
        (regexp
         (concat
          "\\(?:^;;;;\\(;*\\) \\([^\n]+\\)$\\)\\|"
          "\\(?:^(\\(?:cl-defun\\|defun\\|defsubst\\|defmacro\\)"
          "[ \n]+" "\\([^ \n]+\\)[ \n]+\\(([^)]*)\\)\\)")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (cond
         ((match-beginning 1)
          (setq level (length (match-string 1)))
          (setq result-text
                (concat result-text
                        (make-string (* level 2) ?\s) "- " (match-string 2)
                        "\n")))
         ((match-beginning 3)
          (let ((funname (match-string 3))
                ;; (params (replace-regexp-in-string
                ;;          "[ \n]+"
                ;;          " " (match-string 4)))
                )
            (setq result-text
                  (concat
                   result-text
                   (make-string (* (1+ level) 2) ?\s)
                   "- " funname
                   ;; " " params
                   "\n")))))))
    (when (called-interactively-p 'interactive)
      (kill-new result-text)
      (message "Copied"))
    result-text))
;; EXAMPLE: (tblfn--generate-function-list-for-readme)

(defun tblfn--update-function-list-in-readme ()
  (interactive)
  (let ((function-list (tblfn--generate-function-list-for-readme)))
    (dolist (file '("README.org" "README-ja.org"))
      (find-file file)
      (goto-char (point-min))
      (re-search-forward "^\\* \\(Function List\\|関数一覧\\)")
      (forward-line)
      (let ((start (point))
            (end (progn (re-search-forward "^\\*") (forward-line 0) (point))))
        (delete-region start end)
        (insert "\n" function-list "\n")))))
;; EXAMPLE: (tblfn--update-function-list-in-readme)


(provide 'tblfn)
;;; tblfn.el ends here
