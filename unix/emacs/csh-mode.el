;; csh-mode.el --- sh (ksh, bash) script editing mode for GNU Emacs.

;; Copyright (C) 1992-95 Gary Ellison.

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; csh-mode|John D. Hobbs|hobbs@fnal.gov
;; Mode for editing csh scripts
;;
;; Maintainer: John D. Hobbs
;; Keywords: languages, shell, csh, tcsh, unix
;;
;;

;;; Commentary:

;;
;; Description:
;;   csh script editing commands for emacs.  Minor tweaking to ksh-mode.el
;; 
;; Installation:
;;   Put csh-mode.el in some directory in your load-path.
;;   Refer to the installation section of csh-mode's function definition.
;;
;; Usage:
;;   This major mode assists shell script writers with indentation
;;   control and control structure construct matching in much the same
;;   fashion as other programming language modes. Invoke describe-mode
;;   for more information.
;; 
;; Bugs:
;;   When the csh-align-to-keyword is non-nil and the nester
;;   is a multi-command expression with a compound command
;;   the lines following the compound end will align incorrectly
;;   to the compound command instead of it's current indentation.
;;   The fix will probably require the detection of syntax elements
;;   in the nesting line.
;;   
;;   Function ending brace "}" must be on a separate line for indent-line
;;   to do the right thing.
;;
;;   Explicit function definition matching will proclaim in the minibuffer
;;   "No matching compound command" followed by "Matched ... "
;;
;;   indent-for-comment fails to recognize a comment starting in column 0,
;;   hence it moves the comment-start in comment-column.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HISTORY  - See ksh-mode.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst csh-mode-version "1.0"
  "*Version numbers of this version of csh-mode")

;;
;; Variables controlling indentation style
;;

(defvar csh-indent 2 
  ;; perhaps c-basic-offset would be okay to use as a default, but using
  ;; default-tab-width as the default is ridiculous --Stig
  "*Indentation of csh statements with respect to containing block. A value
of nil indicates compound list keyword \(\"do\" and \"then\"\) alignment.")
(defvar csh-case-item-offset csh-indent
  "*Additional indentation for case items within a case statement.")
(defvar csh-case-indent 2
  "*Additional indentation for statements under case items.")
(defvar csh-group-offset (- csh-indent)
  "*Additional indentation for keywords \"do\" and \"then\".")
(defvar csh-brace-offset 0
  "*Additional indentation of \"{\" under functions or brace groupings.")
(defvar csh-multiline-offset 1
  "*Additional indentation of line that is preceded of a line ending with a
\\ to make it continue on next line.")
(defvar csh-match-and-tell t
  "*If non-nil echo in the minibuffer the matching compound command
for the \"end\", \"}\", \"endsw\" or \"endif\". ")
(defvar csh-tab-always-indent t
  "*Controls the operation of the TAB key. If t (the default), always
reindent the current line.  If nil, indent the current line only if
point is at the left margin or in the line's indentation; otherwise
insert a tab.")

(defvar csh-align-to-keyword nil
  ;; #### - this is broken, so it should be disabled by default --Stig
  "*Controls whether nested constructs align from the keyword or
the current indentation. If non-nil, indentation will be relative to
the column the keyword starts. If nil, indentation will be relative to
the current indentation of the line the keyword is on.
The default value is nil.
The non-nil case doesn't work very well.")

(defvar csh-comment-regexp "^\\s *#"
  "*Regular expression used to recognize comments. Customize to support
csh-like languages.")

(defun csh-current-indentation ()
  nil
  )
;;
(fset 'csh-current-indentation 'current-column)
;;
;; Variables controlling completion
(defvar csh-completion-list '())
(make-variable-buffer-local 'csh-completion-list)
(set-default 'csh-completion-list  '())

;;
;; -type-  : type number, 0:misc, 1:variable, 2:function
;; -regexp-: regexp used to parse the script
;; -match- : used by match-beginning/end to pickup target
;;
(defvar csh-completion-type-misc 0)
(defvar csh-completion-regexp-var "\\([A-Za-z_0-9]+\\)=")
(defvar csh-completion-type-var 1)
(defvar csh-completion-match-var 1) 
(defvar csh-completion-regexp-var2 "\\$\\({\\|{#\\)?\\([A-Za-z_0-9]+\\)[#%:}]?")
(defvar csh-completion-match-var2 2)
(defvar csh-completion-regexp-function
  "\\(function\\)?[ \t]*\\([A-Za-z_0-9]+\\)[ \t]*([ \t]*)")
(defvar csh-completion-type-function 2)
(defvar csh-completion-match-function 2)

;;
;; Variable controlling fontification
;;
(defvar csh-keywords '("foreach" "switch" "case" "default:" "endsw"
"if" "then" "else if" "else" "endif" "while" "time"
"alias" "bg" "break" "breaksw" "continue" "cd" "chdir" "echo" "fg" 
"jobs" "kill" "exit" "pwd" "set" "shift" 
"unlimit" "unalias" "unset" "end" "repeat" "goto" "eval" "exec" "glob" 
"hashstat" "history" "limit" "login" "logout" "nice" "nohup" "notify"
"onintr" "popd" "pushd" "rehash" "setenv" "source" "stop" "suspend" "umask"
"unhash" "unsetenv" "wait" ))

;;       '("\\<function[ \t]+\\([^(; \t]+\\)" 1 font-lock-function-name-face)
(defconst csh-font-lock-keywords
      (list
       ;; Fontify [[ ]] expressions
       '("\\(\\[.*\\]\\)"  1 font-lock-doc-string-face t)
       ;; Fontify keywords
       (cons (concat
	      "\\(\\<"
	      (mapconcat 'identity csh-keywords "\\>\\|\\<")
	      "\\>\\)")
	     1)
       ;; Fontify function names
       '("\\<function[ \t]+\\([^(; \t]+\\)" 1 font-lock-function-name-face)
       '("\\(^[ \t]*[A-Za-z_][A-Za-z_0-9]*[ \t]*()\\)" 1 font-lock-function-name-face)
       ))

(put 'csh-mode	'font-lock-keywords 'csh-font-lock-keywords)

;; XEmacs change -- This can incorrectly set some Perl scripts to
;; csh-mode.  It also won't work for some other shells which csh-mode
;; nominally works with.
;(defun csh-check-hook ()
;    (save-excursion
;     (save-restriction
;       (widen)
;       (goto-char (point-min))
;       (cond ((looking-at "#![ \t]*/.*/k?sh[ \t]*")
;	      (csh-mode))))))
;
;(add-hook 'find-file-hooks 'csh-check-hook)

;;
;; Context/indentation regular expressions
;; 
;; indenting expressions
;;
(defconst csh-then-do-re     "^[^#\n]*\\s\"*\\b\\(then\\|do\\)\\b"
  "*Regexp used to locate grouping keywords: \"then\" and \"do\"" )

;;(defconst csh-do-re          "^[ \t]*\\bdo\\(\\b\\|$\\)"
(defconst csh-do-re          "^\\s *\\bdo\\(\\b\\|$\\)"
  "*Regexp used to match keyword: do")

(defconst csh-then-re        "^\\s *\\bthen\\(\\b\\|$\\)"
  "*Regexp used to match keyword: then")

;;
;; Structure starting/indenting keywords
;;
(defconst csh-else-re           "^\\s *\\belse\\(\\b\\|$\\)"
  "*Regexp used to match keyword: else")

(defconst csh-elif-re           "^\\s *\\belseif\\(\\b\\|$\\)"
  "*Regexp used to match keyword: elseif")

(defconst csh-brace-re           "^\\S>*{[ \t\n]"
  "*Regexp used to match syntactic entity: { ")

(defconst csh-case-item-end-re           "breaksw"
  "*Regexp used to match case item end syntactic entity: breaksw;")

(defconst csh-keywords-re
  "^[^#\n]*\\s\"*\\b\\(else\\|if[ 	]*(.*)[ 	]*then\\|elseif\\|case\\|switch\\|while\\|foreach\\)\\b"
  "*Regexp used to detect compound command keywords: if then, else, elseif 
switch, case, while, and foreach")


(defconst csh-if-re         "^[^#\n]*\\s\"*\\b\\(if[ 	]*(.*)[ 	]*then\\)\\b"
  "*Regexp used to match keyword: if then")

(defconst csh-iteration-keywords-re 
  "^[^#\n]*\\s\"*\\b\\(while\\|foreach\\|switch\\|case\\)\\b"
  "*Match one of the keywords: while, foreach, switch, case")

(defconst csh-case-re           "^[^#\n]*\\s\"*\\b\\(case\\)\\b"
  "*Regexp used to match keyword: case")

(defconst csh-explicit-func-re
  "^\\s *\\(function\\s [a-zA-z_][a-zA-Z0-1_]*\\)\\b"
  "*Match an explicit function definition: function name")

(defconst csh-implicit-func-re
  "^\\s *\\([a-zA-z_][a-zA-Z0-1_]*\\)\\s *()\\s *"
  "*Match an implicit function definition: name ()")

(defconst csh-func-brace-re "^\\s *\\(.*{\\)[ \t\n]+"
  "*Match a implicit function definition brace: name { ")

;;
;; indenting 
(defconst csh-case-item-re           "^[ \t\n]*XXXXcaseXXXX"
;;"^[^#\n]*\\s\"*\\()\\)"
  "*Regexp used to match case-items including csh88")

(defconst csh-paren-re           "^[^#\n]*\\s\"*)[ \t\n]+"
  "*Regexp used to match compound list & case items")

;;
;; structure ending keyword regular expressions
(defconst csh-fi-re            "^\\s *endif\\b"
  "*Regexp used to match keyword: endif")

(defconst csh-esac-re          "^\\s *breaksw\\b"
  "*Regexp used to match keyword: breaksw;")

(defconst csh-end-re          "^\\s *end\\b\\|^\\s *endsw\\b\\|^\\s *breaksw\\b"
  "*Regexp used to match keyword: end, breaksw, endsw")

(defconst csh-brace-end-re  "^\\s *}\\s *"
  "*Regexp used to match function brace-groups")

(defconst csh-multiline-re "^.*\\\\$"
  "*Regexp used to match a line with a statement using more lines.")

;;
;;
;; Create mode specific tables
(defvar csh-mode-syntax-table nil
  "Syntax table used while in csh mode.")
(if csh-mode-syntax-table
    ()
  (setq csh-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"" csh-mode-syntax-table)
  (modify-syntax-entry ?` "\"" csh-mode-syntax-table)
  (modify-syntax-entry ?\n ">" csh-mode-syntax-table)
  (modify-syntax-entry ?\f ">" csh-mode-syntax-table)
  (modify-syntax-entry ?# "<" csh-mode-syntax-table)
  (modify-syntax-entry ?_ "w" csh-mode-syntax-table)
  (modify-syntax-entry ?< "." csh-mode-syntax-table)
  (modify-syntax-entry ?> "." csh-mode-syntax-table)
  (modify-syntax-entry ?& "." csh-mode-syntax-table)
  (modify-syntax-entry ?| "." csh-mode-syntax-table)
  (modify-syntax-entry ?$ "." csh-mode-syntax-table)
  (modify-syntax-entry ?% "." csh-mode-syntax-table)
  (modify-syntax-entry ?= "." csh-mode-syntax-table)
  (modify-syntax-entry ?/ "." csh-mode-syntax-table)
  (modify-syntax-entry ?+ "." csh-mode-syntax-table)
  (modify-syntax-entry ?* "." csh-mode-syntax-table)
  (modify-syntax-entry ?- "." csh-mode-syntax-table)
  (modify-syntax-entry ?\; "." csh-mode-syntax-table)
  )

(defvar csh-mode-abbrev-table nil
  "Abbrev table used while in csh mode.")
(define-abbrev-table 'csh-mode-abbrev-table ())

(defvar csh-mode-map nil 
  "Keymap used in csh mode")

(if csh-mode-map
    ()
  (setq csh-mode-map (make-sparse-keymap))
  (define-key csh-mode-map "\t"    'csh-indent-command)
  (define-key csh-mode-map "\n"    'reindent-then-newline-and-indent)
  (define-key csh-mode-map '[return] 'reindent-then-newline-and-indent)
;;  (define-key csh-mode-map "\t"    'csh-indent-line)
;;  (define-key csh-mode-map "\177"    'backward-delete-char-untabify)
  (define-key csh-mode-map "\C-j"    'reindent-then-newline-and-indent)
  (define-key csh-mode-map "\e\t"    'csh-complete-symbol)
  (define-key csh-mode-map "\C-c\t"    'csh-completion-init-and-pickup)
  )


;;;###autoload
(defun csh-mode ()
  "csh-mode 1.1 - Major mode for editing C
shell scripts.
Special key bindings and commands:
\\{csh-mode-map}
Variables controlling indentation style:
csh-indent
    Indentation of csh statements with respect to containing block.
    Default value is 2.
csh-case-indent
    Additional indentation for statements under case items.
    Default value is nil which will align the statements one position 
    past the \")\" of the pattern.
csh-case-item-offset
    Additional indentation for case items within a case statement.
    Default value is 2.
csh-group-offset
    Additional indentation for keywords \"do\" and \"then\".
    Default value is -2.
csh-brace-offset
    Additional indentation of \"{\" under functions or brace groupings.
    Default value is 0.
csh-multiline-offset
   Additional indentation of line that is preceded of a line ending with a
   \\ to make it continue on next line.
csh-tab-always-indent
    Controls the operation of the TAB key. If t (the default), always
    reindent the current line.  If nil, indent the current line only if
    point is at the left margin or in the line's indentation; otherwise
    insert a tab.
csh-match-and-tell
    If non-nil echo in the minibuffer the matching compound command
    for the \"end\", \"}\", \"endif\", or \"end\". Default value is t.

csh-align-to-keyword
    Controls whether nested constructs align from the keyword or
    the current indentation. If non-nil, indentation will be relative to
    the column the keyword starts. If nil, indentation will be relative to
    the current indentation of the line the keyword is on.
    The default value is non-nil.

csh-comment-regexp
  Regular expression used to recognize comments. Customize to support
  csh-like languages. Default value is \"\^\\\\s *#\".

Style Guide.
 By setting
    (setq csh-indent default-tab-width)
    (setq csh-group-offset 0)

    The following style is obtained:

    if [ -z $foo ]
	    then
		    bar    # <-- csh-group-offset is additive to csh-indent
		    foo
    fi

 By setting
    (setq csh-indent default-tab-width)
    (setq csh-group-offset (- 0 csh-indent))

    The following style is obtained:

    if [ -z $foo ]
    then
	    bar
	    foo
    fi

 By setting
    (setq csh-case-item-offset 1)
    (setq csh-case-indent nil)

    The following style is obtained:

    case x in *
     foo) bar           # <-- csh-case-item-offset
          baz;;         # <-- csh-case-indent aligns with \")\"
     foobar) foo
             bar;;
    esac

 By setting
    (setq csh-case-item-offset 1)
    (setq csh-case-indent 6)

    The following style is obtained:

    case x in *
     foo) bar           # <-- csh-case-item-offset
           baz;;        # <-- csh-case-indent
     foobar) foo
           bar;;
    esac
    

Installation:
  Put csh-mode.el in some directory in your load-path.
  Put the following forms in your .emacs file.

 (setq csh-mode-hook
      (function (lambda ()
         (font-lock-mode 1)             ;; font-lock the buffer
         (setq csh-indent 8)
	 (setq csh-group-offset -8))
	 (setq csh-brace-offset -8)   
         (setq csh-tab-always-indent t)
         (setq csh-match-and-tell t)
         (setq csh-align-to-keyword t)	;; Turn on keyword alignment
	 )))"
  (interactive)
  (kill-all-local-variables)
  (use-local-map csh-mode-map)
  (setq major-mode 'csh-mode)
  (setq mode-name "Csh")
  (setq local-abbrev-table csh-mode-abbrev-table)
  (set-syntax-table csh-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'csh-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'csh-indent-region)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  ;;
  ;; config font-lock mode
  (make-local-variable 'font-lock-keywords) 
  (setq font-lock-keywords csh-font-lock-keywords)
  ;;
  ;; Let the user customize
  (run-hooks 'csh-mode-hook)
  (if (not csh-align-to-keyword)
      (csh-align-to-keyword -1)
    )
  ) ;; defun

;;
;; Support functions

(defun csh-align-to-keyword (&optional arg)
  "Toggle value of csh-align-to-keyword and rebind the csh-current-indentation
function. With arg, force alignment to keyword if and only if arg is positive."
  (interactive)
  (if (null arg)			;just toggle
      (cond ((not csh-align-to-keyword)
	     (setq csh-align-to-keyword t)
	     (fset 'csh-current-indentation 'current-column))
	    (t
	     (setq csh-align-to-keyword nil)
	     (fset 'csh-current-indentation 'current-indentation))
	    )
    (cond ((natnump arg)
	   (setq csh-align-to-keyword t)
	   (fset 'csh-current-indentation 'current-column))
	  (t
	   (setq csh-align-to-keyword nil)
	   (fset 'csh-current-indentation 'current-indentation))
	  ))
  )

(defun csh-current-line ()
  "Return the vertical position of point in the buffer.
Top line is 1."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0))
  )


(defun csh-line-to-string ()
  "From point, construct a string from all characters on
current line"
  (skip-chars-forward " \t") ;; skip tabs as well as spaces
  (buffer-substring (point)
                    (progn
                      (end-of-line 1)
                      (point))))

(defun csh-get-nest-level ()
  "Return a 2 element list (nest-level nest-line) describing where the
current line should nest."
  (let ((case-fold-search)
    	(level))
    (save-excursion
      (forward-line -1)
      (while (and (not (bobp))
		  (null level))
	(if (and (not (looking-at "^\\s *$"))
 		 (not (save-excursion
 			(forward-line -1)
 			(beginning-of-line)
			(looking-at csh-multiline-re)))
		 (not (looking-at csh-comment-regexp)))
	    (setq level (cons (current-indentation)
			      (csh-current-line)))
	  (forward-line -1)
	  );; if
	);; while
      (if (null level)
	  (cons (current-indentation) (csh-current-line))
	level)
      )
    )
  )

(defun csh-looking-at-compound-list ()
  "Return true if current line contains compound list initiating keyword"
  (or 
   (looking-at csh-do-re)
   (looking-at csh-then-re)
   ) ;; or
  ) ;; defun

(defun csh-looking-at-case-item ()
  "Return true if current line is a case-item .vs. paren compound list"
  (save-excursion
    (beginning-of-line)
    ;;
    ;; Handle paren indentation constructs for this line
    (cond ((looking-at csh-paren-re)
	   (goto-line (cdr (csh-get-nest-level)))
	   ;;
	   ;; The question is whether this is really a case item or just
	   ;; parenthesized compound list.
	   (cond ((or (looking-at csh-case-re)
		      (looking-at csh-case-item-end-re)))
		 ;;
		 ;; turns out to be a parenthesized compound list
		 ;; so propigate the nil for cond
		 )
	   ))
    )
  ) ;; defun

(defun csh-get-case-indent ()
  "Return the column of the closest open case statement"
  (save-excursion
    (let (
	  (nest-list (csh-get-compound-level csh-case-re csh-esac-re (point)))
	  )
      (if (null nest-list)
	  (progn 
	    (if csh-match-and-tell
		(message "No matching case for ;;"))
	    0)
	(car nest-list)))
    )
  )

;;
;; Functions which make this mode what it is
;;

(defun csh-get-nester-column (nest-line)
  "Return the column to indent to with respect to nest-line taking 
into consideration keywords and other nesting constructs."
  (save-excursion 
    (let ((fence-post)
	  (nester-column)
	  (case-fold-search)
	  (start-line (csh-current-line)))
      ;;
      ;; Handle case item indentation constructs for this line
      (cond ((csh-looking-at-case-item)
	     (save-excursion
	       (goto-line nest-line)
	       (let ((fence-post (save-excursion (end-of-line) (point))))
		 ;;
		 ;; Now know there is a case-item so detect whether
		 ;; it is first under case, just another case-item, or
		 ;; a case-item and case-item-end all rolled together.
		 ;;
		 (cond ((re-search-forward csh-case-re fence-post t)
			(goto-char (match-beginning 1))
			(+ (csh-current-indentation) csh-case-item-offset))

		       ((csh-looking-at-case-item)
			(current-indentation))

		       ((looking-at csh-case-item-end-re)
			(end-of-line)
			(+ (csh-get-case-indent) csh-case-item-offset))
		       )
		 )))
	    (t;; Not a case-item.  What to do relative to the nest-line?
	     (save-excursion
	       (goto-line nest-line)
	       (setq fence-post (save-excursion (end-of-line) (point)))
	       (setq nester-column
		     (save-excursion
		       (cond
			;;
			;; Check if we are in a continued statement
			((and (looking-at csh-multiline-re)
			      (save-excursion
				(goto-line (1- start-line))
				(looking-at csh-multiline-re)))
			 (+ (current-indentation) csh-multiline-offset))

			;; In order to locate the column of the keyword,
			;; which might be embedded within a case-item,
			;; it is necessary to use re-search-forward.
			;; Search by literal case, since shell is
			;; case-sensitive.
			((re-search-forward csh-keywords-re fence-post t)
			 (goto-char (match-beginning 1))
			 (if (looking-at csh-case-re)
			     (+ (csh-current-indentation) csh-case-item-offset)
			   (+ (csh-current-indentation)
			      (if (null csh-indent)
				  2 csh-indent)
			      )))

			((re-search-forward csh-then-do-re fence-post t)
			 (if (null csh-indent)
			     (progn 
			       (goto-char (match-end 1))
			       (+ (csh-current-indentation) 1))
			   (progn
			     (goto-char (match-beginning 1))
			     (+ (csh-current-indentation) csh-indent))
			   ))

			((looking-at csh-brace-re)
			 (+ (current-indentation)
			    (if (null csh-indent)
				2 csh-indent)
			    ))
			;;
			;; Forces functions to first column
			((or (looking-at csh-implicit-func-re)
			     (looking-at csh-explicit-func-re))
			 (if (looking-at csh-func-brace-re)
			     (if (null csh-indent)
				 2 csh-indent)
			   csh-brace-offset))

			;;
			;; Need to first detect the end of a case-item
			((looking-at csh-case-item-end-re)
			 (end-of-line)
			 (+ (csh-get-case-indent) csh-case-item-offset))
			;;
			;; Now detect first statement under a case item
			((csh-looking-at-case-item)
			 (if (null csh-case-indent)
			     (progn
			       (re-search-forward csh-case-item-re fence-post t)
			       (goto-char (match-end 1))
			       (+ (current-column) 1))
			   (+ (current-indentation) csh-case-indent)))

			;; This is hosed when using current-column
			;; and there is a multi-command expression as the
			;; nester.
			(t (current-indentation)))
		       )
		     ));; excursion over
	     ;;
	     ;; Handle additional indentation constructs for this line
	     (cond ((csh-looking-at-compound-list)
		    (+ nester-column csh-group-offset))
		   ((looking-at csh-brace-re)
		    (+ nester-column csh-brace-offset))
		   (t nester-column))
	     );; Not a case-item
	    )
      );;let
    );; excursion
  );; defun

(defun csh-indent-command ()
  "Indent current line relative to containing block and allow for
csh-tab-always-indent customization"
  (interactive)
  (let (case-fold-search)
    (cond ((save-excursion
	     (skip-chars-backward " \t")
	     (bolp))
	   (csh-indent-line))
	  (csh-tab-always-indent
	   (save-excursion
	     (csh-indent-line)))
	  (t (insert-tab))
	  ))
  )


(defun csh-indent-line ()
  "Indent current line as far as it should go according
to the syntax/context"
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (if (bobp)
	  nil
	;;
	;; Align this line to current nesting level
	(let*
	    (
	     (level-list (csh-get-nest-level)) ; Where to nest against
	     ;;           (last-line-level (car level-list))
	     (this-line-level (current-indentation))
	     (nester-column (csh-get-nester-column (cdr level-list)))
	     (struct-match (csh-match-structure-and-reindent))
	     )
	  (if struct-match
	      (setq nester-column struct-match))
	  (if (eq nester-column this-line-level)
	      nil
	    (beginning-of-line)
	    (let ((beg (point)))
	      (back-to-indentation)
	      (delete-region beg (point)))
	    (indent-to nester-column))
	  );; let*
	);; if
      );; excursion
    ;;
    ;; Position point on this line
    (let*
	(
	 (this-line-level (current-indentation))
	 (this-bol (save-excursion
		     (beginning-of-line)
		     (point)))
	 (this-point (- (point) this-bol))
	 )
      (cond ((> this-line-level this-point);; point in initial white space
	     (back-to-indentation))
	    (t nil)
	    );; cond
      );; let*
    );; let
  );; defun


(defun csh-match-indent-level (begin-re end-re)
  "Match the compound command and indent. Return nil on no match,
indentation to use for this line otherwise."
  (interactive)
  (let* ((case-fold-search)
	 (nest-list 
	  (save-excursion
	    (csh-get-compound-level begin-re end-re (point))
	    ))
	 ) ;; bindings
    (if (null nest-list)
	(progn
	  (if csh-match-and-tell
	      (message "No matching compound command"))
	  nil) ;; Propagate a miss.
      (let* (
	     (nest-level (car nest-list))
	     (match-line (cdr nest-list))
	     ) ;; bindings
	(if csh-match-and-tell
	    (save-excursion
	      (goto-line match-line)
	      (message "Matched ... %s" (csh-line-to-string))
	      ) ;; excursion
	  ) ;; if csh-match-and-tell
	nest-level ;;Propagate a hit.
	) ;; let*
      ) ;; if
    ) ;; let*
  ) ;; defun csh-match-indent-level

(defun csh-match-structure-and-reindent ()
  "If the current line matches one of the indenting keywords
or one of the control structure ending keywords then reindent. Also
if csh-match-and-tell is non-nil the matching structure will echo in
the minibuffer"
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at csh-else-re)
	     (csh-match-indent-level csh-if-re csh-fi-re))
	    ((looking-at csh-elif-re)
	     (csh-match-indent-level csh-if-re csh-fi-re))
	    ((looking-at csh-fi-re)
	     (csh-match-indent-level csh-if-re csh-fi-re))
	    ((looking-at csh-end-re)
	     (csh-match-indent-level csh-iteration-keywords-re csh-end-re))
	    ((looking-at csh-esac-re)
	     (csh-match-indent-level csh-case-re csh-esac-re))
	    ;;
	    ((looking-at csh-brace-end-re)
	     (cond
	      ((csh-match-indent-level csh-implicit-func-re csh-brace-end-re))
	      ((csh-match-indent-level csh-explicit-func-re csh-brace-end-re))
	      ((csh-match-indent-level csh-func-brace-re csh-brace-end-re))
	      (t nil)))
	    (t nil)
	    );; cond
      )
    ))

(defun csh-get-compound-level 
  (begin-re end-re anchor-point &optional balance-list)
  "Determine how much to indent this structure. Return a list (level line) 
of the matching compound command or nil if no match found."
  (let* 
      (;; Locate the next compound begin keyword bounded by point-min
       (match-point (if (re-search-backward begin-re (point-min) t)
			(match-beginning 1) 0))
       (nest-column (if (zerop match-point)
			1 
		      (progn
			(goto-char match-point)
			(csh-current-indentation))))
       (nest-list (cons 0 0))    ;; sentinel cons since cdr is >= 1
       )
    (if (zerop match-point)
	nil ;; graceful exit from recursion
      (progn
	(if (nlistp balance-list)
	    (setq balance-list (list)))
	;; Now search forward from matching start keyword for end keyword
	(while (and (consp nest-list) (zerop (cdr nest-list))
		    (re-search-forward end-re anchor-point t))
	  (if (not (memq (point) balance-list))
	      (progn
		(setq balance-list (cons (point) balance-list))
		(goto-char match-point)  ;; beginning of compound cmd
		(setq nest-list
		      (csh-get-compound-level begin-re end-re
					     anchor-point balance-list))
		)))

	(cond ((consp nest-list)
	       (if (zerop (cdr nest-list))
		 (progn
		   (goto-char match-point)
		   (cons nest-column (csh-current-line)))
		 nest-list))
	      (t nil)
	      )
	)
      )
    )
  )


(defun csh-indent-region (start end)
  "From start to end, indent each line."
  ;; The algorithm is just moving through the region line by line with
  ;; the match noise turned off.  Only modifies nonempty lines.
  (save-excursion
    (let (csh-match-and-tell
	  (endmark (copy-marker end)))
      
      (goto-char start)
      (beginning-of-line)
      (setq start (point))
      (while (> (marker-position endmark) start)
	(if (not (and (bolp) (eolp)))
	    (csh-indent-line))
	(forward-line 1)
	(setq start (point)))

      (set-marker endmark nil)
      )
    )
  )

;;
;; Completion code supplied by Haavard Rue <hrue@imf.unit.no>.
;;
;;
;; add a completion with a given type to the list
;;
(defun csh-addto-alist (completion type)
  (setq csh-completion-list
	(append csh-completion-list
		(list (cons completion type)))))
;;
;; init the list and pickup all 
;;
(defun csh-completion-init-and-pickup ()
  (interactive)
  (let (case-fold-search)
    (csh-completion-list-init)
    (csh-pickup-all)))

;;
;; init the list
;;
(defun csh-completion-list-init ()
  (interactive)
  (setq csh-completion-list
	(list
	 (cons "if"  csh-completion-type-misc)
	 (cons "while"  csh-completion-type-misc)
	 (cons "switch"  csh-completion-type-misc)
	 (cons "foreach"  csh-completion-type-misc)
	 (cons "continue"  csh-completion-type-misc)
	 (cons "else if"  csh-completion-type-misc)
	 (cons "endif"  csh-completion-type-misc)
	 (cons "case"  csh-completion-type-misc)
	 (cons "end"  csh-completion-type-misc)
	 (cons "break"  csh-completion-type-misc)
	 (cons "breaksw"  csh-completion-type-misc)
	 (cons "exit"  csh-completion-type-misc)
	 (cons "endsw" csh-completion-type-misc)
	 (cons "end"  csh-completion-type-misc))))

(defun csh-eol-point ()
  (save-excursion
    (end-of-line)
    (point)))

(defun csh-bol-point ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun csh-pickup-all ()
  "Pickup all completions in buffer."
  (interactive)
  (csh-pickup-completion-driver (point-min) (point-max) t))

(defun csh-pickup-this-line ()
  "Pickup all completions in current line."
  (interactive)
  (csh-pickup-completion-driver (csh-bol-point) (csh-eol-point) nil))

(defun csh-pickup-completion-driver (pmin pmax message)
  "Driver routine for csh-pickup-completion."
  (if message
      (message "pickup completion..."))
  (let* (
	 (i1
	  (csh-pickup-completion  csh-completion-regexp-var
				 csh-completion-type-var
				 csh-completion-match-var
				 pmin pmax))
	 (i2
	  (csh-pickup-completion  csh-completion-regexp-var2
				 csh-completion-type-var
				 csh-completion-match-var2
				 pmin pmax))
	 (i3
	  (csh-pickup-completion  csh-completion-regexp-function
				 csh-completion-type-function
				 csh-completion-match-function
				 pmin pmax)))
    (if message
	(message "pickup %d variables and %d functions." (+ i1 i2) i3))))

(defun csh-pickup-completion (regexp type match pmin pmax)
  "Pickup completion in region and addit to the list, if not already
there." 
  (let ((i 0) kw obj)
    (save-excursion
      (goto-char pmin)
      (while (and
	      (re-search-forward regexp pmax t)
	      (match-beginning match)
	      (setq kw  (buffer-substring
			 (match-beginning match)
			 (match-end match))))
	(progn
	  (setq obj (assoc kw csh-completion-list))
	  (if (or (equal nil obj)
		  (and (not (equal nil obj))
		       (not (= type (cdr obj)))))
	      (progn
		(setq i (1+ i))
		(csh-addto-alist kw type))))))
    i))

(defun csh-complete-symbol ()
  "Perform completion."
  (interactive)
  (let* ((case-fold-search)
	 (end (point))
         (beg (unwind-protect
                  (save-excursion
                    (backward-sexp 1)
                    (while (= (char-syntax (following-char)) ?\')
                      (forward-char 1))
                    (point))))
         (pattern (buffer-substring beg end))
	 (predicate 
	  ;;
	  ;; ` or $( mark a function
	  ;;
	  (save-excursion
	    (goto-char beg)
	    (if (or
		 (save-excursion
		   (backward-char 1)
		   (looking-at "`"))
		 (save-excursion
		   (backward-char 2)
		   (looking-at "\\$(")))
		(function (lambda (sym)
			    (equal (cdr sym) csh-completion-type-function)))
	      ;;
	      ;; a $, ${ or ${# mark a variable
	      ;;
	      (if (or
		   (save-excursion
		     (backward-char 1)
		     (looking-at "\\$"))
		   (save-excursion
		     (backward-char 2)
		     (looking-at "\\${"))
		   (save-excursion
		     (backward-char 3)
		     (looking-at "\\${#")))
		  (function (lambda (sym)
			      (equal (cdr sym)
				     csh-completion-type-var)))
		;;
		;; don't know. use 'em all
		;;
		(function (lambda (sym) t))))))
	 ;;
	 (completion (try-completion pattern csh-completion-list predicate)))
    ;;
    (cond ((eq completion t))
	  ;;
	  ;; oops, what is this ?
	  ;;
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern))
	  ;;
	  ;; insert
	  ;;
          ((not (string= pattern completion))
           (delete-region beg end)
           (insert completion))
	  ;;
	  ;; write possible completion in the minibuffer,
	  ;; use this instead of a seperate buffer (usual)
	  ;;
          (t
           (let ((list (all-completions pattern csh-completion-list predicate))
		 (string ""))
	     (while list
	       (progn
		 (setq string (concat string (format "%s " (car list))))
		 (setq list (cdr list))))
	     (message string))))))

(provide 'csh-mode)
;;; csh-mode.el ends here
