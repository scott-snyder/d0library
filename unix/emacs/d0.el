;
;  D0 specific alterations to the emacs environment:
;    C-z (or ^z) is mapped to a d0 specific keymap.
;
;    (a) Cause files with extensions .for, .inc, .lnk and .params to use
;           FORTRAN major mode.
;    (b) Add ^c^h binding in fortran mode which inserts standard D0-style
;           headers for SUBROUTINES, FUNCTIONS and PROGRAMS.
;    (c) Redefine the fortran abbrevs ;su, ;f and ;p to insert D0-style
;           headers.
;    (d) The key sequence ^z^z prompts the user for a bank name and loads
;           the corresponding bank documentation.
;    (e) The key sequence ^z^e loads code from d0library (Via entry_point.lis)
;    (f) Add ^c^h binding for headers for c, c++ and csh files
;
;    Redfines the global key ^z to point to D0 specific keymap
;
;  Created   4-Jan-1994   John D. Hobbs
;  Modified  8-Aug-1995   John D. Hobbs - Check system type for use on VAX
;    all system depencences can be found by searching for "vax-vms"
;  Modified  1-Mar-1996   John D. Hobbs - Added C++ and csh modes.
;  Modified 23-Apr-1996   John D. Hobbs - Fix extra dashes in csh header.
;    previously added python mode and additional known extensions.
;  Modified 17-Sep-1996   John D. Hobbs - Add t arguement to load of modes
;  Modified  5-May-1997   JDH - New standard header format for c++.  Added
;    block indentation abbrevs for fortran
;  Modified 28-Aug-1997   JDH - Insert language header if file is new
;  Modified 27-Oct-1997   JDH - Add new file extensions for C++ mode and
;    insert initial header for .cpp and .hpp
;  Modified 05-Dec-1997   JDH - Fix bug in fortran-file-not-found when asking
;    user for type (subroutine, function, program)

;;; Make sure usual fortran mode is defined but change indentation first
 
(defvar fortran-do-indent 2)
(defvar fortran-if-indent 2)
 
;; -- Make sure these are loaded in order to make D0 extensions work...
;; -- Keep going if they don't exist

(load "fortran" t)
(load "cc-mode" t)
(load "python-mode" t)

;;;; ---------------------  General D0 definitions ---------------------------

;; Insert our extensions at the start, not the end, so that we can override
;; some pre-existing definitions (.c, .C, .h and .H)...
(setq auto-mode-alist
      (append '(("\\.f$" . fortran-mode)
		("\\.for$" . fortran-mode)
		("\\.lnk$" . fortran-mode)
		("\\.params$" . fortran-mode)
		("\\.inc$" . fortran-mode)
;;       How many wacko ways are there to use c++ extensions?
		("\\.C$"   . c++-mode)
		("\\.cc$"  . c++-mode)
		("\\.CC$"  . c++-mode)
		("\\.cpp$" . c++-mode)
		("\\.cxx$" . c++-mode)
		("\\.icc$" . c++-mode)
		("\\.ipp$" . c++-mode)
		("\\.tcc$" . c++-mode)
		("\\.H$"   . c++-mode)
		("\\.hh$"  . c++-mode)
		("\\.HH$"  . c++-mode)
		("\\.hpp$" . c++-mode)
		("\\.h\\+\\+$" . c++-mode)
;;       Some slightly unusual types
		("\\.py$"  . python-mode)
                ("\\.csh$" . csh-mode)
		)auto-mode-alist)) ;; The existing definitions

(setq require-final-newline t)

;;;; ------------------ D0 Compilation Commands  -----------------------------

(setq c-mode-hook
   '(lambda () (or (file-exists-p "makefile") (file-exists-p "Makefile")
      (progn (make-local-variable 'compile-command)
			     (setq compile-command
				    (concat "$cc -c $ccflags "
					    buffer-file-name))))))

(setq c++-mode-hook
   '(lambda () (or (file-exists-p "makefile") (file-exists-p "Makefile")
      (progn (make-local-variable 'compile-command)
			     (setq compile-command
				    (concat "g++ -c -g "
					    buffer-file-name))))))

(setq fortran-mode-hook
   '(lambda () (or (file-exists-p "makefile") (file-exists-p "Makefile")
      (progn (make-local-variable 'compile-command)
			     (setq compile-command
				    (concat "fort -g "
					    buffer-file-name))))))
 
;;;; --------------  System dependent definitions ----------------------------


(defun vms-system ()
  "Define system dependent variables for VMS systems"
  (interactive)
  (message "VMS")
  (defvar d0-username (user-login-name)) ; Used by standard headers
  (defvar d0docs "D0$DOCS:")
  (defvar d0zeb  "D0$ZEB$ROOT:[000000...]")
  (defvar d0egrep "search ")
  (setq floc (substitute-in-file-name "D0$UNIX$EMACS:csh-mode.el" ))
  (if (file-readable-p floc) 
      (load-file floc)
    (message "No csh-mode found"))
)

(defun unix-system ()
  "Define system dependent variables for UNIX systems"
  (interactive)
  (message "Unix")
  (defvar d0-username (user-full-name)) ; Used by standard headers
  (defvar d0docs "$d0library/docs/")
  (defvar d0zeb  "$d0library/zeb/")
  (defvar d0egrep "egrep -i \'^[a-z]*")
  (if (getenv "my_emacs")
   (setq flocal (substitute-in-file-name "$my_emacs/csh-mode.el" ))
   (setq flocal "this-is-a-dummy-file-name-and-wont-match-anything-xxjdhxx"))
  (setq ftest (substitute-in-file-name "$d0test/unix/emacs/csh-mode.el" ))
  (setq fprod (substitute-in-file-name "$d0library/unix/emacs/csh-mode.el" ))
  (cond
   ((file-readable-p flocal) (load-file flocal))
   ((file-readable-p ftest)  (load-file ftest))
   ((file-readable-p fprod)  (load-file fprod))
   (t (message "No csh-mode.el found.")))
)

(if (string= system-type "vax-vms") 
  (vms-system) 
  (unix-system))

;;;; -------------- Loading standard routines and zebra docs ----------------
 

;; The rest of the d0 library interaction setup

(defvar zeb-doc-buffer "*D0ZebDocInternal*")
(defvar d0-entry-string "*D0EntryBuffer*")

(defvar d0-entry-point-file  
       (substitute-in-file-name (concat d0docs "entry_point.lis")))
(defvar d0-unix-entry-point-file 
       (substitute-in-file-name (concat d0docs "unix_entry_point.lis")))
 
(setq d0-entry-buffer nil)
 
(defvar d0-keymap ()
  "Keymap used for general d0 functions.")
 
(if d0-keymap ()
  (setq d0-keymap (make-sparse-keymap))
  (define-key d0-keymap "\C-z" 'd0-zeb-docs)
  (define-key d0-keymap "\C-e" 'd0-entry-points)
)
 
(global-set-key "\C-z" d0-keymap)
 
;;; Main extension routines: Zebra documentation loading
 
(defun d0-zeb-docs (bank)
   "Load the bank documentation for a given bank."
   (interactive "sWhich bank? ")
   (if (not (get-buffer zeb-doc-buffer)) (d0-get-zeblist))
   (setq zeb-file (concat (downcase bank) ".zeb"))
   (set-buffer zeb-doc-buffer)
   (goto-char (point-min))
   (if (search-forward zeb-file nil nil nil)
     (let ((endp (point)))
       (beginning-of-line)
       (find-file-read-only (buffer-substring (point) endp)))
   ())
)
  
 
(defun d0-get-zeblist ()
   "Create a buffer containing a list of all .zeb files in /d0library/zeb..."
   (message "Creating list of documentation files...")
   (setq directory (substitute-in-file-name d0zeb))
   (message (concat "Searching for .zeb files in tree: " directory))
   (if (string= system-type "vax-vms") 
     (call-process "directory/nohead/notrail" nil 
         (get-buffer-create zeb-doc-buffer) nil (concat directory "*.zeb"))
     (call-process "find" nil  (get-buffer-create zeb-doc-buffer) nil
          directory "-name" "*.zeb" "-print"))
)
 
;;; Main extension routines: Reading source code

(defun d0-entry-points (ename)
  "Some definition"
  (interactive "sWhich entry point? ")
  (set-buffer (get-buffer-create d0-entry-string))
  (message (concat "Scanning for " ename))
  (if (string= system-type "vax-vms")
     (shell-command  (concat d0egrep d0-entry-point-file " " ename)  t)
     (shell-command  (concat d0egrep ename "' " d0-entry-point-file)  t))
  (message (concat "Scanning for " ename))
  (setq cl (int-to-string (count-lines (point-min) (point-max))))
  (cond ((equal cl "0") 
        (message (concat "No matches for entry point: " ename)))
     ((equal cl "1") 
        (d0-load-entry-point-source (buffer-substring (point) (point-max))))
     (t (d0-choose-source-file))))

(defun d0-load-entry-point-source (fname) 
  "Load the source file containing a given entry point"
  (message (concat "Loading: " fname))
  (setq buffer-read-only nil)
  (erase-buffer)
  (string-match "\\( \\)\\([A-Z].*$\\)" fname)
  (setq lfname (substring fname (match-beginning 2) (match-end 2)))
  (if (string= system-type "vax-vms") () 
     (setq lfname (d0-parse-to-unix lfname)))
  (message (concat "Loading: " lfname))
  (set-buffer-modified-p nil)
  (kill-buffer (get-buffer d0-entry-point-file))
  (find-file-read-only lfname)
  (message (concat "Loaded: " lfname))) 

(defun d0-choose-source-file () 
  "Choose a source file from a list of choices"
  (message "Move cursor to desired entry and select with <CR>")
  (switch-to-buffer (get-buffer d0-entry-string))
  (setq buffer-read-only t)
  (d0-mode))

(defun d0-parse-to-unix (lfname)
  "Convert a VMS file name to its unix equivalent"
  (call-process "vff" nil t nil lfname)
  (end-of-line) (backward-char)
  (setq lfname (buffer-substring (point-min) (point)))
  (message lfname)
)

;; Define D0-mode which is used to get the file name from a list of file names

(defvar d0-mode-map nil "")
(if d0-mode-map () (setq d0-mode-map (make-sparse-keymap)))
(define-key d0-mode-map "f" 'd0-choose-entry)
(define-key d0-mode-map "\012" 'd0-choose-entry)
(define-key d0-mode-map "\015" 'd0-choose-entry)

(defun d0-mode ()
  "Major mode for choosing a source file from a list of D0 entry points"
   (use-local-map d0-mode-map)
   (setq  mode-name "D0")
   (setq major-mode 'd0-mode))

(defun d0-choose-entry ()
  "Use the current position in the entry list buffer to choose load file"
   (interactive)
  (beginning-of-line) (setq lb (point))
  (end-of-line) (setq le (point))
  (d0-load-entry-point-source (buffer-substring lb le)))


;;;; ------------------ D0 Fortran specific definitions ---------------------
 
 
;;; Add some extra definitions
 
(define-key fortran-mode-map "\C-c\C-h" 'fortran-skeleton)
(define-abbrev fortran-mode-abbrev-table  ";su"  "" 'fortran-su-abbrev)
(define-abbrev fortran-mode-abbrev-table  ";f"  "" 'fortran-f-abbrev)
(define-abbrev fortran-mode-abbrev-table  ";p"  "" 'fortran-p-abbrev)
(define-abbrev fortran-mode-abbrev-table  ";i"  "" 'fortran-ife-abbrev)
(define-abbrev fortran-mode-abbrev-table  ";el"  "elseif() then" )
(define-abbrev fortran-mode-abbrev-table  ";dw"  "" 'fortran-dw-abbrev)
 
(defun fortran-su-abbrev () (fortran-skeleton "S"))
(defun fortran-f-abbrev () (fortran-skeleton "F"))
(defun fortran-p-abbrev () (fortran-skeleton "P"))

(defun fortran-ife-abbrev ()
  (insert "if() then\n")
  (insert "else\n")
  (insert "endif\n")
  (push-mark (point) t)
  (forward-line -3)
  (indent-region (point) (mark t) nil)
  )

(defun fortran-dw-abbrev ()
  (let ((cpos (point)))
    (insert "do while()\n")
    (insert "enddo\n")
    (indent-region cpos (point) (+ (calculate-fortran-indent) 2))))
 
(setq fortran-header-prefix "")
(setq fortran-header-appendix "")
 
 
;;; Subroutines used by the fortran extensions
 
(defun fortran-file-not-found ()
   "Hook routine to run if a fortran file cannot be found"
   (interactive)
 (if (or (string-match "\.for$" buffer-file-name)
       (string-match "\.f$" buffer-file-name))
   (fortran-skeleton-dispatch (read-string "Header type (P,S,F) " "S"))))
 
(defun fortran-skeleton (htype)
   "Determine the header to be inserted in a fortran source file"
   (interactive "*sHeader type (P,S,F:[S]) ")
   (fortran-skeleton-dispatch htype)
)
 
(defun fortran-skeleton-dispatch (inansw)
   "Intermediate call between fortran-skeleton and specific routines"
   (let ((answ (upcase inansw)))
      (cond
          ((string= answ "P") (command-execute 'fortran-p-skeleton) )
          ((string= answ "S") (command-execute 'fortran-su-skeleton) )
          ((string= answ "F") (command-execute 'fortran-f-skeleton) )
          ( t (command-execute 'fortran-su-skeleton) )
      )
   )
)
 
 
(defun fortran-p-skeleton (pname)
   "Insert a fortran PROGRAM header"
   (interactive "*sEnter program name: ")
   (d0-position-for-skeleton)
   (insert fortran-header-prefix)
   (insert "      PROGRAM ") (insert (upcase pname)) (newline)
   (fortran-skeleton-body)
   (insert " 999  CONTINUE\n")
   (insert "      END\n")
   (insert fortran-header-appendix)
)
 
(defun fortran-su-skeleton (sname)
   "Insert a fortran SUBROUTINE header"
   (interactive "*sEnter subroutine name: ")
   (d0-position-for-skeleton)
   (insert fortran-header-prefix)
   (insert "      SUBROUTINE ") (insert (upcase sname)) (newline)
   (fortran-skeleton-body)
   (insert "  999 RETURN\n")
   (insert "      END\n")
   (insert fortran-header-appendix)
)
 
(defun fortran-f-skeleton (fname ftype)
   "Insert a fortran FUNCTION header"
   (interactive "*sEnter function name:
sEnter function type: " )
   (d0-position-for-skeleton)
   (insert fortran-header-prefix)
   (insert "      ") (insert (upcase ftype)) (insert " FUNCTION ")
        (insert (upcase fname)) (newline)
   (fortran-skeleton-body)
   (insert " 999  RETURN\n")
   (insert "      END\n")
   (insert fortran-header-appendix)
)
 
;;; Utility routines
 
(defun fortran-skeleton-body ()
   "Insert the generic portion of the fortran header"
   (setq cline (concat "C--------------------------------------------"
                        "---------------------------\n"))
   (insert cline )
   (insert "C-\n")
   (insert "C-   Purpose and Methods: \n")
   (insert "C-\n")
   (insert "C-   Inputs  :\n")
   (insert "C-   Outputs :\n")
   (insert "C-   Controls:\n")
   (insert "C-\n")
   (insert "C-   Created  ") (insert (d0-date-string) )
        (insert "   ") (insert d0-username) (newline)
   (insert "C-\n")
   (insert cline )
   (insert "      IMPLICIT NONE\n")
   (insert cline )
)
 
 
;;; --------------------- C Mode header ------------------------------
 
(defun c-file-not-found ()
   "Hook to be called if a new C language source or header file is edited"
   (if (or (string-match "\.c$" buffer-file-name)
	   (string-match "\.h$" buffer-file-name))
       (c-header-skeleton)))
 
;;JDH no longer automatically created in v20.3
(define-key c-mode-map "\C-c\C-h" 'c-skeleton)
(setq c-header-prefix "")
(setq c-header-appendix "")

(defun c-header-skeleton ()
  "Add a file header to a c (.c or .h) file" 
  (interactive)
   (d0-position-for-skeleton)
   (insert "/*\n")
   (insert " * $Id$\n")
   (insert " *-\n")
   (insert " *-   Purpose and Methods: \n")
   (insert " *-\n")
   (insert " *-   Inputs  :\n")
   (insert " *-   Outputs :\n")
   (insert " *-   Controls:\n")
   (insert " *-\n")
   (insert " *-   Created  ") (insert (d0-date-string) )
        (insert "   ") (insert d0-username) (newline)
   (insert " *- $Revision$\n")
   (insert " *-\n*/\n")
   (insert c-header-appendix))

(defun c-skeleton (pname)
   "Insert a C function header"
   (interactive "*sEnter function name: ")
   (d0-position-for-skeleton)
   (insert c-header-prefix)
   (insert "/*\n")
   (insert " *-\n")
   (insert " *-   Purpose and Methods: \n")
   (insert " *-\n")
   (insert " */\n")
   (insert (concat pname "()\n") )
   (insert "{\n")
   (insert "}\n")
   (insert c-header-appendix)
)
 
;;; --------------------- C++ Mode header ------------------------------
 
;JDH No longer automatically created in v20.3
(define-key c++-mode-map "\C-c\C-h" 'c++-mini-skeleton)
(setq c++-header-prefix "")
(setq c++-header-appendix "")
 
(defun c++-file-not-found ()
 "Do this if a new C++ (mode) file is loaded"
 (interactive)
 (if ( or ( or (or (string-match "\.cc$" buffer-file-name)
		   (string-match "\.hh$" buffer-file-name))
	       (string-match "\.cpp$" buffer-file-name))
	  (string-match "\.hpp$" buffer-file-name))
   (c++-choose-skeleton)))

(defun c++-choose-skeleton ()
  "Decide if it's a header or source file"
  (interactive)
  (if (or (string-match "\.hh$" buffer-file-name)
	  (string-match "\.hpp$" buffer-file-name))
      (c++-include-header-skeleton) (c++-header-skeleton)))

(defun c++-header-skeleton ()
  "Insert a D0-style C++ file header now"
   (interactive)
   (d0-position-for-skeleton)
   (insert c++-header-prefix)
   (insert "//\n")
   (insert "// $Id$\n")
   (insert "//\n")
   (insert "// File: ") (insert (file-name-nondirectory buffer-file-name))
       (insert "\n")
   (insert "// Purpose: \n")
   (insert "// Created: ") (insert (d0-date-string) )
        (insert "   ") (insert d0-username) (newline)
   (insert "//\n")
   (insert "// $Revision$\n")
   (insert "//\n")
;   (insert "// $Log$\n")
   (insert "//\n\n")
   (insert "// Include files\n\n")
   (insert "// Global definitions\n\n")
   (insert "// Constructors/Destructors\n\n")
   (insert "// Accessors\n\n")
;   (message (concat "File: " (file-name-nondirectory buffer-file-name))) 
)
 
(defun c++-include-header-skeleton ()
  "Insert a D0-style C++ file header now"
   (interactive)
   (d0-position-for-skeleton)
   (insert c++-header-prefix)
   (setq tagext "_XX")
   (if (string-match "\.hh$" buffer-file-name) (setq tagext "_HH")(message " "))
   (if (string-match "\.hpp$" buffer-file-name) (setq tagext "_HPP")(message " "))
   (setq tagname (concat  (upcase (file-name-sans-extension 
				   (file-name-nondirectory buffer-file-name)))
			  (upcase tagext)))
   (insert "#ifndef ") (insert tagname) (insert "\n")
   (insert "#define ") (insert tagname) (insert "\n")
   (insert "//\n")
   (insert "// $Id$\n")
   (insert "//\n")
   (insert "// File: ") (insert (file-name-nondirectory buffer-file-name))
       (insert "\n")
   (insert "// Purpose: \n")
   (insert "// Created: ") (insert (d0-date-string) )
        (insert "   ") (insert d0-username) (newline)
   (insert "//\n")
   (insert "// $Revision$\n")
   (insert "//\n")
;   (insert "// $Log$\n")
   (insert "//\n\n")
   (insert "// Include files\n\n")
   (insert "// Global definitions\n\n")
   (insert "// Constructors/Destructors\n\n")
   (insert "// Accessors\n\n")
   (insert "#endif //") (insert tagname) (insert "\n")
;   (message (concat "File: " (file-name-nondirectory buffer-file-name))) 
)
 
(defun c++-mini-skeleton (pname)
   "Insert a C++ function/global/ header"
   (interactive "*sEnter C++ function name: ")
   (d0-position-for-skeleton)
   (insert c++-header-prefix)
   (insert (concat pname "()\n") )
   (insert "//\n")
   (insert "// Purpose: \n")
   (insert "//\n")
   (insert "// Arguements:\n")
   (insert "//\n")
   (insert "// Returns:\n")
   (insert "//\n")
   (insert "{\n")
   (insert "}\n")
   (forward-line -8) (end-of-line)
)
  
;;; --------------------- Csh Mode header ------------------------------

;; Add a header if the file is new

(defun csh-file-not-found ()
   "Hook to be called if a new csh file is edited"
 (if (string-match "\.csh$" buffer-file-name)
   (csh-skeleton)))

 
;; Now do the definitions...

(define-key csh-mode-map "\C-c\C-h" 'csh-skeleton)
(setq csh-header-prefix "")
(setq csh-header-appendix "")
 
(defun csh-skeleton ()
   "Insert a D0 standard csh header"
   (interactive)
   (d0-position-for-skeleton)
   (insert csh-header-prefix)
   (csh-skeleton-body)
   (insert csh-header-appendix)
   (message "Inserted .csh header")
)
 
(defun csh-skeleton-body ()
   "Insert the generic portion of a csh header"
   (insert "#! /bin/csh -f\n" )
   (insert (concat "# --------------------------------------------"
                        "---------------------------------\n"))
   (insert "#\n")
   (insert "# Name       : ") 
   (insert (file-name-nondirectory (buffer-file-name))) (newline)
   (insert "#\n")
   (insert "# Purpose    : \n")
   (insert "#\n")
   (insert "# Usage      :\n")
   (insert "#\n")
   (insert "# Options    :\n")
   (insert "#\n")
   (insert "# Arguements :\n")
   (insert "#\n")
   (insert "# Created    : ") (insert (d0-date-string) )
        (insert "   ") (insert d0-username) (newline)
   (insert "#\n")
   (insert (concat "# --------------------------------------------"
                        "---------------------------------\n"))
)
 
;;; Generic header utilities
 
 (defun d0-position-for-skeleton ()
   (if (not 'bolp) (newline) ())
)
 
(defun d0-date-string ()
  "Format the date string to use with the standard D0 fortran headers"
  (let ((oldbuf (current-buffer)))
     (if (string= system-type "vax-vms")
       (call-process "show" nil (get-buffer-create "*D0FortranDateBuffer*")
            nil "time")
       (call-process "date" nil (get-buffer-create "*D0FortranDateBuffer*")
            nil "+  %d-%h-%Y %T"))
     (set-buffer "*D0FortranDateBuffer*")
     (setq time-string (upcase (buffer-substring 3 14)))
     (set-buffer-modified-p nil)
     (kill-buffer "*D0FortranDateBuffer*")
     (set-buffer oldbuf))
    (concat time-string))


;;; Register file-not-found routines.

;(add-hook 'find-file-not-found-hooks 'c-file-not-found )
(add-hook 'find-file-not-found-hooks 'csh-file-not-found )
(add-hook 'find-file-not-found-hooks 'c++-file-not-found )
(add-hook 'find-file-not-found-hooks 'fortran-file-not-found )
