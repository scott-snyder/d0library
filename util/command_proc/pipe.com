$!========================================================================
$!
$! Name      : pipe.com
$!
$! Purpose   : perform a command on each of several files
$!              see PIPE.DOC
$!            This file contains the procedures for PIPE, PIPES, and PIPER
$!              to use, define symbols as follows:
$!              PIPE :== @PIPE.COM
$!              PIPER*EAD :== @PIPE PIPE_READ
$!              PIPES*EARCH :== @PIPE PIPE_SEARCH
$! Arguments :
$!              P1  action to be performed:
$!                      SET, PIPE_READ, PIPE_SEARCH, or cmd
$! SET is used to set the default operating mode of PIPE--
$!       SET    NOL*IST         no list file (DEFAULT)
$!       SET    LI*ST           send output to list file 
$!       SET    NOCONT*INUE     don't continue after first error encountered
$!       SET    CONT*INUE       continue even if an error occurs in 
$!                                 executing the command (DEFAULT)
$!       SET    CONF*IRM        request confirmation before each proceess
$!       SET    NOCONF*IRM      proceed without asking (DEFAULT)
$!       SET    SCR*EEN_INPUT   allow screen input to commands
$!       SET    NOSC*REEN_INPUT SYS$INPUT is command file (DEFALUT)
$!       SET    D*EFAULT        Return to default options
$!       SET    SP*ACE          leave a space between cmd and file (DEFAULT)
$!       SET    NOSP*ACE        no space between cmd and file
$!       SET    NODRY*_RUN      really execute the command (DEFAULT)
$!       SET    DRY*_RUN        don't do command, just show how translation
$! PIPE_READ means do the command with arguments from an
$!      argument list file
$! PIPE_SEARCH means to use SEARCH to select files
$! cmd (none of these special cases) means standard PIPE
$!
$! description of remaining arguments:
$! case: P1 = cmd       P1=PIPE_READ    P1=PIPE_SEARCH
$!       --------       ---------       -----------
$! P2    dirspec        cmd             cmd
$! P3    cmd2           filspec         searchspec
$! P4      X            cmd2            searchstr
$! P5      X             X              cmd2
$! P6-8    X             X               X
$!     
$! where
$! dirspec=     spec given to DIR to define files list (may include subdir 
$!              spec, or wildcards) it may include any qualifiers used by DIR 
$!              which pertain to file selection, such as /SINCE or /EXCLUDE
$! searchspec = filespec to SEARCH (may include qualifiers)
$! filspec    = argument file filespec
$! cmd2       = second part of command
$!                              OR
$!              NAME  to process on NAMEs of the files only, 
$!              deleting information about their subdirectory and extension
$!              FNAME   as above, but leave extension
$! searchstr  = search_string(s) passed to SEARCH
$!    X       = any additional parameters: just passed along to cmd
$!
$! Examples:
$!      PIPE SET LIST
$!      @PIPE FORTRAN/LIST *.FOR/SINCE
$!      @PIPE SET NOCONT
$!      @PIPE PIPE_SEARCH FORTRAN/LIST *.FOR new_include_file
$!      @PIPE SET DRY_RUN
$!      @PIPE RENAME *.OBJ DEB_?.OBJ
$! see PIPE.DOC for further examples
$!
$! Created 1-Jan-1988 John Gunther, Betz Laboratories, Trevose, PA (date?) 
$!      PIPE DIR and PIPE SEARCH
$! Modified 28-AUG-1990 James T. Linnemann, Michigan State University
$!      add PIPE READ, NAME, list file, set NOON; clean up SEARCH string passing
$! Modified 13-SEP-1990 Philippe Laurens (MSU)
$!      cleanup; allow recursion
$! Rewritten 18-SEP-1990 James T. Linnemann, MSU
$!      merge into a single file; add SET command, confirm
$! Modified 30-Oct-1990 James T. Linnemann, MSU
$!      add SET SCREEN option
$! Modified  2-NOV-1990 Philippe Laurens (MSU)
$!      made VMS V4.x compatible
$! Modified  15-NOV-1990 Philippe Laurens (MSU)
$!      cleanup, bug fix, add SET DEFAULT
$! Modified 15-NOV-1990   James T. Linnemann 
$!      allow users to use /EXCLUDE
$! Modified 15-NOV-1990   James T. Linnemann add FNAME, NOSPACE
$! Modified 28-Jun-1994   James T. Linnemann add matching of * -> ?, DRY_RUN
$!                                   eliminate need for """ on search strings
$!========================================================================
$!
$   ON ERROR     THEN $ GOTO exit
$   ON CONTROL_Y THEN $ GOTO exit
$!================================================
$!   set all parameters with spaces to "px" except p1, the command passed.
$!================================================
$   count = 2 ! Start with parameter P3
$!      P1, P2 can't be quoted strings, so don't add back quotes
$quote_loop:
$   count = count + 1
$   quoted_String = P'count'
$   gosub add_in_quotes
$   P'count' = quoted_String
$   if (count.LT.8) Then goto quote_loop
$!
$!
$   SAY := WRITE SYS$OUTPUT
$
$  !define_default_options_at_first-call:
$   IF ( "''pipe_options_set'" .EQS. "" ) THEN GOSUB set_default_conditions
$!be sure new option init'd in case user switches to this version in mid-session
$   IF ( "''pipe_dry'" .EQS. "" ) THEN pipe_dry == "FALSE"  ! -> not a dry run
$   
$   choice = P1
$   
$  !try_set_option:
$   IF ( choice .EQS. "SET" ) THEN GOTO change_default_options
$
$  !try_choice_SEARCH:
$   IF ( choice .NES."PIPE_SEARCH" ) THEN GOTO try_choice_READ
$     GOSUB get_search_arguments
$     name_file = "PIPE.TMP"
$     GOSUB build_name_file_with_search
$     GOTO now_ready_to_go
$
$  try_choice_READ:
$   IF ( choice .NES."PIPE_READ" ) THEN GOTO default_choice
$     GOSUB get_read_arguments
$     name_file = filspec
$     GOTO now_ready_to_go
$
$  default_choice: 
$     !Basic PIPE, with DIR doing the selection
$     GOSUB get_dir_arguments
$     name_file = "PIPE.TMP"
$     GOSUB build_name_file_with_dir
$     !GOTO now_ready_to_go
$
$  now_ready_to_go:
$   GOSUB feed_list_to_command
$
$fast_exit:
$   EXIT
$!=============================================================================
$! subroutines are listed below
$!=============================================================================
$!
$add_in_quotes:
$   if (quoted_string.EQS."") Then Return
$   quote = """
$   IF ((quoted_string-" ".NES.quoted_string).AND. -
        (quoted_string-quote.EQS.quoted_string))
$   THEN
      quoted_string = quote + quoted_string + quote
$   ENDIF
$   Return
$!=============================================================================
$!
$set_default_conditions:
$!
$   pipe_continue_on_error == "TRUE"  ! -> want NOON 
$   pipe_list              == "FALSE" ! -> no list
$   pipe_confirm           == "FALSE" ! -> no confirm
$   pipe_screen            == "FALSE" ! -> no screen input
$   pipe_space             == "TRUE"  ! -> space between cmd and file
$   pipe_dry               == "FALSE" ! -> not a dry run
$
$   pipe_options_set       == "TRUE"
$   
$  RETURN
$!
$change_default_options:
$ 
$! define symbols to recognize the option desired
$ PIPE__NOL*IST         := "NOLIST"
$ PIPE__LI*ST           := "LIST"
$ PIPE__NOCONT*INUE     := "NOCONTINUE"
$ PIPE__CONT*INUE       := "CONTINUE"
$ PIPE__CONF*IRM        := "CONFIRM"
$ PIPE__NOCONF*IRM      := "NOCONFIRM"
$ PIPE__SCR*EEN_INPUT   := "SCREEN_INPUT"
$ PIPE__NOSC*REEN_INPUT := "NOSCREEN_INPUT"
$ PIPE__D*EFAULT        := "DEFAULT_OPTIONS"
$ PIPE__SP*ACE          := "SPACES"
$ PIPE__NOSP*ACE        := "NO_SPACES"
$ PIPE__DRY*_RUN        := "DRY_RUN"
$ PIPE__NODRY*_RUN      := "NODRY"
$  
$ option = "PIPE__" + P2
$  
$! perform option if it is recognized 
$ IF ( F$TYPE('option') .NES. "" ) THEN GOTO legitimate_set_command
$
$!failure_understanding_command:
$ SAY "/''P2'/ isn't a PIPE SET option - or is ambiguous "
$ SAY "Documentation for Pipe is in D0$UTIL:PIPE.DOC"
$ SAY "If you meant to use a VMS SET command, type $ PIPE ""SET ..."""
$ GOTO fast_exit
$
$legitimate_set_command:
$ GOTO &'option' ! this uses the symbols above to pick the target
$
$NOCONTINUE:            !change the default
$ pipe_continue_on_error == "FALSE" 
$ GOTO fast_exit
$CONTINUE:              ! go back to original default
$ pipe_continue_on_error == "TRUE"   
$ GOTO fast_exit
$NOLIST:                ! go back to original the default
$ pipe_list              == "FALSE"
$ GOTO fast_exit
$LIST:                  ! change the default
$ pipe_list              == "TRUE"
$ GOTO fast_exit
$NOCONFIRM:             !go back to original default
$ pipe_confirm           == "FALSE"
$ GOTO fast_exit
$CONFIRM:               ! change the default
$ pipe_confirm           == "TRUE"
$ GOTO fast_exit
$SCREEN_INPUT:          ! change the default
$ pipe_screen            == "TRUE"
$ GOTO fast_exit
$NOSCREEN_INPUT:        !go back to original default
$ pipe_screen            == "FALSE"
$ GOTO fast_exit
$SPACES:                !go back to original default
$ pipe_space             == "TRUE"
$ GOTO fast_exit
$NO_SPACES:
$ pipe_space             == "FALSE"
$ GOTO fast_exit
$DRY_RUN:
$ pipe_dry               == "TRUE"
$ GOTO fast_exit
$NODRY:
$ pipe_dry               == "FALSE"
$ GOTO fast_exit
$DEFAULT_OPTIONS:
$ GOSUB set_default_conditions
$ GOTO fast_exit
$!=============================================================================
$!
$get_search_arguments:
$!
$   cmd       = P2      !for the SEARCH option
$   filspec   = P3
$   search_string      = P4
$   P4 = ""
$   cmd2 = P5
$   P5 = ""
$   GOSUB prompt_for_command
$!
$   IF ( filspec .EQS. "" ) THEN INQUIRE/NOPUNCTUATION filspec -
       "Select the files with wild card and/or qualifiers for $SEARCH:"
$   IF ( filspec .EQS. "" ) THEN filspec = "*.*"
$ IF search_string.EQS."" THEN INQUIRE/NOPUNCTUATION search_string -
"String(s) ? "                                     
$   RETURN
$!=============================================================================
$!
$prompt_for_command:
$!
$ask_for_command:          ! get the value of cmd        
$!
$   IF ( cmd .EQS. "" ) THEN INQUIRE cmd -
         "Enter the DCL Command to perform on all selected files:"
$   IF ( cmd .EQS. "" ) THEN GOTO ask_for_command
$   RETURN
$!=============================================================================
$!
$build_name_file_with_search:
$!
$ SEARCH 'filspec'  /OUTPUT='name_file' -
    /NONUMBERS/NOREMAINING/WINDOW=0 'search_string' 
$!
$   RETURN
$!=============================================================================
$!
$get_read_arguments:
$!
$   cmd       = P2
$   filspec   = P3
$   cmd2 = P4
$   P4 = ""
$   search_string = ""
$   GOSUB prompt_for_command
$ask_for_argument_list_file:
$   IF ( filspec .EQS. "" ) THEN INQUIRE/NOPUNCTUATION filspec -
 " Give the file with the list of arguments for the command: "
$   IF ( filspec .EQS. "" ) THEN GOTO ask_for_argument_list_file
$   RETURN
$!=============================================================================
$!
$get_dir_arguments:
$!
$   cmd       = P1
$   filspec   = P2
$   cmd2      = P3
$   search_string = ""
$   GOSUB prompt_for_command
$   IF ( filspec .EQS. "" ) THEN INQUIRE filspec -
       "Select the files with wild card and/or qualifiers for $DIR:"
$   IF ( filspec .EQS. "" ) THEN filspec = "*.*"
$   RETURN
$!=============================================================================
!
$build_name_file_with_dir:
$!
$!probably_dont_want_ALL_versions:
$ versions = "/VERSIONS=1" !by default
$! if ; in dirspec apply to spec'd versions
$ IF ( F$ELEMENT(1,";",filspec) .NES. ";" ) THEN versions = ""
$!
$ DIRECTORY 'filspec'  -
 /OUTPUT='name_file' 'versions' -
 /COLUMNS=1 /NOACL /NODATE /NOFILE_ID /NOFULL /NOGRAND_TOTAL /NOHEADING  -
 /NOOWNER /NOPROTECTION /NOSECURITY /NOSIZE /NOTOTAL /NOTRAILING
$!
$   RETURN
$!=============================================================================
$!
$feed_list_to_command:
$!
$   name_only = cmd2 .EQS. "NAME"
$   name_only = cmd2 .EQS. "NAME"
$   full_name = cmd2 .EQS. "FNAME"
$   IF name_only THEN cmd2 = ""
$   IF full_name THEN cmd2 = ""
$   IF pipe_list THEN SAY "Output will be directed to PIPE.LIS"
$   IF pipe_list THEN DEFINE SYS$OUTPUT PIPE.LIS
$
$!advertise_call_made:
$   SAY " PIPE "+cmd+" "+filspec+" "+search_string+" "  -
                        +cmd2+" "+P4+" "+P5+" "+P6+" "+P7+" "+P8
$  IF pipe_list THEN TYPE/NOPAGE 'name_file'
$  IF cmd .EQS. "NULL" THEN GOTO exit
$
$! build a symbol for the name file which varies from call to call so that
$! PIPE can be called recursively
$!
$   time = F$TIME()
$   id = F$EXTRACT(18,2,time) + F$EXTRACT(21,2,time)
$   name_file_symbol = "name_file''id'"
$!
$!is a pattern match requested?
$  oldcmd2 = cmd2
$  IF ((cmd2-"?").NES.cmd2)
$  THEN
$! find matching pattern from input 
$   fspc = f$extract(0,f$locate("/",filspec),filspec) !remove qualifiers
$   namtyp = f$parse(fspc,,,"NAME","SYNTAX_ONLY") +  -
        f$parse(fspc,,,"TYPE","SYNTAX_ONLY")
$   before = f$extract(0,f$locate("*",namtyp),namtyp) 
$   after =  f$extract(1+f$locate("*",namtyp),f$length(namtyp),namtyp)
$  ENDIF
$!read_list_of_file_names:
$   OPEN/READ 'name_file_symbol' 'name_file'
$
$next_file:
$
$       READ/END_OF_FILE=exit 'name_file_symbol' file
$       fnam = F$PARSE(file,,,"NAME") + F$PARSE(file,,,"TYPE")
$       IF "''fnam'" .eqs. "''name_file'" THEN GOTO next_file
$       IF "''fnam'" .eqs. "PIPE.LIS" THEN GOTO next_file
$       IF  name_only THEN  file = F$PARSE(file,,,"NAME")
$       IF  full_name THEN  file = F$PARSE(file,,,"NAME") + F$PARSE(file,,, -
            "TYPE")
$!try pattern matching on 
$       cmd2 = oldcmd2
$       IF ((cmd2-"?").NES.cmd2)
$       THEN
$          star = F$PARSE(file,,,"NAME")+F$PARSE(file,,,"TYPE") - before - after
$          SAY " Changing ? to ''star'"
$          newcmd2 = f$extract(0,f$locate("?",cmd2),cmd2) + star -
                    + f$extract(1+f$locate("?",cmd2),f$length(cmd2),cmd2)
$          cmd2 = newcmd2
$       ENDIF
$!advertise current action taken
$ IF pipe_space THEN -
 SAY "$ "+cmd+" "+file+" "+cmd2+" "+P4+" "+P5+" "+P6+" "+P7+" "+P8
$ IF .NOT. pipe_space THEN -
 SAY "$ "+cmd+file+" "+cmd2+" "+P4+" "+P5+" "+P6+" "+P7+" "+P8
$       IF pipe_list THEN WRITE SYS$ERROR "Processing " + file
$       IF .not. pipe_confirm THEN GOTO go_for_it
$ WRITE SYS$ERROR cmd+" "+file+" "+cmd2+" "+P4+" "+P5+" "+P6+" "+P7+" "+P8
$             INQUIRE wantit "Proceed? [N]"  
$             IF .NOT.wantit THEN GOTO next_file
$go_for_it:
$       IF pipe_dry THEN GOTO next_file
$       IF pipe_continue_on_error THEN SET NOON
$       IF pipe_screen THEN DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$       IF pipe_space THEN 'cmd' 'file' 'cmd2' 'P4' 'P5' 'P6' 'P7' 'P8'
$       IF .NOT. pipe_space THEN 'cmd''file' 'cmd2' 'P4' 'P5' 'P6' 'P7' 'P8'
$       SET ON
$       cmd2 = oldcmd2
$ GOTO next_file
$
$exit:
$   IF pipe_dry THEN SAY "Dry run.  Do PIPE SET NODRY to actually execute"
$   IF ( F$TYPE(name_file_symbol)   .NES. "" ) THEN CLOSE 'name_file_symbol'
$   IF ( F$SEARCH(name_file) .NES. "" ) .AND. ( choice .NES. "PIPE_READ" ) -
                THEN DELETE 'name_file';*
$   IF pipe_list THEN DEASSIGN SYS$OUTPUT
$   RETURN
