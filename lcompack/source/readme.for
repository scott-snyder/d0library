      SUBROUTINE README(OUTSTR,PF1,FULL,PROMPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read a string from terminal and interpret it
C-                         for entry of special keys. VAX-specific
C-
C-   Inputs  : FULL:   Flag set if in full screen mode
C-             PROMPT: Prompt string to issue before read
C-   Outputs : OUTSTR: Entered string (if any)
C-             PF1:    PF key number struck (if any)
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C-      27-Oct-1989 Penelope Constanta-Fanourakis
C-          Replace all reads to unit 5 with reads to
C-          INPLUN. This will allow better control of
C-          where the input is read.
C-    Modified 22-FEB-1991 Scott Snyder
C-      Call smg$put_line after the input if it wasn't terminated with
C-      a CR. Otherwise, the cursor position doesn't get reset to column
C-      1, and we risk hanging up on an INVCOL error from SMG.
C-    Revised   2-APR-1991  Scott Snyder
C-      Move smg$put_line call inside of the test of the status from
C-      smg$read_string.  Call LIBEMV after reading a line from the
C-      terminal.
C-    Modified 18-MAY-1991  Scott Snyder
C-      smg$read_string can return EOF with tcode != 26.  test for this too.
C-    Updated  26-Sep-1991  Herbert Greenlee
C-    Modified 14-AUG-1992  sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) OUTSTR,PROMPT
      INTEGER PF1
      LOGICAL FULL
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IUNI,IERR, I
      CHARACTER*132 OUTTXT,INIUSE
C----------------------------------------------------------------------
      OUTSTR=' '
      INIUSE=' '
C
      IF(TRMFLG.OR.(.NOT.RDCOM.AND.MAINID.EQ.0)) THEN
        WRITE(OUTTXT,1) PROMPT
    1   FORMAT('$',A,' ')
        CALL OUTMSG(OUTTXT)
      ENDIF
      READ(INPLUN,98,END=100) OUTSTR
   98 FORMAT(A)
      IF(RDCOM) THEN
        I=INDEX(OUTSTR,'!-<?')
        IF(I.GT.0) THEN
          WRITE(OUTTXT,1) PROMPT
          CALL OUTMSG(OUTTXT)
          READ(*,98) OUTSTR
        ELSE
          I=INDEX(OUTSTR,'!-<')
          IF(I.GT.0) THEN
            OUTSTR=OUTSTR(1:I-1)
          ENDIF
        ENDIF
      ENDIF
      GOTO 5
 100  PF1=4
      CMDOPEN = .FALSE.         !End of input command file.
    5 CONTINUE
      RETURN
      END
