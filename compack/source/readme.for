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
C&IF VAXVMS
      INCLUDE '($TRMDEF)'
      INCLUDE '($SMGDEF)'
      INCLUDE '($SMGMSG)'
C&ELSE
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
      INTEGER*2 TCODE2, INLEN2
      INTEGER TCODE
      INTEGER SMG$READ_STRING,ISTAT,TRULEN,POSOUT,INLEN,I,RECOUN
      INTEGER SMG$RETURN_INPUT_LINE, SMG$PUT_LINE, IJ,IK,TCOFLG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IUNI,IERR,LIBREP
      CHARACTER*132 OUTTXT,INIUSE
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        TCOFLG=0
        TCOFLG=IBSET(TCOFLG,2)
        TCOFLG=IBSET(TCOFLG,13)
        TCOFLG=IBSET(TCOFLG,18)
        TCOFLG=IBSET(TCOFLG,23)
        TCOFLG=IBSET(TCOFLG,26)
      ENDIF
      OUTSTR=' '
      RECOUN=0
      INIUSE=' '
      IF(FULL.AND.TRMFLG) THEN     !TRMFLG set when reading FROM TERMINAL
        OUTTXT=' '
   10   CONTINUE
        IJ=TRULEN(INIUSE)
        IK=TRULEN(PROMPT)
        IF(IK.GT.0) THEN
C&IF VAXVMS
          ISTAT=SMG$READ_STRING(KEYID,OUTTXT,PROMPT(1:MIN(PBCOLS-3,
     &      IK+1)),132,TRM$M_TM_NORECALL,,
     &      %DESCR(TCOFLG),INLEN2,TCODE2,
     &      MAINID,INIUSE(1:IJ),,)
C&ELSE
C&          ISTAT=SMG$READ_STRING(KEYID,OUTTXT,PROMPT(1:MIN(PBCOLS-3,
C&     &      IK+1)),132,TRM$M_TM_NORECALL,%VAL(0),
C&     &      TCOFLG,INLEN2,TCODE2,
C&     &      MAINID,INIUSE(1:IJ),%VAL(0),%VAL(0))
C&ENDIF
        ELSE
C&IF VAXVMS
          ISTAT=SMG$READ_STRING(KEYID,OUTTXT,,132,
     *            TRM$M_TM_NORECALL,,%DESCR(TCOFLG),
     *            INLEN2,TCODE2,MAINID,INIUSE(1:IJ),,)
C&ELSE
C&          ISTAT=SMG$READ_STRING(KEYID,OUTTXT,'',132,
C&     *            TRM$M_TM_NORECALL,%VAL(0),TCOFLG,
C&     *            INLEN2,TCODE2,MAINID,INIUSE(1:IJ),%VAL(0),%VAL(0))
C&ENDIF
        ENDIF
        TCODE = TCODE2
        INLEN = INLEN2
        IF(MOD(ISTAT,2).NE.0) THEN
          IF (TCODE .NE. SMG$K_TRM_CR)
     &      ISTAT = SMG$PUT_LINE(MAINID, ' ',
     &      %VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0))
C&IF VAXVMS
          IF(TCODE.EQ.274.OR.TCODE.EQ.2) THEN          ! Sometimes CTRL/B
C                                                      ! returned for uparrow key
            RECOUN=RECOUN+1
            IF(RECOUN.LT.21) THEN
              ISTAT=SMG$RETURN_INPUT_LINE(KEYID,INIUSE,,RECOUN,
     *                  INLEN2)
              INLEN = INLEN2
              IJ=INLEN+LEN(PROMPT)
              IF(IJ.GE.PBCOLS) THEN
                INIUSE=INIUSE(1:INLEN-1)
              ENDIF
            ELSE
              INIUSE=' '
            ENDIF
            GOTO 10
C&ELSE
C&          IF(.FALSE.)THEN
C&ENDIF
          ELSEIF(TCODE.EQ.18.OR.TCODE.EQ.23) THEN    ! Refresh screen on
C                                                ! CTRL/R and CTRL/W
            ISTAT=LIBREP()
            GOTO 10
C&IF VAXVMS
          ELSEIF(TCODE.EQ.275) THEN
            RECOUN=RECOUN-1
            IF(RECOUN.GT.0) THEN
              ISTAT=SMG$RETURN_INPUT_LINE(KEYID,INIUSE,,RECOUN,
     *                  INLEN2)
              INLEN = INLEN2
              IJ=INLEN+LEN(PROMPT)
              IF(IJ.GE.PBCOLS) THEN
                INIUSE=INIUSE(1:INLEN-1)
              ENDIF
            ELSE
              INIUSE=' '
            ENDIF
            GOTO 10
C&ENDIF
          ELSEIF(TCODE.EQ.510) THEN
            INLEN=INLEN-1
          ELSEIF(TCODE.NE.13) THEN
            POSOUT=0
            CALL CHKCOM(TCODE,PF1,POSOUT,1,1)
            IF(PF1.EQ.0) THEN
              IF((TCODE.EQ.270.OR.TCODE.EQ.260).AND.FULSCR) THEN  !ENTER key
C                                        ! or 0 key used
                CALL REPSCR
              ENDIF
              GOTO 10
            ENDIF
          ENDIF
          CALL LIBEMV
          IF (PF1.EQ.0) THEN
            OUTSTR=OUTTXT(1:INLEN)
          ENDIF
        ELSE
C&IF VAXVMS
          IF(TCODE.EQ.26 .OR. ISTAT.EQ.SMG$_EOF) THEN !Ctrl-Z was used
C&ELSE
C&          IF(TCODE.EQ.26 .OR. ISTAT.EQ.0) THEN !Ctrl-Z was used
C&ENDIF
            PF1=4                                     !or we've reached EOF
            GOTO 5
          ELSEIF(TCODE.EQ.270.OR.TCODE.EQ.260) THEN           ! 270 returned when ENTER
C                                             ! hit, 260 for keypad 0
            CALL OUTMSG(' ')
            GOTO 10
          ELSEIF(TCODE.NE.510) THEN           ! 510 returned when
C                                             ! broadcast was trapped
            GOTO 5
          ENDIF
        ENDIF
      ELSE
C
        IF(TRMFLG.OR.(.NOT.RDCOM.AND.MAINID.EQ.0)) THEN
          WRITE(OUTTXT,1) PROMPT
    1     FORMAT('$',A,' ')
          CALL OUTMSG(OUTTXT)
        ENDIF
        READ(INPLUN,98,END=100) OUTSTR
   98   FORMAT(A)
        IF(RDCOM) THEN
          I=INDEX(OUTSTR,'!-<?')
          IF(I.GT.0) THEN
            WRITE(OUTTXT,1) PROMPT
            CALL OUTMSG(OUTTXT)
            CALL GTUNIT(555,IUNI,IERR)
            IF(IERR.EQ.0) THEN
              OPEN(IUNI,FILE='SYS$INPUT',STATUS='UNKNOWN')
              READ(IUNI,98) OUTSTR
              CLOSE(IUNI)
              CALL RLUNIT(555,IUNI,IERR)
            ENDIF
          ELSE
            I=INDEX(OUTSTR,'!-<')
            IF(I.GT.0) THEN
              OUTSTR=OUTSTR(1:I-1)
            ENDIF
          ENDIF
        ENDIF
        GOTO 5
  100   PF1=4
        CMDOPEN = .FALSE.       !End of input command file.
      ENDIF
    5 CONTINUE
      RETURN
      END
