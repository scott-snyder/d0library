      SUBROUTINE GREADS(NUMPAR,PARLIN,OUTSTR,PF1,POS1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read new parameter values for GETDIS
C-                         VAX-specific
C-
C-   Inputs  : NUMPAR: Number of parameters used
C-             PARLIN: Array of current parameter strings
C-   Outputs : OUTSTR: New string for chosen parameter
C-             PF1:    Possible PF-key number struck
C-             POS1:   Chosen position within display
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified   13-NOV-1990   Scott Snyder
C-     erase the LOCID virdisp before pasting it. required for SMG$READ_STRING
C-     to work correctly under VMS V5.4.
C-
C-     figure out the pasteboard line the cursor is on using
C-     get_pasting_info rather than using the crufty tests on the state
C-     of the display.
C-
C-     a ctrl-u will blank the entire line that the cursor is on. to
C-     make it work properly, we must set the prompt to be the entire
C-     contents of the line before the read position, and paste the read
C-     virdisp in pb column 1. note that this means that the datum
C-     currently selected will be in boldface, but i think it looks
C-     better this way anyway.
C-
C-     use SMG symbolic names for character codes.
C-
C-     removed redundant LIBERL call. reorder calls to prevent unneeded
C-     (annoying) physical screen updates.
C-
C-     if a ^Z is pressed before any other characters, the next call to
C-     smg$read_string returns with tcode = SGM$K_TRM_CTRLZ. the _next_
C-     call to smg$read_string returns with EOF status. the next call
C-     after _this_ completes normally. so, squelch that annoying error
C-     message following ^Z by performing an extra read when one is read
C-     to clear the EOF status.
C-
C-     if ^Z is pressed following other text, smg$read_string returns
C-     SMG$_EOF. treat this as an abort request rather than complaining.
C-   Modified 5-FEB-1991 Scott Snyder
C-     try to cut down on the flicker attending moving around in the
C-     menu by:
C-        - copying the prompt line from MAINID to LOCID before pasting
C-          LOCID.
C-        - the calls to smg$repaint_line seem to be unneeded, so i've
C-          removed them.
C-
C-     before doing anything else, print the current value of this
C-     parameter to the main virdisp followed by a clear-to-EOL. this is
C-     in case the user had previously entered an out of range value.
C-
C-   Updated 2-OCT-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR,PF1,POS1
      CHARACTER*(*) PARLIN,OUTSTR
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INCLUDE '($TRMDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($SMGDEF)'
      INCLUDE '($SMGMSG)'
C&ELSE
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
C
      INTEGER TCODE, INLEN, IMOD, LOCID, LINE, COL, USELEN,
     &        FLAGS, MAIN_PBROW, PBLINE, TCOFLG, IMOD1, IMOD2
      INTEGER*2 TCODE2, INLEN2
      LOGICAL ISTAT
      CHARACTER*132 OUTTXT, PROMPT
C
      INTEGER LIBERL, LIBGET, LIBPUT, LIBREP, TRULEN
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY, SMG$ERASE_DISPLAY,
     &        SMG$GET_PASTING_INFO, SMG$MOVE_TEXT,
     &        SMG$PASTE_VIRTUAL_DISPLAY, SMG$READ_FROM_DISPLAY,
     &        SMG$READ_STRING, SMG$REPAINT_LINE,
     &        SMG$UNPASTE_VIRTUAL_DISPLAY
C
      INTEGER COLUMN
      PARAMETER (COLUMN=5)
      LOGICAL FIRST
      SAVE FIRST, TCOFLG, LOCID
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        ISTAT=SMG$CREATE_VIRTUAL_DISPLAY(1,132,LOCID,
     *         SMG$M_TRUNC_ICON,%VAL(0),%VAL(0))
        IF(.NOT.ISTAT) THEN
          CALL MSGSCR(ISTAT,'GREADS-VIRT-->')
        ENDIF
        TCOFLG=0
        TCOFLG=IBSET(TCOFLG, SMG$K_TRM_CTRLB)
        TCOFLG=IBSET(TCOFLG, SMG$K_TRM_CTRLM)
C        TCOFLG=IBSET(TCOFLG, SMG$K_TRM_CTRLR) ! doesn't term. if editing on
        TCOFLG=IBSET(TCOFLG, SMG$K_TRM_CTRLW)
        TCOFLG=IBSET(TCOFLG, SMG$K_TRM_CTRLZ)
      ENDIF
      ISTAT=LIBGET(LINE,COL)
      USELEN=MIN0(TRULEN(PARLIN),PBCOLS/2-COLUMN)
      ISTAT=LIBPUT(PARLIN(1:USELEN),LINE,PBCOLS/2+COLUMN,0)
      ISTAT=LIBERL(LINE, PBCOLS/2 + COLUMN + USELEN)
    1 CONTINUE
      ISTAT = SMG$ERASE_DISPLAY(LOCID,%VAL(0),%VAL(0),%VAL(0),%VAL(0))
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
C compute the pasteboard line which corresponds to line LINE in the
C  main display.
C
      ISTAT = SMG$GET_PASTING_INFO(MAINID, PASTID, FLAGS, MAIN_PBROW,
     &                             %VAL(0))
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      PBLINE = LINE + MAIN_PBROW - 1
C
C copy the line from the main display to the LOCID virdisp and paste
C  LOCID on top of it (the line in the main display).
C
      ISTAT = SMG$MOVE_TEXT(MAINID, LINE, 1, LINE, PBCOLS,
     &                      LOCID, 1, 1, SMG$M_TEXT_SAVE)
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = SMG$PASTE_VIRTUAL_DISPLAY(LOCID, PASTID, PBLINE, 1,
     &                                  %VAL(0))
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
C read the prompt off the virdisp
C
      PROMPT = ' '
      ISTAT = SMG$READ_FROM_DISPLAY(MAINID, PROMPT, %VAL(0), LINE)
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
      OUTSTR=' '
      IMOD1=TRM$M_TM_TRMNOECHO
      IMOD2=TRM$M_TM_NORECALL
      IMOD=IOR(IMOD1,IMOD2)
C&IF VAXVMS
      ISTAT=SMG$READ_STRING(KEYID,OUTTXT,PROMPT(1:PBCOLS/2+COLUMN-1),
     &                      132,IMOD,%VAL(0),%DESCR(TCOFLG),
     *         INLEN2,TCODE2,LOCID,PARLIN(1:USELEN),SMG$M_BOLD,%VAL(0))
C&ELSE
C&      ISTAT=SMG$READ_STRING(KEYID,OUTTXT,PROMPT(1:PBCOLS/2+COLUMN-1),
C&     &                      132,IMOD,%VAL(0),TCOFLG,
C&     *         INLEN2,TCODE2,LOCID,PARLIN(1:USELEN),SMG$M_BOLD,%VAL(0))
C&ENDIF
      INLEN = INLEN2
      TCODE = TCODE2
      IF(ISTAT) THEN
        IF((TCODE.GT.255 .AND. TCODE.NE.SMG$K_TRM_BUFFER_FULL) .OR.
     &     TCODE .LT. SMG$K_TRM_CTRLZ) THEN    !Special characters
          CALL CHKCOM(TCODE,PF1,POS1,NUMPAR,1)
          IF(TCODE.NE.SMG$K_TRM_KP0 .AND. TCODE.NE.SMG$K_TRM_ENTER) THEN
C            IF(SPLFLG) THEN
C              ISTAT=SMG$REPAINT_LINE(PASTID,LINE+SPLLIN,1)
C            ELSE
C              ISTAT=SMG$REPAINT_LINE(PASTID,LINE,1)
C            ENDIF
          ELSE
            PF1=30      !Special value to force redraw of screen
          ENDIF
        ELSEIF(TCODE.EQ.SMG$K_TRM_BUFFER_FULL) THEN ! Buffer full
          INLEN=INLEN-1
        ELSEIF(TCODE.EQ.SMG$K_TRM_CTRLZ) THEN   ! CTRL/Z was used as EXIT
          PF1=4
C&IF VAXVMS
          ISTAT = SMG$READ_STRING(KEYID, PROMPT,,,,,,,,LOCID) ! clear EOF cond
C&ENDIF
        ENDIF
        OUTSTR=OUTTXT(1:INLEN)
C&IF VAXVMS
      ELSEIF(ISTAT.EQ.SS$_ABORT) THEN           !44 for abort from BROAST
        GOTO 1
      ELSE IF (ISTAT .EQ. SMG$_EOF) THEN
        PF1 = 4
C&ENDIF
      ELSE
        CALL MSGSCR(ISTAT,'GREADS-->')
      ENDIF
      IF(PF1.NE.30) THEN
        ISTAT=LIBPUT(OUTSTR(1:INLEN),LINE,PBCOLS/2+COLUMN,0)
      ENDIF
C      call smg$ring_bell(mainid, 2)
      ISTAT=SMG$UNPASTE_VIRTUAL_DISPLAY(LOCID,PASTID)
      RETURN
      END
