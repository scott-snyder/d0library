      SUBROUTINE CHKCOM(TCODE,PF1,POSLOC,MAXIN,NUMIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECK string for up and down cursor or PF-keys.
C-
C-   Inputs  : TCODE: Terminator code from SMG input
C-             MAXIN: Maximum number of items in menu display
C-             NUMIN: Number of columns in menu display
C-             POSLOC:   Current position in display
C-   Outputs : PF1 :   Number of PF-key struck (if any)
C-             POSLOC:   New position in display if arrow key was used
C-   Controls:
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated  18-SEP-1990   Harrison B. Prosper
C-      Added TCODE -1 for event flag and timeout
C-   Updated   7-NOV-1990    Scott Snyder: changed to SMG$ symbolic
C-    constants; added window scrolling keys.
C-   Revised  31-MAR-1991    Scott Snyder
C-    Go to the end of the message window if the user presses an arrow
C-    or PF key.
C-   Revised   2-APR-1991    Scott Snyder
C-    Add LIBIOF/LIBIND calls around ENTER and KP0 handling.
C-   Revised   9-MAY-1991    Scott Snyder
C-    Return PF1=4 for ^Z.
C-   Updated  20-OCT-1991   Harrison B. Prosper
C-    Better handling of event flag PF1 value
C-   Updated  31-OCT-1991   Herbert Greenlee  
C-    Add machine blocks for UNIX 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TCODE,PF1,POSLOC,MAXIN,NUMIN,TC
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
      INTEGER I,LIBREP,LIBRES,LIBSAV
      LOGICAL INTAST,GETSPL
C----------------------------------------------------------------------
      IF((TCODE.EQ.SMG$K_TRM_UP .OR. TCODE.EQ.SMG$K_TRM_CTRLB)  ! Sometimes
     *    .AND.POSLOC.GT.NUMIN) THEN          ! CTRL/B returned for uparrow key
        POSLOC=POSLOC-NUMIN
      ELSEIF (TCODE.EQ.SMG$K_TRM_DOWN .AND.
     &        POSLOC.LT.(MAXIN-(NUMIN-1))) THEN
        POSLOC=POSLOC+NUMIN
      ELSEIF (TCODE.EQ.SMG$K_TRM_RIGHT .AND.
     &        MOD(POSLOC,NUMIN).NE.0) THEN
        POSLOC=POSLOC+1
      ELSEIF (TCODE.EQ.SMG$K_TRM_LEFT .AND.
     &        MOD(POSLOC-1,NUMIN).NE.0) THEN
        POSLOC=POSLOC-1
      ELSEIF (TCODE.EQ.SMG$K_TRM_PF1 .OR. TCODE.EQ.SMG$K_TRM_DO) THEN
        PF1=1
      ELSEIF(TCODE.EQ.SMG$K_TRM_PF2 .OR. TCODE.EQ.SMG$K_TRM_HELP) THEN
        PF1=2
      ELSEIF(TCODE.EQ.SMG$K_TRM_PF3) THEN
        PF1=3
      ELSEIF(TCODE.EQ.SMG$K_TRM_PF4 .OR. TCODE.EQ.SMG$K_TRM_CTRLZ) THEN
        PF1=4
C
C ****  Handle Event flag commands
C
      ELSEIF(TCODE.LT.0) THEN          ! Event flag
        PF1=-1
        TC = IABS(TCODE)
        IF    (TC.EQ.SMG$K_TRM_PF1 .OR. TC.EQ.SMG$K_TRM_DO) THEN
          PF1=-1
        ELSEIF(TC.EQ.SMG$K_TRM_PF2 .OR. TC.EQ.SMG$K_TRM_HELP) THEN
          PF1=-2
        ELSEIF(TC.EQ.SMG$K_TRM_PF3) THEN
          PF1=-3
        ELSEIF(TC.EQ.SMG$K_TRM_PF4 .OR. TC.EQ.SMG$K_TRM_CTRLZ) THEN
          PF1=-4
        ENDIF
      ENDIF
C
C The keystrokes listed in the following cases are the ones for which
C  we should _not_ pop the message window back to the end.  All other
C  keys fall through to the trailing else, where we move the message
C  window to the end.
C
      IF(TCODE.EQ.SMG$K_TRM_CTRLR .OR.
     &   TCODE.EQ.SMG$K_TRM_CTRLW) THEN     ! Refresh screen on
C                                           ! CTRL/R and CTRL/W
        I=LIBREP()
      ELSEIF(TCODE.EQ.SMG$K_TRM_ENTER) THEN      ! Toggle split screen
C                                                ! on ENTER
        CALL LIBIOF
        CALL SAVSCR
        IF(SPLFLG) THEN
          CALL ENDSPL
        ELSE
          CALL SPLTIT
        ENDIF
        CALL LIBIND
      ELSEIF(TCODE.EQ.SMG$K_TRM_KP0) THEN        ! Toggle STATUS screen
C                                                ! on Keypad 0
        CALL LIBIOF
        CALL SAVSCR
        IF(STAFLG) THEN
          CALL ENDSTA
        ELSE
          CALL SPLSTA
        ENDIF
        CALL LIBIND
      ELSEIF (TCODE .EQ. SMG$K_TRM_NEXT_SCREEN .OR.
     &         TCODE .EQ. SMG$K_TRM_KP3) THEN
        CALL LIBPMV(+1)
      ELSEIF (TCODE .EQ. SMG$K_TRM_PREV_SCREEN .OR.
     &         TCODE .EQ. SMG$K_TRM_KP9) THEN
        CALL LIBPMV(-1)
      ELSEIF (TCODE .EQ. SMG$K_TRM_KP7) THEN
        CALL LIBHMV
      ELSEIF (TCODE .EQ. SMG$K_TRM_KP1) THEN
        CALL LIBEMV
      ELSE
        CALL LIBEMV
      ENDIF
      IF(POSLOC.GT.MAXIN) POSLOC=MAXIN
      IF(PF1.EQ.0.AND.INTAST()) CALL SETFLG       ! To avoid redrawing menu in AST mode
      RETURN
      END
