      SUBROUTINE OREADS(NUMPAR,OPTNUM,OPTCUR,PFOUT,POSOUT,MAXLOC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read and interpret keystroke for GETOPT
C-
C-   Inputs  : NUMPAR: Number of parameters being used
C-             OPTNUM: Number of options for currently selected parameter
C-             OPTCUR: Currently selected option for current parameter
C-             MAXLOC: Number of lines used by current parameter
C-   Outputs : PFOUT: PF-key struck (if any)
C-             POSOUT: Number of parameter chosen
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified 30-APR-1991  Scott Snyder
C-    Add scrolling window stuff.
C-   Modified  9-MAY-1991  Scott Snyder
C-    Return PFOUT=4 on ^Z.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR,PFOUT,POSOUT,OPTNUM,OPTCUR,MAXLOC
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBKEY,TCODE
      INTEGER I,LIBREP,LIBRES,LIBSAV
      LOGICAL INTAST,GETSPL
C----------------------------------------------------------------------
      ISTAT=LIBKEY(KEYID,MAINID,TCODE)
      IF(MOD(ISTAT,2).NE.0) THEN
C
C  CHECK string for up and down cursor or PF-keys.
C
        IF((TCODE.EQ.274.OR.TCODE.EQ.2)              ! Sometimes CTRL/B returned for uparrow key
     *    .AND.POSOUT.GT.1) THEN
          POSOUT=POSOUT-1
        ELSEIF(TCODE.EQ.275.AND.POSOUT.LT.NUMPAR) THEN
          POSOUT=POSOUT+1
        ELSEIF(NUMPAR.EQ.1.AND.(TCODE.EQ.274.OR.TCODE.EQ.2)
C                                    ! Sometimes CTRL/B returned for uparrow key
     *    .AND.OPTCUR.GT.MAXLOC) THEN
          OPTCUR=OPTCUR-MAXLOC
        ELSEIF(NUMPAR.EQ.1.AND.TCODE.EQ.275.AND.
     &    OPTCUR+MAXLOC.LE.OPTNUM) THEN
          OPTCUR=OPTCUR+MAXLOC
        ELSEIF(TCODE.EQ.276.AND.OPTCUR.GT.1) THEN
          OPTCUR=OPTCUR-1
        ELSEIF(TCODE.EQ.277.AND.OPTCUR.LT.OPTNUM) THEN
          OPTCUR=OPTCUR+1
        ELSEIF(TCODE.EQ.256.OR.TCODE.EQ.296) THEN
          PFOUT=1
        ELSEIF(TCODE.EQ.257.OR.TCODE.EQ.295) THEN
          PFOUT=2
        ELSEIF(TCODE.EQ.258) THEN
          PFOUT=3
        ELSEIF(TCODE.EQ.259) THEN
          PFOUT=4
        ENDIF
c
c The keystrokes listed in the following cases are the ones for which
c  we should _not_ pop the message window back to the end.  All other
c  keys fall through to the trailing else, where we move the message
c  window to the end.
c
        IF(TCODE.EQ.18.OR.TCODE.EQ.23) THEN      ! Refresh screen on
C                                                ! CTRL/R and CTRL/W
          I=LIBREP()
        ELSEIF(TCODE.EQ.270) THEN                  ! Toggle split screen
C                                                ! on ENTER
          CALL LIBIOF
          IF(SPLFLG) THEN
            CALL ENDSPL
          ELSE
            CALL SPLTIT
          ENDIF
          PFOUT=30       !Special return value for split screen mode
          CALL LIBIND
        ELSEIF(TCODE.EQ.260) THEN                ! Toggle STATUS screen
C                                                ! on Keypad 0
          CALL LIBIOF
          IF(STAFLG) THEN
            CALL ENDSTA
          ELSE
            CALL SPLSTA
          ENDIF
          PFOUT=30       !Special return value for split screen mode
          CALL LIBIND
C$$$        ELSEIF (TCODE .EQ. SMG$K_TRM_NEXT_SCREEN .OR.
C$$$     &          TCODE .EQ. SMG$K_TRM_KP3) THEN
        ELSEIF (TCODE .EQ. 316 .OR.
     &          TCODE .EQ. 263) THEN
          CALL LIBPMV(+1)
C$$$        ELSEIF (TCODE .EQ. SMG$K_TRM_PREV_SCREEN .OR.
C$$$     &          TCODE .EQ. SMG$K_TRM_KP9) THEN
        ELSEIF (TCODE .EQ. 315 .OR.
     &          TCODE .EQ. 269) THEN
          CALL LIBPMV(-1)
C$$$        ELSEIF (TCODE .EQ. SMG$K_TRM_KP7) THEN
        ELSEIF (TCODE .EQ. 267) THEN
          CALL LIBHMV
C$$$        ELSEIF (TCODE .EQ. SMG$K_TRM_KP1) THEN
        ELSEIF (TCODE .EQ. 261) THEN
          CALL LIBEMV
        ELSE
          CALL LIBEMV
        ENDIF
        IF(POSOUT.GT.NUMPAR) POSOUT=NUMPAR
C$$$      ELSE IF (TCODE .EQ. SMG$K_TRM_CTRLZ) THEN
      ELSE IF (TCODE .EQ. 26) THEN
        PFOUT = 4
      ELSE
        CALL MSGSCR(ISTAT,'READ_KEYSTROKE-->')
      ENDIF
      RETURN
      END
