      SUBROUTINE CURSOR(PFOUT,POSIT,MAXIN,NUMIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read up and down cursor or PF-keys.
C-
C-           It first read 3 characters from input, determines whether
C-           it is a cursor key or a PF-key or not. Only called in full
C-           screen mode.
C-
C-   Inputs  : MAXIN: Maximum number of items in current menu.
C-             NUMIN: Number of columns in current display.
C-             POSIT: Current position in menu display.
C-   Outputs : PFOUT: Number of PF-key struck (if any)
C-             POSIT: New position in menu display
C-   Controls: None
C-
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-   Revised      9-MAY-1991   Scott Snyder
C-    Return PFOUT=4 on ^Z.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PFOUT,POSIT,MAXIN,NUMIN
C
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBKEY,TCODE
C----------------------------------------------------------------------
      ISTAT=LIBKEY(KEYID,MAINID,TCODE)
      IF(MOD(ISTAT,2).NE.0) THEN
        CALL CHKCOM(TCODE,PFOUT,POSIT,MAXIN,NUMIN)
        IF(TCODE.EQ.270.OR.TCODE.EQ.260) THEN        ! Redisplay menu after
C                                                    ! toggling split screen
          CALL MENDIS(.TRUE.)
        ENDIF
      ELSEIF(ISTAT.EQ.44) THEN           !ABORT signalled
        CALL MENDIS(.TRUE.)
      ELSEIF(TCODE.EQ.26) THEN
        PFOUT = 4
      ELSE
        CALL MSGSCR(ISTAT,'READ_KEYSTROKE-->')
      ENDIF
      RETURN
      END
