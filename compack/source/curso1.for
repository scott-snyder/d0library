      SUBROUTINE CURSO1(PFOUT, POS, MAXPOS, NUMCOL, REGEN_NEEDED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read up and down cursor or PF-keys.
C-
C-           It first read 3 characters from input, determines whether
C-           it is a cursor key or a PF-key or not. Only called in full
C-           screen mode. The parameter REGEN_NEEDED is set to true
C-           if the menu display needs to be regenerated (i.e., the
C-           user selected or deselected one of the small displays)
C-           and false otherwise.
C-
C-           This routine should be used instead of CURSOR in routines
C-           for which MENDIS will not properly regenerate the display.
C-
C-   Inputs  : POS:    Current position in display
C-             MAXPOS: Maximum number of items at this level
C-             NUMCOL: Number of columns in display
C-   Outputs : PFOUT:  Number of PF-key struck (if any)
C-             POS:    New position in menu display
C-             REGEN_NEEDED: true if the menu display must be regenerated
C-   Controls: None
C-
C-   Created  27-NOV-1990   Scott Snyder
C-   Updated  23-FEB-1991   Jan S. Hoftun  (renamed CURSO1)
C-   Updated   9-MAY-1991   Scott Snyder
C-    Return PFOUT=4 on ^Z.
C-   Updated   1-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PFOUT, POS, MAXPOS, NUMCOL
      LOGICAL REGEN_NEEDED
      INCLUDE 'd0$inc:smgcom.inc'
C&IF VAXVMS
      INCLUDE '($smgdef)'
      INCLUDE '($ssdef)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
C
      INTEGER ISTAT, TCODE
C
      INTEGER LIBKEY
C----------------------------------------------------------------------
      REGEN_NEEDED = .FALSE.
      ISTAT=LIBKEY(KEYID,MAINID,TCODE)
      IF(MOD(ISTAT,2).NE.0) THEN
        CALL CHKCOM(TCODE, PFOUT, POS, MAXPOS, NUMCOL)
        IF(TCODE .EQ. SMG$K_TRM_ENTER .OR.
     &     TCODE .EQ. SMG$K_TRM_KP0) THEN            ! Redisplay menu after
C                                                    ! toggling split screen
          REGEN_NEEDED = .TRUE.
        ENDIF
C&IF VAXVMS
      ELSEIF(ISTAT .EQ. SS$_ABORT) THEN     !ABORT signalled
C&ENDIF
        REGEN_NEEDED = .TRUE.
      ELSEIF(TCODE .EQ. SMG$K_TRM_CTRLZ) THEN
        PFOUT = 4
      ELSE
        CALL MSGSCR(ISTAT,'READ_KEYSTROKE-->')
      ENDIF
  999 RETURN
      END
