      SUBROUTINE PX_ROTATE(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize screen parameters to rotate
C-   the current view(s).
C-
C-   Inputs  : COMMAND  [C*]    Screen Command
C-   Outputs : COMMAND  [C*]    Screen Command
C-   Controls:
C-
C-   Created  28-MAY-1991   Lupe Howell, Harrison B. Prosper
C-   Updated  18-JUN-1991   Nobuaki Oshima, Harrison B. Prosper
C-      Modify calling sequence in px_rotate
C-   Updated  10-JAN-1992   Nobuaki Oshima - To use 'QUIT' button
C-   Updated  13-MAY-1992   Lupe Howell, Harrison B. Prosper
C-      Simplify the rotation execution
C-   Updated  16-MAY-1992   Nobuaki Oshima
C-      Add resetting 'ROTATING' and COMMAND for E&S.
C-   Updated   1-JUL-1992   Lupe Howell
C-      Update screens and push commands to stack
C-   Updated  22-JUL-1992   Nobuaki Oshima
C-      Fix the push commands problem
C-   Updated   2-SEP-1992   Nobuaki Oshima
C-      Store coordinate information and set 'HARDWARE_ROTATE' flag.
C-   Updated   9-OCT-1992   Nobuaki Oshima
C-      Call PU_RESET_QUEUE when HARDWARE_ROTATE mode was ended.
C-   Updated  17-NOV-1992   Lupe Howell New commands added to the options
C-      that can be push into the queue
C-   Updated   8-DEC-1992   Nobuaki Oshima
C-      Remove PU_SET_3D_POINT and add Hardware control param 
C-      'HROTATE CONTRL'.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) COMMAND
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
C----------------------------------------------------------------------
      INTEGER RSEGTYP(512)
      INTEGER NREAL,NSEG,I
      PARAMETER( NREAL = 21 )
      INTEGER ICHAR,IER
C
      REAL    RLEVEL,HSCAL
      REAL    RLIST(NREAL)
C
      CHARACTER*80 COMD,STRING(10)
      LOGICAL EZERROR
C----------------------------------------------------------------------

      CALL JIQDIL(RLEVEL)
C
C ****  Rotation for standard DI3000
C
      IF ( RLEVEL .GE. 6 ) THEN
C
C ****  Set the hardware rotate flag to FALSE
C
        CALL FLGSET('HARDWARE_ROTATE',.FALSE.)
C
        CALL DI3_ROTATE_3D(COMMAND)
C
C **** Other devices (SGI, E&S, etc)
C
      ELSE
C
C ****  Set the hardware rotate flag TRUE
C
        CALL FLGSET('HARDWARE_ROTATE',.TRUE.)
C-
C--- Get Hardware rotation control params, if it's available.
C-
        CALL EZPICK('PX_SYSTEM_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PX_ROTATE',
     &      'Bank PX_SYSTEM_RCP NOT FOUND','W')
          GOTO 999
        ENDIF
        CALL PUGETV('HROTATE CONTRL',HSCAL)
        CALL EZRSET
C
C ****  Get segment type list
C ****  pack command into RLIST
C
        CALL PU_GET_SEGMENT_TYPE(NSEG,RSEGTYP)
        COMD = COMMAND
        CALL DCTOH(80,COMD,RLIST)
        RLIST(21) = HSCAL
        CALL JESCAP(ROTATE_3D,NSEG,NREAL,RSEGTYP,RLIST)
C
C ****  Pushing commands, if any, to the general pixie queue
C
        IF ( NSEG.GT.0 .AND. NSEG.LE.10) THEN
C
C ****  Update screen parameters in RCP
C ****  all the currently active screens
C
          CALL PU_UPDATE_SCREEN_PARAMS(COMMAND)
C
C ****  RSEGTYP will return integer values that tell us what is
C ****  what to push in queue
C ****  1 ROTATE
C ****  2 HARDCOPY
C ****  3 REDRAW
C ****  4 PICK
C ****  5 Push the COMMAND view
C
          DO I=1,10
            STRING(I) = ' '
          ENDDO
          DO I=1,NSEG
            IF     ( RSEGTYP(I) .EQ. 1 ) THEN
              STRING(I) = 'ROTATE'
            ELSEIF ( RSEGTYP(I) .EQ. 2 ) THEN
              STRING(I) = 'HARDCOPY'
            ELSEIF ( RSEGTYP(I) .EQ. 3 ) THEN
              STRING(I) = 'REDRAW'
            ELSEIF ( RSEGTYP(I) .EQ. 4 ) THEN
              STRING(I) = 'PICK'
            ELSEIF ( RSEGTYP(I) .EQ. 5 ) THEN
              STRING(I) = COMMAND
            ELSEIF ( RSEGTYP(I) .EQ. 6 ) THEN
              STRING(I) = 'REMOVE'
            ENDIF
          ENDDO
          CALL PU_PUSH_QUEUE(NSEG,STRING)
        ELSE
C
C ****  EXIT: Set the hardware rotate flag to FALSE
C
          CALL FLGSET('HARDWARE_ROTATE',.FALSE.)
          CALL PU_RESET_QUEUE
C
        ENDIF
        COMMAND = ' '
      ENDIF
  999 RETURN
      END
