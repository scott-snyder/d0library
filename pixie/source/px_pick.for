      SUBROUTINE PX_PICK(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pick a point and store coordinates of
C-   selected point in virtual, world and PIXIE coordinates.
C-
C-   Inputs  : None
C-
C-   Outputs : COMMAND  [C*]    Screen Command or BLANK
C-   Controls:
C-
C-   Created  13-MAY-1991   Harrison B. Prosper
C-   Updated  03-JUL-1991   Nobuaki Oshima
C-       Add a new entry PX_PICK_QUIT for PHI picking in CALDIS
C-   Updated  10-JAN-1992   Nobuaki Oshima - To use 'QUIT' button
C-   Modified  2-SEP-1992   Nobuaki Oshima - 'QUIT' button is only for DI3000
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
C----------------------------------------------------------------------
      INTEGER IER,IDDX,BUTID,ID
      REAL    WXCOOR,WYCOOR,WZCOOR,X(3)
      REAL    RLEVEL
      CHARACTER*32 PACK
      CHARACTER*40 COMM
      LOGICAL LQUIT,FIRST
      DATA LQUIT,FIRST /.FALSE.,.TRUE./
C----------------------------------------------------------------------
      CALL JIQDIL(RLEVEL)
      IF ( LQUIT ) THEN
        LQUIT = .FALSE.
        CALL FLGSET('PICKING',.FALSE.)
        COMMAND = ' '
        GO TO 999
      ENDIF
C-
C--- Create QUIT button
C-
      IF ( FIRST .AND. RLEVEL.GE.6. ) THEN
        CALL PUTEXT_CREATE(BUTID)
        CALL PUTEXT_SET(BUTID,'AUTO',0)      ! Auto-position button
        CALL PUTEXT_SET(BUTID,'RED/E',0)
        CALL PUTEXT(BUTID,'QUIT',1)
        FIRST = .FALSE.
      ENDIF
C-
C--- do picking here...
C-
      CALL PU_SELECT_VIEWPORT(WXCOOR,WYCOOR,WZCOOR,PACK,COMM,IDDX,IER)
C-
C--- Find a QUIT button for exit
C-
      IF ( RLEVEL .GE. 6. ) THEN
        X(1) = WXCOOR
        X(2) = WYCOOR
        X(3) = WZCOOR
        CALL PUTEXT_FIND(X,ID)
        IF (ID .EQ. BUTID) THEN
          CALL FLGSET('PICKING',.FALSE.)
          CALL PUTEXT_DELETE(ID)
          FIRST = .TRUE.
          COMMAND = ' '
          GO TO 999
        ENDIF
      ENDIF
C
C ****  Check command type
C
      IF ( IER .EQ. 0 ) THEN
        IF ( INDEX(COMMAND,'%') .GT. 0 ) THEN
          CALL FLGSET('COMBINED_MODE',.TRUE.)
        ENDIF
        CALL FLGSET('PICKING',.TRUE.)
      ELSE
        CALL FLGSET('PICKING',.FALSE.)
        COMMAND = ' '
        GO TO 999
      ENDIF
C
      RETURN
C-
C---------- PX_PICK_QUIT
C-
      ENTRY PX_PICK_QUIT
C-
      LQUIT = .TRUE.
C-
  999 RETURN
      END
