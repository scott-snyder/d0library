      SUBROUTINE PUBUTTON(ID,LABEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a labeled button in the current
C-   viewport. Create the button with PUBUTTON_CREATE(ID). Set the
C-   button attributes with PUBUTTON_SET. Use PUBUTTON_FIND to determine
C-   which button was pressed.
C-
C-   Inputs  : ID       [I]     Button ID
C-             LABEL    [C*]    Name of button
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-MAY-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID
      CHARACTER*(*) LABEL
      CHARACTER*(*) OPTION
      REAL    X(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      LOGICAL A_BUTTON(MAXOBJ)
      REAL    XX(3)
      DATA A_BUTTON/MAXOBJ*.FALSE./
C----------------------------------------------------------------------
      IF ( A_BUTTON(ID) ) THEN
        CALL PUTEXT_SET(ID,'AUTO',0)      ! Auto-position button
        CALL PUTEXT(ID,LABEL(1:LEN(LABEL)),1)
      ENDIF
      RETURN
C
      ENTRY PUBUTTON_CREATE(ID)
      CALL PUTEXT_CREATE(ID)
      IF ( ID .GT. 0 ) THEN
        A_BUTTON(ID) = .TRUE.
      ENDIF
      RETURN
C
      ENTRY PUBUTTON_SET(ID,OPTION,X)
      CALL PUTEXT_SET(ID,OPTION(1:LEN(OPTION)),X)
      RETURN
C
      ENTRY PUBUTTON_FIND(ID)
      CALL PU_GET_PICKW(XX)
      CALL PUTEXT_FIND(XX,ID)
      IF ( ID .GT. 0 ) THEN
        IF ( .NOT. A_BUTTON(ID) ) THEN
          ID = 0
        ENDIF
      ENDIF
      RETURN
C
C      ENTRY PUBUTTON_EXIT(ID)
C      IF ( .NOT. A_BUTTON(ID) ) THEN
C        CALL PUTEXT_CREATE(ID)
C      ENDIF
C      CALL PUTEXT(ID,' EXIT ',1)
C
C ****  
C
  999 RETURN
      END
