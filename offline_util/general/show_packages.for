      SUBROUTINE SHOW_PACKAGES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to Display via INTMSG which packages are 
C-                         available in the current EXAMINE EXE.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JUL-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N
      LOGICAL ACTIVE,PBD_GET_NEXT_FLAG,PBD_VALUE,OK
      CHARACTER PBD_FLAG*32,MSG*40,STATE*3
C----------------------------------------------------------------------
      N = 1
      ACTIVE = .TRUE.
      DO WHILE (ACTIVE)
        ACTIVE = PBD_GET_NEXT_FLAG(PBD_FLAG,PBD_VALUE,N)
        IF(PBD_VALUE) THEN
          STATE = ' ON'
        ELSE
          STATE = 'OFF'
        END IF
        IF (ACTIVE) THEN
          WRITE(MSG,10)N-1,PBD_FLAG,STATE
          CALL INTMSG(MSG)
        END IF
      END DO
   10 FORMAT(1X,I3,1X,A32,A3)
  999 RETURN
      END
