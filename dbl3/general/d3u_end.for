C----------------------------------------------------------------------
      SUBROUTINE D3U_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 - wrap up and go home for current dbl3
C-    database defined by top directory of path D3_PATH
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Stolen   25-NOV-1990   Lor
C-   Modified 10-OCT-1992   Lor, To be consistent with multiple open files
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER D3UNIT,D3UTOP
C
      INTEGER IRET, LC
      CHARACTER *12 TOPD
C----------------------------------------------------------------------
      D3_UNIT = D3UNIT('R',D3_PATH)
      LC = D3UTOP(D3_PATH,TOPD)
C
      IF (D3_UNIT .GT. 0 .AND. LC .GT. 0) THEN
         CALL DBENDF(TOPD)
         IF (IQUEST(1) .NE. 0) 
     &      CALL MSGO('en','D3U_END','DBENDF error, quest=',iquest(1))
         D3_END = .FALSE.
         CLOSE(UNIT=D3_UNIT)
         D3_UNIT = 0
      END IF
C
  999 RETURN
      END
