      SUBROUTINE ERRGET(KEY, NTIMES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the number of calls of the message KEY
C-
C-   Inputs  : KEY         Identifies the message
C-
C-   Outputs : NTIMES      The current count of calls with KEY
C-
C-   Controls: None
C-   Internal Logicals:
C-      FOUND       .TRUE. ==> if KEY is available.
C-                         .FALSE.==> otherwise.
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)KEY
      CHARACTER*32 KEYIN,KEYUPC
      INTEGER NTIMES ,POS
      LOGICAL FOUND
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCNT.INC'
C
      CALL ERRINT
      KEYIN = KEY                       ! Chop down to standard length
      CALL UPCASE(KEYIN, KEYUPC)
      CALL ERRFND( KEYUPC, POS, FOUND )
      IF ( FOUND ) THEN
         NTIMES = COUNT(POS)
      ELSEIF ( .NOT. FOUND ) THEN
         NTIMES = 0
      ENDIF
C
  999 RETURN
      END
