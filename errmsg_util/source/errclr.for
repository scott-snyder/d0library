      SUBROUTINE ERRCLR(KEY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remove the message KEY from storage, if
C-                         KEY = " ", then remove all current message
C-                         in storage.
C-
C-   Inputs  : KEY         message to be removed
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)KEY
      CHARACTER*32 KEYIN,KEYUPC
      INTEGER POS, N, I
      LOGICAL FOUND
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C
C **** If KEY = " " , remove all messages
C
      CALL ERRINT
      KEYIN = KEY                       ! Chop down to standard length
      IF (KEYIN .EQ. ' ') THEN
        NENTRY = 0
      ELSE
C
C **** Search for the position of the message KEY
C
        CALL UPCASE( KEYIN, KEYUPC )
        CALL ERRFND( KEYUPC, POS, FOUND )
C
C **** delete the message, if available
C
        N = NENTRY - 1
        IF (FOUND) THEN
          DO 10 I = POS, N
            ID(I) = ID(I+1)
            COUNT(I) = COUNT(I+1)
            MAXWRN(I) = MAXWRN(I+1)
            MAXLOG(I) = MAXLOG(I+1)
   10     CONTINUE
          NENTRY = NENTRY - 1
        ENDIF
      ENDIF

C
  999 RETURN
      END
