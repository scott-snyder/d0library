      SUBROUTINE XXXXFL(LXXXX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank XXXX.
C-
C-   Inputs  :  LXXXX = link of bank to be filled.
C-              LXXXX < 0, routine will get link using GZXXXX
C-              LXXXX = 0, routine will book bank.
C-            
C-   Outputs :
C-   Controls:
C-
C-   Created  XDATE  XAUTHOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LXXXX
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZXXXX
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
      IF ( LXXXX .LT. 0 ) THEN
        LXXXX = GZXXXX()    ! GET LINK.
      ENDIF
C
      IF ( LXXXX .EQ. 0 ) THEN
        CALL BKXXXX(LXXXX)        
      ENDIF
C
C Book the bank if argument = 0.
C
      IQ(LXXXX+1) = 1               ! Bank version
C
C fill in the rest of the bank here.
  999 RETURN
      END
