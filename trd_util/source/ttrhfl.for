      SUBROUTINE TTRHFL(LTTRH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank TTRH
C-
C-   Inputs  :LTTRH = link of bank to be filled.
C-            LTTRH < 0, routine will get link using GZTTRH
C-            LTTRH = 0, routine will book bank.
C-            
C-   Outputs :
C-   Controls:
C-
C-   Created  27-OCT-1989 18:42:05.09  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LTTRH, GZTTRH
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF

C
      IF(LTTRH.LT.0)LTTRH = GZTTRH()    ! GET LINK.
C
      IF(LTTRH.EQ.0)CALL BKTTRH(LTTRH)        
C Book the bank if argument = 0.
C
      IQ(LTTRH+1) = 1               ! Bank version
C fill in the rest of the bank here.
C----------------------------------------------------------------------
  999 RETURN
      END

