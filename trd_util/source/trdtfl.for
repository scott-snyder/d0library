      SUBROUTINE TRDTFL(LTRDT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank TRDT
C-
C-   Inputs  :LTRDT = link of bank to be filled.
C-            LTRDT < 0, routine will get link using GZTRDT
C-            LTRDT = 0, routine will book bank.
C-            
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstej
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LTRDT, GZTRDT
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF

C
      IF(LTRDT.LT.0)LTRDT = GZTRDT()    ! GET LINK.
C
      IF(LTRDT.EQ.0)CALL BKTRDT(LTRDT)        
C Book the bank if argument = 0.
C
      IQ(LTRDT+1) = 1               ! Bank version
C fill in the rest of the bank here.
C----------------------------------------------------------------------
  999 RETURN
      END

