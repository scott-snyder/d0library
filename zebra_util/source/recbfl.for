      SUBROUTINE RECBFL(LRECB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank RECB
C-
C-   Inputs  :LRECB = link of bank to be filled.
C-            LRECB < 0, routine will get link using GZRECB
C-            LRECB = 0, routine will book bank.
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JUL-1990 10:27:01.49  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LRECB, GZRECB
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
      ENDIF

C
      IF(LRECB.LT.0)LRECB = GZRECB()    ! GET LINK.
C
      IF(LRECB.EQ.0)CALL BKRECB(LRECB)
C Book the bank if argument = 0.
C
      IQ(LRECB+1) = 1               ! Bank version
C fill in the rest of the bank here.
C----------------------------------------------------------------------
  999 RETURN
      END
