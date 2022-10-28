      SUBROUTINE FIT2FL(LFIT2,FIT2PAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank FIT2.
C-
C-   Inputs  :  LFIT2 = link of bank to be filled.
C-              LFIT2 < 0, routine will get link using GZFIT2
C-              LFIT2 = 0, routine will book bank.
C-              FIT2PAR (F) Fitted info array to go into FIT2 bank      
C-
C-   Outputs :
C-   Controls:
C-
C-   Created   3-SEP-1993 23:29:14.64  Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LFIT2,I
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZFIT2
      REAL    FIT2PAR(50)
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
      IF ( LFIT2 .LT. 0 ) THEN
        LFIT2 = GZFIT2()    ! GET LINK.
      ENDIF
C
      IF ( LFIT2 .EQ. 0 ) THEN
        CALL BKFIT2(LFIT2)
      ENDIF
C
C Book the bank if argument = 0.
C
      IQ(LFIT2+1) = 1               ! Bank version
C
C fill in the rest of the bank here.
      IQ(LFIT2+2) = FIT2PAR(2)
      IQ(LFIT2+3)=FIT2PAR(3)
      DO I=4,50
        Q(LFIT2+I) = FIT2PAR(I)
      ENDDO
  999 RETURN
      END
