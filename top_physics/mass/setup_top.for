      SUBROUTINE SETUP_TOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETS UP TOP 4 VECTORS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INTEGER IW
C----------------------------------------------------------------------
      IF ( SOL(1) ) THEN
        DO IW =  1 , 2
          CALL PAIR_MASSD(W1(1,IW),JET1,TOP1(1,IW))
        ENDDO
      ENDIF
C
      IF ( SOL(2) ) THEN
        DO IW =  1 , 2
          CALL PAIR_MASSD(W2(1,IW),JET2,TOP2(1,IW))
        ENDDO
      ENDIF
C
  999 RETURN
      END
