      SUBROUTINE ABC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test of EDEBUG 3 dim array handling
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-APR-1992   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER A(1000),I1,I2,J1,J2,K1,K2
      INTEGER II
C----------------------------------------------------------------------
   10 CONTINUE
      I1 = 1
      I2 = 2
      J1 = 1
      J2 = 4
      K1 = -2
      K2 = 2
      CALL ABC2(A,I1,I2,J1,J2,K1,K2)
      II = 1
      IF ( II.GT.0 ) GOTO 10
  999 RETURN
      END
      SUBROUTINE ABC2(A,I1,I2,J1,J2,K1,K2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-APR-1992   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I1,I2,J1,J2,K1,K2
      INTEGER A(I1:I2,J1:J2,K1:K2)
      INTEGER IND,I,J,K
C----------------------------------------------------------------------
      IND = 0
      DO K = K1, K2
        DO J = J1, J2
          DO I = I1, I2
            IND = IND + 1
            A(I,J,K) = IND
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
