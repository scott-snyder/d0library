C DEC/CMS REPLACEMENT HISTORY, Element CPREDICT_LONG.FOR
C *1    25-OCT-1989 11:39:51 RAJA "prediction routines split"
C DEC/CMS REPLACEMENT HISTORY, Element CPREDICT_LONG.FOR
      SUBROUTINE CPREDICT_LONG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Longitudinal H matrix predicition
C-
C-   Inputs  : 
C-   Outputs : Predicted Crack energy
C-             Predicted Cryostat energy
C-   Controls: 
C-
C-   Created  25-OCT-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER ILO,IHI,JLO,JHI,KLO,KHI
      INTEGER I,J,K,II,JJ,KK
      INTEGER IET,IPH,ILYR,JET,JPH,KET,KPH
      DOUBLE PRECISION DELJ
      REAL    YDUM
C----------------------------------------------------------------------
C
C ****  now to do the purely longitudinal prediction.
C
      JLO = 1
      JHI = NDIMVL
      ILO = JHI+1
      IHI = NDIML
      KLO = ILO
      KHI = IHI                         ! LIMITS OF SUMMING.
C
      DO K = KLO,KHI
        KK = K-KLO+1
        DELPREDL(KK) = 0.
        DO I = ILO,IHI
          II = I - ILO + 1
          DO J = JLO,JHI
            DELJ = QUANTL(J)-AVERL(J)
            DELPREDL(KK) = DELPREDL(KK)
     &        - HMATRL_INV(II,KK)*HMATRL(I,J)*DELJ
          ENDDO
        ENDDO
      ENDDO
      DO K = KLO ,KHI
        KK = K-KLO+1
        PREDL(KK) = DELPREDL(KK) + AVERL(K)
      ENDDO
C
  999 RETURN
      END
