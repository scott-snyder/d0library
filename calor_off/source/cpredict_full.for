      SUBROUTINE CPREDICT_FULL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Full H matrix predicition
C-
C-   Inputs  : 
C-   Outputs : Predicted Crack energy
C-             Predicted Cryostat energy
C-             Predicted X,Y,Z positions of shower center
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
      INTEGER IX
C----------------------------------------------------------------------
      IX(IET,IPH,ILYR) = 1 + (IET-NETLO) + (IPH-NPHLO)*NETTOT +
     &  (ILYR-1)*NETTOT*NPHTOT                ! INDEX STATEMENT FUNCTION.
C
C ****  now to do the full predicition.
C
      JLO = 1
      JHI = IPREDF -1
      ILO = IPREDF
      IHI = NDIMH
      KLO = ILO
      KHI = IHI                         ! LIMITS OF SUMMING.
C
      DO K = KLO,KHI
        KK = K-KLO+1
        DELPRED(KK) = 0
        DO I = ILO,IHI
          II = I - ILO + 1
          DO J = JLO,JHI
            DELJ = QUAN(J)-AVR(J)
            DELPRED(KK) = DELPRED(KK) - HMAT_INV(II,KK)*HMAT(J,I)*DELJ
          ENDDO
        ENDDO
      ENDDO
C
      DO K = KLO ,KHI
        KK = K-KLO+1
        PRED(KK)  = DELPRED(KK) + AVR(K)
      ENDDO
C
  999 RETURN
      END
