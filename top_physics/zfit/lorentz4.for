      SUBROUTINE LORENTZ4(DIR,P4IN,P4OUT)

C
C CERN PROGLIB# U101    LORENTZ4          .VERSION KERNFOR  4.07  830624
C ORIG. 20/02/68
C
C-   Updated  22-FEB-1993   Pushpa C. Bhat
C
C      DOUBLE PRECISION PCM2, ONMCM, EPBETA, PROD
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DIR(4),P4IN(4),P4OUT(4)
C
C--                VN(A) MEANS N-VECTOR A
C--                GAMMA=ECM/MCM
C--                EPBETA=ECM*V3(PCM)*V3(BETA)
C--                V3(BETA)=V3(PCM)/ECM
C
      PCM2=DIR(1)*DIR(1)+DIR(2)*DIR(2)+DIR(3)*DIR(3)
      ONMCM=1.D0/DSQRT (DIR(4)*DIR(4)-PCM2)
      EPBETA=P4IN(1)*DIR(1)+P4IN(2)*DIR(2)+P4IN(3)*DIR(3)
      PROD=EPBETA*(DIR(4)*ONMCM-1.D0)/PCM2-P4IN(4)*ONMCM
      P4OUT(4)=ONMCM*(P4IN(4)*DIR(4)-EPBETA)
         DO 50 I=1,3
   50 P4OUT(I)=P4IN(I)+DIR(I)*PROD
      RETURN
      END
