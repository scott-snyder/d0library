      SUBROUTINE DLOREN4  (DIR,P4IN,P4OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Transforms a lab 4 vector to rest frame
C-                        of particle with 4 vector DIR
C-
C-   Inputs  : DIR = 4 vector of restframe
C-             P4in = 4 vector of particle in lab
C-              
C-   Outputs : p4out = 4 vector of particle in restframe
C-   Controls:
C-
C-   Created  26-FEB-1994   Rajendran Raja
C-                          double precision version of routine in Cernlib
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

C
C CERN PROGLIB# U101    LOREN4          .VERSION KERNFOR  4.07  830624
C ORIG. 20/02/68
C
      DOUBLE PRECISION PCM2, ONMCM, EPBETA, PROD
      DOUBLE PRECISION DIR(4),P4IN(4),P4OUT(4)
      INTEGER I
C
C--                VN(A) MEANS N-VECTOR A
C--                GAMMA=ECM/MCM
C--                EPBETA=ECM*V3(PCM)*V3(BETA)
C--                V3(BETA)=V3(PCM)/ECM
C
C----------------------------------------------------------------------
      PCM2=DIR(1)*DIR(1)+DIR(2)*DIR(2)+DIR(3)*DIR(3)
      ONMCM=1.D0/DSQRT (DIR(4)*DIR(4)-PCM2)
      EPBETA=P4IN(1)*DIR(1)+P4IN(2)*DIR(2)+P4IN(3)*DIR(3)
      PROD=EPBETA*(DIR(4)*ONMCM-1.D0)/PCM2-P4IN(4)*ONMCM
      P4OUT(4)=ONMCM*(P4IN(4)*DIR(4)-EPBETA)
      DO 50 I=1,3
   50 P4OUT(I)=P4IN(I)+DIR(I)*PROD
C
  999 RETURN
      END
