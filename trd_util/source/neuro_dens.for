      SUBROUTINE NEURO_DENS(INDEX,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,
     #    NUVW,NU,NV,NW,XU,YUV,ZUVW,
     #    INDEXP,XPMIN,XPMAX,YPMIN,YPMAX,ZPMIN,ZPMAX,
     #    NPUVW,NPU,NPV,NPW,XPU,YPUV,ZPUVW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads TDOR bank 
C-
C-   Inputs  :
C-               TDOR bank which gives the Cell ordering of the 3 dimensional
C-               likelihood distribution in order to get the maximum background
C                rejection.
C-               The background is selected using fake electrons (Anti-cuts on
C                the isolation and chi2)
C-               The electron sample corresponds to good electrons from the W 
C-               and Z sample
C-         
C-   Outputs:   
C-              WITHOUT dE/dX cuts in the CDC:
C-             XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX: Limits of the 3 dimensional
C-            distribution of the 3 likelihoods values for GOOD ELECTRONS
C-            NU,NV,NW: Number of Cells in each of the 3 directions.
C-            XU,YUV,ZUVW: Limits of the cells in each direction.
C-            
C-             WITH      dE/dX cuts in the CDC:
C-             XPMIN,XPMAX,YPMIN,YPMAX,ZPMIN,ZPMAX: Limits of the 3 dimensional
C-            distribution of the 3 likelihoods values for GOOD ELECTRONS
C-            NPU,NPV,NPW: Number of Cells in each of the 3 directions.
C-            XPU,YPUV,ZPUVW: Limits of the cells in each direction. 
C-
C-             INDEX Cell ordering ( WITHOUT dE/dX cuts)
C-             INDEXP Cell ordering ( WITH dE/dX cuts)
C-   Controls:
C-
C-   Created  22_FEB-1996   Y. Ducros
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NUVW,NU,NV,NW,LTDOR,GZTDOR
      INTEGER INDEX(32000)
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
      REAL XU(41),YUV(41,41),ZUVW(41,41,21)
      INTEGER NPUVW,NPU,NPV,NPW
      INTEGER INDEXP(32000)
      REAL XPMIN,XPMAX,YPMIN,YPMAX,ZPMIN,ZPMAX
      REAL XPU(41),YPUV(41,41),ZPUVW(41,41,21)
      INTEGER KKK,I,J,K
C
C   READ TDOR BANK
      LTDOR=GZTDOR()
      IF(LTDOR.LE.0) THEN
        GO TO 999
      END IF
C
C ***********  CELL INDEX AND CELL BOUNDARIES ( 3 TRD LAYERS + CDC)
C
      NUVW=INT(C(LTDOR+2))
      NU=INT(C(LTDOR+3))
      NV=INT(C(LTDOR+4))
      NW=INT(C(LTDOR+5))
C   
      XMIN= C(LTDOR+6)
      XMAX= C(LTDOR+7)
      YMIN= C(LTDOR+8)
      YMAX= C(LTDOR+9)
      ZMIN= C(LTDOR+10)
      ZMAX= C(LTDOR+11)
C
C **************    CELL INDEX ******************
C
      DO J=1,NUVW
        INDEX(J)=INT(C(LTDOR+12+J))
      END DO
C **************   CELL BOUNDARIES **********************
      KKK=LTDOR+12+NUVW
      DO I=1,NU+1
        KKK=KKK+1
        XU(I)=C(KKK)
      END DO
      DO I=1,NU
        DO J=1,NV+1
          KKK=KKK+1
          YUV(I,J)=C(KKK)
        END DO
      END DO
      DO I=1,NU
        DO J=1,NV
          DO K=1,NW+1
            KKK=KKK+1
            ZUVW(I,J,K)=C(KKK)
          END DO
        END DO
      END DO
C
C *******  CELL INDEX AND CELL BOUNDARIES (3 TRD Layers+CDC  With DE/DX cut)
C
      NPUVW=INT(C(LTDOR+4000+2))
      NPU=INT(C(LTDOR+4000+3))
      NPV=INT(C(LTDOR+4000+4))
      NPW=INT(C(LTDOR+4000+5))
C   
      XPMIN= C(LTDOR+4000+6)
      XPMAX= C(LTDOR+4000+7)
      YPMIN= C(LTDOR+4000+8)
      YPMAX= C(LTDOR+4000+9)
      ZPMIN= C(LTDOR+4000+10)
      ZPMAX= C(LTDOR+4000+11)
C
C **************    CELL INDEX ******************
C
      DO J=1,NPUVW
        INDEXP(J)=INT(C(LTDOR+4000+12+J))
      END DO
C **************   CELL BOUNDARIES **********************
      KKK=LTDOR+4000+12+NPUVW
      DO I=1,NPU+1
        KKK=KKK+1
        XPU(I)=C(KKK)
      END DO
      DO I=1,NPU
        DO J=1,NPV+1
          KKK=KKK+1
          YPUV(I,J)=C(KKK)
        END DO
      END DO
      DO I=1,NPU
        DO J=1,NPV
          DO K=1,NPW+1
            KKK=KKK+1
            ZPUVW(I,J,K)=C(KKK)
          END DO
        END DO
      END DO
  999 CONTINUE
      END
