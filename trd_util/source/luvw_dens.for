      INTEGER FUNCTION LUVW_DENS(VRAIS_PI,VRAIS_EL,VRAIS_CO,
     #XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,NU,NV,NW,XU,YUV,ZUVW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Computes the cell number from the likelihood values 
C-
C-   Inputs  :Likelihood values for PION,EECTRON,and Conversion hypothesis:
C-            VRAIS_PI,VRAIS_EL,VRAIS_CO
C-            XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX: Limits of the 3 dimensional
C-            distribution of the 3 likelihoods values for GOOD ELECTRONS
C-            NU,NV,NW: Number of Cells in each of the 3 directions.
C-            XU,YUV,ZUVW: Limits of the cells in each direction.
C-      
C-   Outputs :LUVW_DENS = Event Cell number
C-   Controls:
C-
C-   Created  27-SEP-1995   Y. Ducros
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
      INTEGER NU,NV,NW
      INTEGER DICHO,I
      REAL XU(41),YUV(41,41),ZUVW(41,41,21)
      REAL VRAIS_PI,VRAIS_EL,VRAIS_CO
      REAL EL(3)
      REAL ELX,ELY,ELZ
      REAL ELXM,ELYM,ELZM
      REAL XX(41)
      INTEGER LU,LV,LW
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
        FIRST=.FALSE.
      END IF
C
      ELX=(VRAIS_PI-VRAIS_EL)/SQRT(2.)-XMIN
      ELY=(VRAIS_PI+VRAIS_EL)/SQRT(2.)-YMIN
      ELZ=VRAIS_CO-ZMIN
      ELXM=(VRAIS_PI-VRAIS_EL)/SQRT(2.)-XMAX
      ELYM=(VRAIS_PI+VRAIS_EL)/SQRT(2.)-YMAX
      ELZM=VRAIS_CO-ZMAX
      IF(ELX.LT.0..OR.ELY.LT.0..OR.ELZ.LT.0..OR.ELXM.GT.0..OR.ELYM.GT.
     &  0..OR.ELZM.GT.0.)THEN
        LUVW_DENS=0
        GO TO 998
      END IF
      LU=DICHO(ELX,XU,NU+1)
      DO I=1,NV+1
        XX(I)=YUV(LU,I)
      END DO
      LV=DICHO(ELY,XX,NV+1)
      DO I=1,NW+1
        XX(I)=ZUVW(LU,LV,I)
      END DO
      LW=DICHO(ELZ,XX,NW+1)
c  ***********************************************
      IF(LU.GE.1.AND.LU.LE.NU.AND.LV.GE.1.AND.LV.LE.NV.
     #AND.LW.GE.1.AND.LW.LE.NW) THEN
        LUVW_DENS=(LU-1)*NV*NW+(LV-1)*NW+LW
      ELSE 
        LUVW_DENS=0
        GO TO 998
      END IF
C  ***********************************************
  998 CONTINUE
  999 continue
      RETURN
      END
