      SUBROUTINE MUCAXYZ(VTX,DIR,XYZ,COVXYZ,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given muon trajectory find the point of
C-   exit from the Calorimeter
C-
C-   Inputs  :   VTX(3) Point on muon trajectory (vertex) 
C-               DIR(3) direction cosines 
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-APR-1993   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      REAL VTX(3),DIR(3),XYZ(3),X,Y,Z,COVXYZ(2,2),ENERGY,EMAX,ET,CX,CY
      INTEGER NCLMAX,IER 
      PARAMETER (NCLMAX=450)
      INTEGER NCELL,IETAC(NCLMAX),IPHIC(NCLMAX),LAYERC(NCLMAX)
      INTEGER ICELL,CELL,IETA,IPHI,IETA0,IPHI0,LAYER,OK
      REAL PATHC(NCLMAX)
      LOGICAL FIRST 
      DATA FIRST /.TRUE./
      DATA CX,CY/400.,400./
      IF(FIRST) THEN
      ENDIF
      CALL CLINPL(VTX,DIR,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,PATHC,IER)
      IF (IER.NE.0) GO TO 999
      LAYER=0
C
C  Find the highest layer number
C
      DO 10 ICELL=1,NCELL
        IF (LAYERC(ICELL).GT.LAYER) THEN
          LAYER=LAYERC(ICELL)
          CELL=ICELL
        END IF
  10  CONTINUE
      EMAX=0.2
      IER=1
      DO 200 IETA=IETAC(CELL)-1,IETAC(CELL)+1
        DO 300 IPHI=IPHIC(CELL)-1,IPHIC(CELL)+1
          CALL CELL_ENERGY(IETA,IPHI,LAYER,ENERGY,ET)
          IF (ENERGY.GT.EMAX) THEN
            IER=0
            EMAX=ENERGY
            IETA0=IETA
            IPHI0=IPHI
            CALL CELXYZ(IETA0,IPHI0,LAYER,X,Y,Z,OK)
            XYZ(1)=X
            XYZ(2)=Y
            XYZ(3)=Z
            COVXYZ(1,1)=CX          
            COVXYZ(2,2)=CY
          END IF
  300   CONTINUE
  200 CONTINUE
  999 RETURN
      END
