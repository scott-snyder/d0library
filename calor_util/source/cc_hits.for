      SUBROUTINE CC_HITS(VTX,DIR,NCLMAX,NCELL,TETA,TPHI,TLYR,ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds all calor cells hit by a line
C-
C-   Inputs  : VTX(3)         Point origin of the line
C-             DIR(3)         Direction cosines of the line
C-             NCLMAX         Max number of cells to be returned
C-   Outputs : NCELL          Actual number of hit cells returned
C-             TETA(NCLMAX)  List of etas of hit cells
C-             TPHI(NCLMAX)  List of phis of hit cells
C-             TLYR(NCLMAX) List of layerc's of hit cells
C-             ARGSOK         Flag for arg errors
C-   Controls:
C-
C-   Created  25-OCT-1991  W. Dharmaratna 
C-   Updated  15-JAN-1992   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      REAL VTX(3),DIR(3)
      INTEGER NCLMAX,NCELL
      REAL  TETA(NCLMAX),TPHI(NCLMAX),TLYR(NCLMAX)
      INTEGER ARGSOK
      INTEGER IETA,IPHI,LAYER,AROKIN
      INTEGER I,NSL,IS,NS,NRD
      REAL CENRAD,DRAD,CENZED,DZED,ZED,S(2),X,Y,Z
      REAL PHI,THETA,ETA,RADIUS,RDLIST(45),LYRLIST(MXLYCH)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        NRD=0
        IETA=1
C
C ****  List CC cylinders
C
          DO 300 LAYER=1,MNLYCH          !LOOP ON CC LAYERS
            IF(LAYER.GT.3.AND.LAYER.LT.7) GO TO 300  !SKIP FLOOR 3 REDUNDANCIES
            IF(LAYER.GT.7.AND.LAYER.LT.11) GO TO 300 !SKIP ICD & MG
            IF(LAYER.EQ.14) GO TO 300                !NO LYR=14 IN CC
            IETA=1
            CALL CALRAD(IETA,LAYER,CENRAD,DRAD,CENZED,DZED,AROKIN)
            IF(AROKIN.NE.0) GO TO 300
            NRD=NRD+1
            RDLIST(NRD)=CENRAD
            LYRLIST(NRD)=LAYER
  300     CONTINUE
        FIRST=.FALSE.
      ENDIF
C
C ****  End of initialization
C
      ARGSOK=1
      NSL=0
      IETA=1
C
C ****  Find intersections of line with all radial cylinders
C
      NCELL=0
      CALL VZERO(TETA,NCLMAX)
      CALL VZERO(TPHI,NCLMAX)
      CALL VZERO(TLYR,NCLMAX)
      DO 950 I=1,NRD
        CALL CLNRD(VTX,DIR,RDLIST(I),NS,S)
        IF(NS.GT.0) THEN
          X=VTX(1)+DIR(1)*S(1)
          Y=VTX(2)+DIR(2)*S(1)
          Z=VTX(3)+DIR(3)*S(1)
C
C ****  Convert to Eta and Phi
C
          PHI=ATAN2(Y,X)               
          IF(PHI.LT.0.) PHI=PHI+TWOPI  
          PHI= PHI*64.0/TWOPI 
          RADIUS = SQRT(X**2+Y**2)          
          THETA=ATAN2(RADIUS,Z)        
          ETA=-ALOG(TAN(THETA/2.))     
          ETA=10.0*ETA    
          IF(NCELL.GE.NCLMAX) GO TO 999
          NCELL=NCELL+1
          TETA(NCELL)=ETA
          TPHI(NCELL)=PHI
          TLYR(NCELL)=LYRLIST(I)
        ENDIF
  950 CONTINUE
      ARGSOK=0
  999 RETURN
      END
