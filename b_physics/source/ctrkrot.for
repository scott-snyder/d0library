      SUBROUTINE CTRKROT(LVTXT,PHI0,SCERR,COOR0,XVEC,ERVEC) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert a central track from cylindrical to
C-   carthesian coordinates
C-   
C-   The parameters are 
C    XVEC(1-4)  a,b,c,d 
C-   ERVEX(4,4) error matrix
C-
C-
C-   Inputs  :  LVTXT: VTX or CDC bank address 
C-              PHI0 - rotation angle in the XY  plane 
C-              SCERR: scaling factor for the error
C-   Outputs :  CORR0: central coordinate 
C-              XVEC,ERVEC
C-
C-   Controls: 
C-
C-   Created  31-May-1993   Daria Zieminska
C-                          Andrzej Zieminski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LVTXT
      REAL XVEC(*),ERVEC(4,4) 
      REAL X0,Y0,Z0,R0,PHI,THE,SP2,CP2,EPHI,EXY,ETHE,EZ 
      REAL COOR0,SCERR,PHI0
C
      PHI=Q(LVTXT+6)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
      PHI=PHI-PHI0
      IF(PHI.LT.0.) PHI=PHI+TWOPI
C
      X0 =Q(LVTXT+7)
      Y0 =Q(LVTXT+8)
      R0 =Q(LVTXT+10)
      Z0 =Q(LVTXT+11)
      THE=Q(LVTXT+9)
      SP2=(SIN(PHI))**2
      CP2=(COS(PHI))**2
      EPHI=Q(LVTXT+16)*SCERR
      EXY= Q(LVTXT+17)*SCERR
      ETHE=Q(LVTXT+18)*SCERR
      EZ=  Q(LVTXT+19)*SCERR
      ETHE=MAX(ETHE,0.001)
      ERVEC(1,2)=0. 
      ERVEC(3,4)=0. 
      ERVEC(1,3)=0. 
      ERVEC(1,4)=0. 
      ERVEC(2,3)=0. 
C
      COOR0  = X0*COS(PHI0)+Y0*SIN(PHI0)
      XVEC(1)=-X0*SIN(PHI0)+Y0*COS(PHI0)
      XVEC(2)=TAN(PHI) 
      XVEC(3)=Z0 
      XVEC(4)=COS(THE)/SIN(THE)/COS(PHI) 
      ERVEC(1,1)=EXY**2*CP2
      ERVEC(2,2)=1./CP2**2*EPHI**2
      ERVEC(3,3)=EZ**2 
      ERVEC(4,4)=1./SIN(THE)**4/COS(PHI)**2*ETHE**2+
     1               (XVEC(4)*SIN(PHI)/COS(PHI))**2*EPHI**2
      ERVEC(2,4)=SIN(PHI)/CP2**2*COS(THE)/SIN(THE)*EPHI**2
      ERVEC(2,1)=ERVEC(1,2)
      ERVEC(3,1)=ERVEC(1,3)
      ERVEC(4,1)=ERVEC(1,4)
      ERVEC(3,2)=ERVEC(2,3)
      ERVEC(4,2)=ERVEC(2,4)
      ERVEC(4,3)=ERVEC(3,4)
 777  CONTINUE
C

  999 RETURN
      END
