      SUBROUTINE FTRKROT(LVTXT,SCERR,COOR0,XVEC,ERVEC) 
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
C-   Inputs  :  LVTXT: VTX bank address 
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
      REAL COOR0,SCERR, TANT, OCT2
C
      PHI=Q(LVTXT+6)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
C
      X0 =Q(LVTXT+7)
      Y0 =Q(LVTXT+8)
      R0 =Q(LVTXT+10)
      Z0 =Q(LVTXT+11)
      THE=Q(LVTXT+9)
      SP2=(SIN(PHI))**2
      CP2=(COS(PHI))**2
      TANT=SIN(THE)/COS(THE)
      OCT2=1./COS(THE)**2
      EPHI=Q(LVTXT+16)*SCERR
      EXY= Q(LVTXT+17)*SCERR
      ETHE=Q(LVTXT+18)*SCERR
      EZ=  Q(LVTXT+19)*SCERR
      ETHE=MAX(ETHE,0.001)
C
C  Increase r-z error
C
      ETHE=ETHE*10.
      EZ=EZ*10.
      ERVEC(1,2)=0. 
      ERVEC(3,4)=0. 
      ERVEC(1,3)=0. 
      ERVEC(1,4)=0. 
      ERVEC(2,3)=0. 
C
      COOR0  = Z0 
      XVEC(1)= X0 
      XVEC(2)= COS(PHI)*TANT
      XVEC(3)= Y0 
      XVEC(4)= SIN(PHI)*TANT
C     ERVEC(1,1)=EXY**2*SP2
      ERVEC(1,1)=EZ**2*XVEC(2)**2
      ERVEC(2,2)=SP2*TANT**2*EPHI**2 + CP2*OCT2**2*ETHE**2
C     ERVEC(3,3)=EXY**2*CP2
      ERVEC(3,3)=EZ**2*XVEC(4)**2
      ERVEC(4,4)=CP2*TANT**2*EPHI**2 + SP2*OCT2**2*ETHE**2 
      ERVEC(2,4)=COS(PHI)*SIN(PHI)*(OCT2**2*ETHE**2-TANT**2*EPHI**2) 
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
