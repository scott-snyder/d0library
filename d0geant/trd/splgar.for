      SUBROUTINE SPLGAR(EPL1,EPL2,XFOIL,XGAP,DXFOIL,DXGAP,NFOIL,GAMMA,
     >FOI,GAP)
C======================================================================
C
C   Purpose and Methods :1)Call to Garibian Xray generator (DWDEGR) for a
C                          limited number of values
C-                       2)Interpolate with a SPINE fit to get values
C-                        for all the energies defined in the common
C-                        block XRAY
C
C   Inputs  : Radiator characteristics
C-            GAMMA of the particle
C   Outputs :GDGAP(I) (I=1,NSTEP) Radiated Energy in energy bin
C                                 startind at ED(I) and XSTEP wide.
C                                 Used in routine TRDYLD
C
C-   Created   7-OCT-87    B.MANSOULIE
C-   Updated  12-JUL-1989   A. Zylberstejn
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:XRAY.INC'
      INTEGER I,IFOIS,IOP,NFOIL,NSS1
      PARAMETER(NSS1=250)
      INTEGER NDIM
      PARAMETER(NDIM =15 )
      CHARACTER *3 FOI,GAP
      REAL X(NDIM),Y(NDIM),DERIV(NDIM,2)
      REAL Z(NSS1),FVALUE(NSS1),FDERIV(NSS1,2)
      REAL EPL1,EPL2,XFOIL,XGAP,DXFOIL,DXGAP,GAMMA,DWDEGR
      REAL DXGI
      REAL EI,STEPG
      LOGICAL FIRST
C
      DATA X/1.95,3.95,5.95,7.95,9.95,11.95,13.95,15.95,17.95,19.95,
     +21.95,23.95,25.95,27.95,29.95/
      DATA DXGI/-1000./
      DATA FIRST/.TRUE./
      DATA IFOIS/0/
      IF(FIRST)THEN
        FIRST=.FALSE.
C    Compute step size for primary  X ray spectrum
        STEPG=(ED(NSTEP)*1.02-ED(1)*.98)/FLOAT(NDIM-1)
        EI=ED(1)*.98
        DO 10 I=1,NDIM
          X(I)=EI
          EI=EI+STEPG
   10   CONTINUE
      END IF
C      IF(DXGAP.NE.DXGI)THEN
C        WRITE(LOUT,*)' DELTA GAP= ',DXGAP
C        DXGI=DXGAP
C      END IF
      IOP=-1
      DO 30 I=1,NDIM
        Y(I)=DWDEGR(X(I),EPL1,EPL2,XFOIL,XGAP,DXFOIL,DXGAP,NFOIL,GAMMA,
     >  FOI,GAP)
   30 CONTINUE
C
C  Now do the SPLINE fit
C
      CALL SPLIN3(X,Y,DERIV,NDIM,NDIM,ED,GDGAP,FDERIV,NSTEP,
     + NSTEP,IOP)
  800 FORMAT(10F10.4)
  999 RETURN
      END
