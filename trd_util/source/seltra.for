      SUBROUTINE SELTRA(ISV,ISP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select Isajet tracks and compute roads in PHI and
C-                         THETA
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-NOV-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ANGLIM.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER CELLNB,ISV,ISP,IDENT
      REAL DCELL,PHI,PMOM,VIN(6)
      REAL PX,PY,PZ
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        DCELL =TWOPI/64.
        FIRST=.FALSE.
      END IF
      IF(NTROAD.GE.NTRSUP)GO TO 999
      PHI=Q(ISP+7)
      PMOM=Q(ISP+5)
      IF(PMOM.LT.1.)GO TO 999      !MOMENTUM CUT
      IDENT=0
C      IF(Q(ISP+6).LT.0.1)IDENT=1
C      VIN(1)=Q(ISV+7)
C      VIN(2)=Q(ISV+8)
C      VIN(3)=Q(ISV+9)
C      VIN(4)=Q(ISP+2)/PMOM
C      VIN(5)=Q(ISP+3)/PMOM
C      VIN(6)=Q(ISP+4)/PMOM
      CELLNB=PHI/DCELL
      NTROAD=NTROAD+1
      PHIMIN(NTROAD)=FLOAT(CELLNB)*DCELL
      PHIMAX(NTROAD)=PHIMIN(NTROAD)+DCELL
      PHIMIN(NTROAD)=PHIMIN(NTROAD)-DCELL*.2
      PHIMAX(NTROAD)=PHIMAX(NTROAD)+DCELL*.2
      TTHINF(NTROAD)=45.*DEGRAD
      TTHSUP(NTROAD)=135.*DEGRAD
      PX=Q(ISP+2)
      PY=Q(ISP+3)
      PZ=Q(ISP+4)
C      pt=sqrt(px**2+py**2)
C      rap=.5*alog((pmom+pz)/(pmom-pz))
C      crap=(rap+1.2)/.1
      PTTR(NTROAD)=SQRT(PX**2+PY**2)
c        WRITE(6,*)' NTROAD',NTROAD,'phi track ',PHI,
c     &  'PHIMIN,PHIMAX',PHIMIN(NTROAD),PHIMAX(NTROAD),
c     &  ' CELL',CELLNB,' tthinf,tthsup',TTHINF(NTROAD),TTHSUP(NTROAD)
      IF ((TNEVDG.GT.0).AND.(SWTDBG.EQ.1))THEN
        WRITE(LUDEBG,*)' NTROAD',NTROAD,'phi track ',PHI,
     &  'PHIMIN,PHIMAX',PHIMIN(NTROAD),PHIMAX(NTROAD),
     &  ' CELL',CELLNB,' tthinf,tthsup',TTHINF(NTROAD),TTHSUP(NTROAD)
      END IF
  999   RETURN
        END
