      SUBROUTINE MUTPRO(IHIT,SLA,SLYA,SLBC,SLYBC,ZG,XGBC,
     A  XGA,YG,YGA,XYZW,XYP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get the projected coordinates of a muon
C-                         hit on a track
C-
C-   Inputs  : LHIT - pointer to hit in MUOH bank
CC           SLBC,SLA,SLY,SLYA - SLOPE IN BC AND A LAYER AND NONBEND VIEW
CC           ZG                - CENTERS OF FITS
CC           XGBC,XGA      - CENTERS OF FITS IN BEND VIEW
CC           YG,YGA        - CENTER OF FIT IN NONBEND VIEW
C-   Outputs : XYZ(3) - coordinates of wire after rotation correction
C              XYP(2) - coordinate of track extrapolation
C              IER - 0 for success      
C-   Controls: 
C-
C-   Created  22-JUL-1992   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IQUAD,GZMUHT,GZMUOH,LMUHT,LMUOH,NMUSRT,LP,IADD
      INTEGER NMOD,NPLN,NWIR,IERR,JQUAD,LAYER
      INTEGER IORENT,MULAYR,MUQUAD
      REAL SLBC,SLA,SLYBC,SLYA,ZG,XGBC,XGA,YG,YGA
      REAL COST,ANGLE,XX,YY,ZZ
      REAL XP,YP,WLEN
      INTEGER IOR,IODD,IER
      REAL DX,DY,DZ,XXX,YYY,ZZZ,DXX,DYY,DZZ
      INTEGER IHIT
      REAL XYZW(3),XYP(2)
C
      INTRINSIC ABS,ACOS
      EXTERNAL MULAYR,MUQUAD,GZMUHT,GZMUOH
C-------------------------------------------------------------------------
CC
      CALL VZERO(XYZW,3)
      CALL VZERO(XYP,2)
      IER = 1
C get pointers and check for legitimate hit bank
      LMUHT=GZMUHT(0)
      IF(LMUHT.LE.0) GOTO 999
      LMUOH=GZMUOH(0)
      IF(LMUOH.LE.0) GOTO 999
      NMUSRT=IQ(LMUHT+2)
      IF(IHIT.GT.NMUSRT) GOTO 999
C decode hit information
      LP=28*(IHIT-1)+LMUOH
      IADD=IQ(LP+1)       ! WIRE ADDRESS
      CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR)
      JQUAD=MUQUAD(NMOD)
      LAYER=MULAYR(NMOD)
      IORENT=IABS(IQ(LP+5))
      IF(LAYER.EQ.1) THEN      ! A LAYER
        COST=1./SQRT(1.+SLA**2)
      ELSE    ! BC LAYER
        COST=1./SQRT(1.+SLBC**2)
      ENDIF
      ANGLE=ACOS(COST)
CC
CC    FILL UP ARRAY OF HITS ON TRACK XX=BEND,YY=NONBEND,ZZ=BETWEEN PLANES
CC
      IF(IORENT.EQ.1) THEN
        XX=Q(LP+23)
        YY=Q(LP+22)
        ZZ=Q(LP+21)
      ELSE IF(IORENT.EQ.2) THEN
        XX=Q(LP+23)
        YY=Q(LP+21)
        ZZ=Q(LP+22)
      ELSE IF(IORENT.EQ.3) THEN
        XX=Q(LP+21)
        YY=Q(LP+22)
        ZZ=Q(LP+23)
      ELSE IF(IORENT.EQ.4) THEN
        XX=Q(LP+22)
        YY=Q(LP+21)
        ZZ=Q(LP+23)
      ENDIF
CC  PROJECT SEGMENT TO THIS CELL
      IF(LAYER.EQ.1) THEN
        XP=XGA+SLA*(ZZ-ZG)
        YP=YGA+SLYA*(ZZ-ZG)
      ELSE
        XP=XGBC+SLBC*(ZZ-ZG)
        YP=YG+SLYBC*(ZZ-ZG)
      ENDIF
CC   SKIP OUT IF 'FAR' AWAY BEFORE DOING ROTATIONS
      WLEN=Q(LP+24)
      IF(ABS(YP-YY).GT.WLEN/2.+30.) GO TO 999   ! OUTSIDE CHAMBER
CC    DO ROTATIONS   
      IF(IORENT.EQ.1) THEN
        XXX=ZZ
        YYY=YP
        ZZZ=XP
      ELSE IF(IORENT.EQ.2) THEN
        XXX=YP
        YYY=ZZ
        ZZZ=XP
      ELSE IF(IORENT.EQ.3) THEN
        XXX=XP
        YYY=YP
        ZZZ=ZZ
      ELSE IF(IORENT.EQ.4) THEN
        XXX=YP
        YYY=XP
        ZZZ=ZZ
      ENDIF
      DXX=0.
      DYY=0.
      DZZ=0.
      CALL MUROTC(IADD,XXX,YYY,ZZZ,DX,DY,DZ,IER)
CCC   ADD ROTATION CORRECTION TO HIT
      IF(IER.EQ.0) THEN
        DXX=DX
        DYY=DY
        DZZ=DZ
        IF(IORENT.EQ.1) THEN
          XX=XX+DZ
          YY=YY+DY
          ZZ=ZZ+DX
        ELSE IF(IORENT.EQ.2) THEN
          XX=XX+DZ
          YY=YY+DX
          ZZ=ZZ+DY
        ELSE IF(IORENT.EQ.3) THEN
          XX=XX+DX
          YY=YY+DY
          ZZ=ZZ+DZ
        ELSE IF(IORENT.EQ.4) THEN
          XX=XX+DY
          YY=YY+DX
          ZZ=ZZ+DZ
        ENDIF
CC  PROJECT TO THIS CELL
        IF(LAYER.EQ.1) THEN
          XP=XGA+SLA*(ZZ-ZG)
          YP=YGA+SLYA*(ZZ-ZG)
        ELSE
          XP=XGBC+SLBC*(ZZ-ZG)
          YP=YG+SLYBC*(ZZ-ZG)
        ENDIF
      ENDIF
      IER = 0
      XYZW(1) = XX
      XYZW(2) = YY
      XYZW(3) = ZZ
      XYP(1) = XP
      XYP(2) = YP
  999 RETURN
      END
