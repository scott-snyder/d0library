      SUBROUTINE LDVSEG(LAYER,SECTOR,NHIT,LRWIR,IHIT
     1   ,PHI,XG,YG,VG,VAL,CHISQ,THETA,VZGTHETA,ZG,VZG,VALZ,CHISQZ,NEW)
C-----------------------------------------------------------------------
C
C  Load track segment to Zebra bank VSG0,VSG1 or VSG2 for segment in
C                            LAYER=  0    1       2
C
C  Input:  LRWIR(1:NHIT) = WIRE*2+LR (LR=0/1 for phi(hit) >/< phi(wire))
C          IHIT (1:NHIT) = pointer to hit in bank VSEC
C          PHI
C          XG,YG         = center of gravity in x,y plane
C          VG,VAL        = variance of center of gravity in XY plane and PHI
C          CHISQ         = chi_squared for fit in x,y plane
C          THETA
C          VZGTHETA      = Covariance of ZG (below) and THETA
C          ZG            = ZG of R_Z track at (XG,YG) in D0 frame
C          VZG,VALZ      = variance of ZG and THETA
C          CHISQZ        = chi_squared for fit in r,z plane
C
C  D.Zieminska April 1987
C-   Updated   4-NOV-1991   Peter M. Grudberg  Fix PATH
C    update   10-Aug-1992   Liang-ping Chen Load segment errors to VSEG
C    update   20-sep-1992   Liang-ping Chen Redefine ZG and VZG, VZGTHETA
C-   Updated  23_APR_1993   Liang-ping Chen take out IQ(LSEGM+1)=NSEGM,
C_                                          it is redefined in VUSDSG 
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IB(0:2)
      INTEGER LAYER,SECTOR,NHIT,LRWIR(*),IHIT(*),LVTRH,GZVTRH
      INTEGER NZBANK,LSEG(0:2),LSEGM,ID,ICALL,IX(0:2),MSEGM(5),NSEGM
      REAL PHI,XG,YG,THETA,VZGTHETA,ZG,CHISQ,CHISQZ,Z0
      REAL VG, VAL, VZG, VALZ
      LOGICAL NEW
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      SAVE LSEGM
      DATA IB/4HVSG0,4HVSG1,4HVSG2/
      DATA ICALL/0/
C
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('VSG0','19I 12F',IX(0))
        CALL MZFORM('VSG1','19I 12F',IX(1))
        CALL MZFORM('VSG2','19I 12F',IX(2))
        MSEGM(2)=0
        MSEGM(3)=0
        MSEGM(4)=31
        ICALL=1
      END IF
      LVTRH=GZVTRH()
      MSEGM(1)=IB(LAYER)
      MSEGM(5)=IX(LAYER)
C
C  Book new segment; overwrite previous segment if hits are shared and
C  chisq of the new one is better.
C
      IF (NEW) THEN
        CALL MZLIFT(0,LSEGM,LVTRH,-3-LAYER,MSEGM,0)
      END IF
      LSEG(LAYER)=LQ(LVTRH-3-LAYER)
      NSEGM=NZBANK(0,LSEG(LAYER))
      IQ(LSEGM-5)=NSEGM
      IQ(LSEGM+2)=LAYER*32+SECTOR
      IQ(LSEGM+3)=NHIT                  ! number of hits on track segment
      DO 200 ID=1,NHIT
        IQ(LSEGM+3+ID) =LRWIR(ID)
        IQ(LSEGM+11+ID)=IHIT(ID)
  200 CONTINUE
      DO 300 ID=NHIT+1,8
        IQ(LSEGM+3+ID) =18
        IQ(LSEGM+11+ID)=999
  300 CONTINUE
      Q(LSEGM+20)=PHI
      Q(LSEGM+21)=XG
      Q(LSEGM+22)=YG
      Q(LSEGM+23)=THETA
      Q(LSEGM+24)=CHISQ
      Q(LSEGM+25)=CHISQZ
      Q(LSEGM+26)=SQRT(VG)
      Q(LSEGM+27)=SQRT(VAL)
      Q(LSEGM+28)=VZGTHETA
      Q(LSEGM+29)=ZG
      Q(LSEGM+30)=SQRT(VZG)
      Q(LSEGM+31)=SQRT(VALZ)
 1000 RETURN
      END
