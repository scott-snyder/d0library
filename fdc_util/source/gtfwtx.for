      SUBROUTINE GTFWTX(HALF,QUAD,SECTOR,DIMENS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the dimensions and location of a Theta
C-                         sector in the FDC.
C-
C-   Inputs  : HALF   = FDC half 
C-             QUAD   = quadrant
C-             SECTOR = sector
C-   Outputs : DIMENS(1)=x half-thickness of box volume
C-             DIMENS(2)=y half-thickness of box volume
C-             DIMENS(3)=z half-thickness of box volume
C-             DIMENS(4)=x-position of center of volume from I.R. zero
C-             DIMENS(5)=y-position of center of volume from I.R. zero
C-             DIMENS(6)=z-position of center of volume from I.R. zero
C-
C-   Created  14-APR-1989   Jeffrey Bantly
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER HALF,MODULE,LAYER,QUAD,SECTOR,IOFSET,NPARVL
      INTEGER LKFWTX,I
      INTEGER GZFWTA,GZFWTB
C
      REAL    DIMENS(6),DIMENC(6),ANG,DX,DY,DZ,XC,YC,ZC,RC
C----------------------------------------------------------------------
      CALL VFILL(DIMENS,6,-1.)
      IF(SECTOR.GT.5) GOTO 999
      IF( QUAD.EQ.0 .OR. QUAD.EQ.2 .OR. QUAD.EQ.4 .OR. QUAD.EQ.6 ) THEN
        LKFWTX = GZFWTB()
      ELSE
        LKFWTX = GZFWTA()
      ENDIF
      IF(LKFWTX.LE.5) GOTO 999
C
      LAYER =0
      IF(QUAD.GT.3) LAYER =1
      ANG = (QUAD*90.)+45.
      IF(QUAD.GT.3) ANG=ANG-45.
      ANG=ANG*PI/180.
      NPARVL = IC(LKFWTX+2)
      IOFSET = 9+(SECTOR*NPARVL)
      MODULE=HALF*3+LAYER
      CALL GTFWAL(MODULE,DIMENC)
      IF(DIMENC(1).LT.0.0) GOTO 999
C
      DX=C(LKFWTX+IOFSET+2)
      DY=C(LKFWTX+IOFSET+3)
      DZ=C(LKFWTX+IOFSET+4)
      XC=C(LKFWTX+IOFSET+5)
      YC=C(LKFWTX+IOFSET+6)
      ZC=0.
      RC=SQRT(XC**2 + YC**2)
C
      DIMENS(1)=DX
      DIMENS(2)=DY
      DIMENS(3)=DZ
      DIMENS(4)=DIMENC(4)+RC*COS(ANG)
      DIMENS(5)=DIMENC(5)+RC*SIN(ANG)
      DIMENS(6)=DIMENC(6)
C
C----------------------------------------------------------------------
  999 RETURN
      END
