      SUBROUTINE GTL2FWAL(MODULE,DIMENS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the dimensions and location of a layer in 
C-                         the FDC, inner theta, outer theta, or phi 
C-
C-   Inputs  : MODULE = the FDC MODULE involved, 0=I.T.,1=O.T.,2=Phi,
C-                                               +3 for Half 1
C-   Outputs : DIMENS(1)=inner radius of volume
C-             DIMENS(2)=outer radius of volume
C-             DIMENS(3)=half-thickness of volume
C-             DIMENS(4)=x-position of center of volume from I.R. zero
C-             DIMENS(5)=y-position of center of volume from I.R. zero
C-             DIMENS(6)=z-position of center of volume from I.R. zero
C-
C-   Created  14-APR-1989   Jeffrey Bantly
C-   Updated  29-MAY-1990   Jeffrey Bantly  output exact values, not local 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER MODULE,LAYER,IOFSET,NPARVL
      INTEGER LKFWAL,I,LKFGEH
      INTEGER GZL2FWAL,GZL2FGEH
C
      REAL    DIMENS(6),XC,YC,ZC,DIR
C----------------------------------------------------------------------
      CALL VFILL(DIMENS,6,-1.)        ! default, can be checked for error
      IF(MODULE.LT.0 .OR. MODULE.GT.5) GOTO 999
      LAYER = MODULE
      IF(MODULE.GT.2) LAYER=MODULE-3
      DIR = -1.
      IF(MODULE.GT.2) DIR =1.
C
      LKFGEH = GZL2FGEH()
      IF(LKFGEH.LE.5) GOTO 999
      LKFWAL = GZL2FWAL()
      IF(LKFWAL.LE.5) GOTO 999
C
      XC = C(LKFGEH+6)
      YC = C(LKFGEH+7)
      ZC = C(LKFGEH+8)
      NPARVL = IC(LKFWAL+2)
      IOFSET = 2+LAYER*NPARVL+1
      DIMENS(1)=C(LKFWAL+IOFSET+1)             ! x half-thick
      DIMENS(2)=C(LKFWAL+IOFSET+2)             ! y half-thick
      DIMENS(3)=C(LKFWAL+IOFSET+3)             ! z half-thick
      DIMENS(4)=XC+C(LKFWAL+IOFSET+6)          ! x center
      DIMENS(5)=YC+C(LKFWAL+IOFSET+7)          ! y center
      IF(LAYER.LE.1) DIMENS(6)=(ZC-C(LKFWAL+IOFSET+8))  ! z center
      IF(LAYER.EQ.2) DIMENS(6)=(ZC+C(LKFWAL+IOFSET+8))  ! z center
      DIMENS(6)=DIMENS(6)*DIR
C
C----------------------------------------------------------------------
  999 RETURN
      END
