      SUBROUTINE GTFDTX(QUAD,SECTOR,DIMENS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the dimensions and location of a unit in 
C-                         the FDC, inner theta or outer theta 
C-
C-   Inputs  : QUAD,SECT = quadrant(0:7),sector(0:5)
C-   Outputs : DIMENS(1)=x half-thickness of box volume
C-             DIMENS(2)=y half-thickness of box volume
C-             DIMENS(3)=z half-thickness of box volume
C-             DIMENS(4)=x-position of center of volume
C-             DIMENS(5)=y-position of center of volume
C-             DIMENS(6)=z-position of center of volume
C-
C-   Created  14-APR-1989   Jeffrey Bantly
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER,QUAD,SECTOR,DUM,IOFSET,NPARVL
      INTEGER LKFDTX,I
      INTEGER GZFDTA,GZFDTB
C
      REAL    DIMENS(6),DIR
C----------------------------------------------------------------------
      IF( QUAD.EQ.0 .OR. QUAD.EQ.2 .OR. QUAD.EQ.4 .OR. QUAD.EQ.6 ) THEN
        LKFDTX = GZFDTA(DUM)
      ELSE
        LKFDTX = GZFDTB(DUM)
      ENDIF
      DIR = 1.
      LAYER = 0
      IF( QUAD .GE. 2 ) DIR =-1.
      IF( QUAD .GE. 4 ) DIR = 1.
      IF( QUAD .GE. 6 ) DIR =-1.
      IF( QUAD .GE. 4 ) LAYER = 1
      NPARVL = IC(LKFDTX+3)
      IOFSET = 5+38+(SECTOR*NPARVL)
      DIMENS(1)=C(LKFDTX+IOFSET+2)
      DIMENS(2)=C(LKFDTX+IOFSET+3)
      DIMENS(3)=C(LKFDTX+IOFSET+4)
      DIMENS(4)=C(LKFDTX+IOFSET+5)*DIR
      DIMENS(5)=C(LKFDTX+IOFSET+6)*DIR
      DIMENS(6)=C(LKFDTX+IOFSET+7)*(-1.)**(LAYER+1)
C----------------------------------------------------------------------
  999 RETURN
      END
