      SUBROUTINE VDTMAX(WIRE,SECTOR,LAYER,TPLS,TMIN,DPLS,DMIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the maximum values of drift time for which 
C-               the distance time map is good
C-   
C-
C-   Inputs  : WIRE,SECTOR,LAYER  -- usual, starting from 0
C-   Outputs : TPLS,TMIN -- maximum drift times parametrized for drift n
C-                          positive and negative PHI direction
C-             DPLS,DMIN -- corresponding distances -- DPLS > 0, DMIN < 0
C-   Controls: 
C-
C-   Created   3-JUN-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
c I/O:
      INTEGER WIRE,SECTOR,LAYER
      REAL    DPLS,DMIN,TPLS,TMIN
c Locals:
      INTEGER LVDTM,ITEMS,SIZE,PT,N,OFFSET,SIDE
      REAL DMAX(0:1),TMAX(0:2)
c Externals
      INTEGER GZVDTM
C----------------------------------------------------------------------
      LVDTM = GZVDTM(LAYER,SECTOR)
      ITEMS = IC(LVDTM+3)
      SIZE =  IC(LVDTM+5)
      DO SIDE = 0,1
        OFFSET = LVDTM + 22 + 2*ITEMS*WIRE + 16*ITEMS*SIDE
        PT = LVDTM + 22 + 32*ITEMS + IC(OFFSET+1)
        N  = IC(OFFSET+2)
        DMAX(SIDE) = C(PT + N - 1)
        TMAX(SIDE) = C(PT + SIZE + N - 1)
      ENDDO
      IF (MOD(SECTOR,2) .EQ. 0) THEN
        TPLS = TMAX(0)
        TMIN = TMAX(1)
        DPLS =-DMAX(0)
        DMIN =-DMAX(1)
      ELSE
        TPLS = TMAX(1)
        TMIN = TMAX(0)
        DPLS = DMAX(1)
        DMIN = DMAX(0)
      ENDIF
  999 RETURN
      END
