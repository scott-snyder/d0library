      SUBROUTINE SSWML2(B0,BX,BY,BZ,PAV,MODP,COSAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates momentum using single
C-                            bend aproximation
C-   Inputs  : B0 Tangent before magnet
C-             BX,BY,BZ Samus B hit
C-             PAV(3)  Wamus C hit
C-   Outputs : MODP Track momentum
C-             COSAM(3) Direction cosines after magnet
C-   Controls: 
C-
C-   Created  26-JUN-1994    Andre Sznajder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR
      REAL MODP,D0,PPAR1,PPAR2,PL,B0,A0
      REAL BX,BY,BZ,DX,DY,DZ,R
      REAL PAV(3),COSAM(3)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET ('PPAR1',PPAR1,IERR)
        CALL EZGET ('PPAR2',PPAR2,IERR)
        CALL EZRSET
      ENDIF
      MODP=0.
      CALL VZERO(COSAM,3)
      DX=(PAV(1)-BX)
      DY=(PAV(2)-BY)
      DZ=(PAV(3)-BZ)
      R=SQRT(DX**2+DY**2)
      A0=ATAN(R/DZ)
      B0=ATAN(B0)
      D0=ABS(B0-A0)
      IF (D0.LT.0.001) D0=0.001
      PL=PPAR1+PPAR2/SIN(D0/2.0)
      MODP=PL/ABS(COS(B0))
      R=SQRT(R**2+DZ**2)
      IF (R.GT.0.) THEN
        COSAM(1)=DX/R
        COSAM(2)=DY/R
        COSAM(3)=DZ/R
      ENDIF
*
  999 RETURN
      END
