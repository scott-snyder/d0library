      SUBROUTINE ETAPHI(PX,PY,PZ,ETA,PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate D0 coordinates eta and phi
C-
C-   Inputs  : Px,Py,Pz  3 momenta of track
C-   Outputs : eta, phi
C-
C-   Created  14-FEB-1989   John Womersley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PX,PY,PZ,PT
      REAL ETA, THETA, PHI
      INCLUDE 'D0$INC:pi.def'
C----------------------------------------------------------------------

      PT=SQRT(PX**2+PY**2)

      IF (PT.EQ.0.)THEN
        ETA=99.
        PHI=0.
        Goto 999
      ENDIF

      PHI=ATAN2(PY,PX)
      IF(PHI.LT.0.)PHI=TWOPI+PHI

      IF(PZ.NE.0.)THEN
        THETA=ATAN(PT/ABS(PZ))
      ELSE
        THETA=HALFPI
      ENDIF
      ETA=-1.*LOG(TAN(THETA/2.))
      IF(PZ.LT.0.)ETA=-1.*ETA

  999 RETURN
      END
