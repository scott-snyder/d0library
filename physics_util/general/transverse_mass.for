      SUBROUTINE TRANSVERSE_MASS(PELE,PNEUT,TRMASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the transverse mass given electron 4
C-   vector and neutrino 2 vector.
C-
C-   Inputs  : PELE= ELECTRON 4 VECTOR
C-             PNEUT=NEUTRINO 2 VECTOR
C-   Outputs : TRMASS=TRANSVERSE MASS
C-   Controls: 
C-
C-   Created  29-JUL-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    PELE(*),PNEUT(*)
      REAL    TRMASS
      REAL    ETE,ETN
C----------------------------------------------------------------------
      ETE = SQRT(PELE(1)**2+PELE(2)**2)
      ETN = SQRT(PNEUT(1)**2+PNEUT(2)**2)
      TRMASS = 2.0*(ETE*ETN - PELE(1)*PNEUT(1) -PELE(2)*PNEUT(2))
      IF ( TRMASS.GT.0.0 ) THEN
        TRMASS = SQRT(TRMASS)
      ELSE
        TRMASS = 0.0
      ENDIF
  999 RETURN
      END
