      REAL FUNCTION XYSFUN(X,ISEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : selection criteria to project Ntuples
C-
C-   Returned value  : weight of variable
C-   Inputs  : X(*) - Ntuple variables
C-   Outputs : ISEL - selection flag
C-   Controls: 
C-
C-   Created  05-OCT-1992   Alexandre Zinchenko
C-   Updated  01-MAR-1993   A.Zinchenko - add check of X(6) - number
C-                                        of vertices
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:XYVCOM.INC'
      INTEGER ISEL, ISEL1
      REAL X(*)
C
      XYSFUN = 1.E+6
      IF (ISEL.LT.0.) THEN
        XYSFUN = 0.
        IF (INT(X(6)).EQ.1) XYSFUN = 1.
        GO TO 999
      ENDIF
      IF (ISEL.GT.10.AND.INT(X(6)).NE.1) GO TO 999
      ISEL1 = MOD (ISEL,10)
      IF (ISEL1.EQ.1) THEN ! X-coordinates
        IF (X(3).GT.0.25) XYSFUN = 
     &     (XYBN(2)-XYBO(2))/TAN(X(4)) + X(1)
      ELSE IF (ISEL1.EQ.2) THEN ! Y-coordinates
        IF (X(3).LT.-0.25) XYSFUN = 
     &     (XYBN(1)-XYBO(1))*TAN(X(4)) + X(2)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
