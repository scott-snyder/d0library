      FUNCTION MRDELT(DELT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inverse of Function MSDELT.
C-                         Calculate Z position from given Delta time.
C-
C-   Returned value  : MRDELT ; Z-coordinate from the far end (cm).
C-   Inputs  : DELT ; Delta Time (nsec)
C-
C-   Created   31-MAY-1991   Susumu Igarashi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ND
      REAL    DELT,MRDELT,V
      DATA V / 28.0/               ! Signal propagation speed (cm/nsec)

      IF(DELT.LT.35.0) THEN
        MRDELT=DELT*V/2.
      ELSE
        MRDELT=(DELT+0.03*490.0)/(2./V+0.03)
      ENDIF

  999 RETURN
      END
