      FUNCTION MSDELT(Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Delta Time Response funcion
C-                         considering the resolution.
C-
C-   Returned value  : MSDELT ; Delta Time
C-   Inputs  : Z ; Z-coordinate from the far end (cm).
C-
C-   Created   10-APR-1991   Susumu Igarashi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ND
      REAL    Z,MSDELT,V,RANDOM,SIGMA
      DATA V / 28.0/               ! Signal propagation speed (cm/nsec)

      IF(Z.LT.490.0) THEN
        MSDELT=Z*2./V
      ELSE
        MSDELT=Z*2./V+0.03*(Z-490.0)
      ENDIF

      SIGMA=9.0+Z*9.0/550.0             ! Delta Time resolution (cm)
      CALL NORRAN(RANDOM)
      MSDELT=MSDELT+RANDOM*SIGMA*2./V

  999 RETURN
      END
