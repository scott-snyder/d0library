      SUBROUTINE ENDDMP(IUNI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End dump to file, use editor to look at file
C-                         when unit is 6
C-
C-   Inputs  : IUNI : Unit number used for dump
C-   Outputs : None
C-
C-   Created  29-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IUNI
C----------------------------------------------------------------------
      CLOSE(IUNI)
      IF(IUNI.EQ.6) THEN 
        CALL EVESAV
      ENDIF
  999 RETURN
      END
