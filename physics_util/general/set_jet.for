      SUBROUTINE SET_JET(JETTYPE, CONESIZE, STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simplified interface to select jets. This
C-   calls SET_CAPH. Use RESET_JET to undo SET_JET.
C-
C-   Inputs  : JETTYPE  [C*]  Type of Jet
C-             CONESIZE [R]   ConeSize (unused for NN-Jets)
C-   Outputs : STATUS   [I]   0 - OK
C-   Controls:
C-
C-   Created  29-APR-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) JETTYPE
      REAL    CONESIZE
      INTEGER STATUS
C----------------------------------------------------------------------
      REAL    TEMPLATE(3)
      CHARACTER*16 NAME
C----------------------------------------------------------------------
      NAME = JETTYPE(1:LEN(JETTYPE))
      CALL UPCASE(NAME, NAME)
C
      IF ( NAME(1:4) .EQ. 'CONE' ) THEN
        TEMPLATE(1) = 1.0       ! Number of fields
        TEMPLATE(2) = 6.0       ! Position of field
        TEMPLATE(3) = CONESIZE  ! Value of field
      ELSE
        TEMPLATE(1) = 0.0
      ENDIF
C
      CALL SET_CAPH(NAME, TEMPLATE, STATUS)
      RETURN
C
      ENTRY RESET_JET
      CALL RESET_CAPH
      RETURN
      END
