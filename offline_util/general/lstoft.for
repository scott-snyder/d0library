      SUBROUTINE LSTOFT(TIMSTR, ITIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert local time in string to D0 standard.
C-
C-   Inputs  : TIMSTR      Local time string
C-   Outputs : ITIM        D0 standard time
C-   Controls: None
C-
C-   Created  29-JUN-1989   Jason McCampbell (MSU)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TIMSTR
      INTEGER ITIM
      INTEGER OFFSET, IER
C----------------------------------------------------------------------
      CALL STROFT(TIMSTR, ITIM, IER)    ! IER can be ignored
      CALL GETOFS(OFFSET)
      ITIM=ITIM+OFFSET                  ! Convert to local time
C
  999 RETURN
      END
