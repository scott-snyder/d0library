      SUBROUTINE SET_SHOWER_PARAM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset shower parametrization on a track-by-
C-                         track basis
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-JUN-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
C
      REAL PTRAN
C----------------------------------------------------------------------
      IF (SHWG .NE. 5) GO TO 200
C
C   Turn parametrization on for hadronic primary tracks and daughters;
C   turn it off for electromagnetic primary tracks.
C
      IF (ISTAK .NE. 0) GO TO 200 ! ISAJET primary track
      IF (IPART .GT. 0 .AND. IPART .LE. 3) THEN
        LINN_PARAM = .FALSE.  ! Electromagnetic particle
      ELSE
        LINN_PARAM = .TRUE.   ! Non-electromagnetic particle
      ENDIF
C
C   Add pt cut to the Linn parametrization, if it is being used
C
  200 IF (LINN_PARAM) THEN
        CALL SET_ELECTRON_PARS
      ENDIF
                          
  999 RETURN
      END
