      FUNCTION IN_IETA_WINDOW(IETA,IETA0,IDELETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns TRUE if eta index inside the window.
C-
C-   Inputs  : IETA         [I] eta index to test if inside window
C-             IETA0        [I] eta index of window center
C-             IDELETA      [I] +/- half size of window 
C-   Outputs : None
C-   Controls: None
C-
C-   Created  17-DEC-1992   Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL IN_IETA_WINDOW
      INTEGER IETA,IETA0,IDELETA
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C----------------------------------------------------------------------
      INTEGER IETA_HI,IETA_LO
C
      IN_IETA_WINDOW = .FALSE.
C
      IF ( (IETA .LT. -NETAL) .OR. 
     &     (IETA .EQ. 0     ) .OR.
     &     (IETA .GT.  NETAL) )                   GO TO 999
      IF ( (IETA0.LT. -NETAL) .OR. 
     &     (IETA0.EQ. 0     ) .OR.
     &     (IETA0.GT.  NETAL) )                   GO TO 999
      IF (IDELETA .LT. 0)                         GO TO 999
C
      IETA_LO = IETA0 - IDELETA
      IF ( IETA_LO*IETA0 .LE. 0 ) IETA_LO = IETA_LO - 1
      IETA_HI = IETA0 + IDELETA
      IF ( IETA_HI*IETA0 .LE. 0 ) IETA_HI = IETA_HI + 1
C
      IF ( (IETA .GE. IETA_LO) .AND. (IETA .LE. IETA_HI) ) THEN
        IN_IETA_WINDOW = .TRUE.
      ENDIF
C
  999 RETURN
      END
