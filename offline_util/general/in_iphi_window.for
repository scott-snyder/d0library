      FUNCTION IN_IPHI_WINDOW(IPHI,IPHI0,IDELPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns TRUE if phi index inside the window.
C-
C-   Inputs  : IPHI         [I] phi index to test if inside window
C-             IPHI0        [I] phi index of window center
C-             IDELPHI      [I] +/- half size of window 
C-   Outputs : None
C-   Controls: None
C-
C-   Created  17-DEC-1992   Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL IN_IPHI_WINDOW
      INTEGER IPHI,IPHI0,IDELPHI
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C----------------------------------------------------------------------
      INTEGER I,IIPHI
C
      IN_IPHI_WINDOW = .FALSE.
C
      IF ( (IPHI .LE. 0) .OR. (IPHI .GT. NPHIL) ) GO TO 999
      IF ( (IPHI0.LE. 0) .OR. (IPHI0.GT. NPHIL) ) GO TO 999
      IF (IDELPHI .LT. 0)                         GO TO 999
C
      DO I=-IDELPHI,IDELPHI
        IIPHI = IPHI0+I
        IF   (IIPHI .LE. 0    ) THEN
                                IIPHI = IIPHI+NPHIL
        ELSE
          IF (IIPHI .GT. NPHIL) IIPHI = IIPHI-NPHIL
        ENDIF        
        IF (IIPHI .EQ. IPHI) THEN
          IN_IPHI_WINDOW = .TRUE.
          GO TO 999         
        ENDIF
      ENDDO
C
  999 RETURN
      END
