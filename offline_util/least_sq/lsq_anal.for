      SUBROUTINE LSQ_ANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Analyze LSQ
C-
C-   Inputs  :IUSE = 1, DO HISTOGRAMS ETC. OTHERWISE RETURN
C-            IUSE =1 IS THE H MATRIX RELEVANT TO PRESENT DATA SAMPLE.
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL first
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      CALL DHDIR('LSQ_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('LSQ','LSQ_ANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('LSQ_RCP')
C
C ****  BOOK HISTOGRAMS HERE
C
        CALL DO_HBOOK('VISIBLE_HISTOGRAMS')     ! BOOK THEM
C
        CALL EZRSET
C
      ENDIF
C
  999 RETURN
      END
