      FUNCTION D0HPLT_SETUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do setup for D0HPLT. Book flag
C-   HISTOGRAM_DISPLAY.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   2-FEB-1990   Boaz Klima
C-   Updated  19-FEB-1990   Harrison B. Prosper
C-      Added booking of flag HISTOGRAM_DISPLAY
C-   Updated  20-FEB-1991   Harrison B. Prosper, Sharon Hagopian 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D0HPLT_SETUP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      D0HPLT_SETUP = .TRUE.
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Book flag
C
        CALL FLGBK('HISTOGRAM_DISPLAY',1)
C
C ****  Initialize D0HLPT
C
        CALL D0HINI
      ENDIF
  999 RETURN
      END
