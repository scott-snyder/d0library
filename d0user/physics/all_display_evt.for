      FUNCTION ALL_DISPLAY_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Generate banks needed for display
C-   Returned value  : true if succesful
C-
C-   Created  25-OCT-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZCAEP
      LOGICAL ALL_DISPLAY_EVT
C----------------------------------------------------------------------
C
      IF(GZCAEP().NE.0) THEN
        CALL CAEHFL        ! fill CAEH
        CALL CATEFL        ! fill CATE
      ENDIF
      ALL_DISPLAY_EVT=.TRUE.
  999 RETURN
      END
