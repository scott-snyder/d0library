      FUNCTION ALL_DISPLAY_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Initialize all detectors for event display
C-
C-   Returned value  : true for succesful initialization
C-
C-   Modified 16-FEB-1994   Nobuaki Oshima
C-                          MUNGT_INI was replaced by MUONLY_INI
C-   Created  25-OCT-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ALL_DISPLAY_INI
      LOGICAL ZTRINI,CALOR_INI,CHTINI,MUONLY_INI,OK,VEEINI
C----------------------------------------------------------------------
      OK=.TRUE.
      OK=OK.AND.ZTRINI()
      OK=OK.AND.CALOR_INI()
      OK=OK.AND.CHTINI()
      OK=OK.AND.MUONLY_INI()
      CALL VEEINI
      ALL_DISPLAY_INI=OK
  999 RETURN
      END
