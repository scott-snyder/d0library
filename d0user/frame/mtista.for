      CHARACTER*8 FUNCTION MTISTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return a character string for program status
C-      to be used in menus
C-   
C-   ENTRY SETMTI(STITLE)
C-   STITLE= character*8 string, value of MTISTA at next call
C-
C-   Created  30-MAR-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*8 SAVTIT,SETMTI
      CHARACTER*(*) STITLE
C----------------------------------------------------------------------
C
      MTISTA=SAVTIT
      RETURN
C
      ENTRY SETMTI(STITLE)
      SAVTIT=STITLE
      SETMTI=SAVTIT
  999 RETURN
      END
