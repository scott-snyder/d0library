      SUBROUTINE CPATHS(PATH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set ZEBRA path to follow appropriate STP header.
C-                         If not called, CPATHG returns 'STPC'
C-
C-   Inputs  : PATH = Character*4 'STPC', 'STPO' or 'STPN'
C-             
C-   ENTRY CPATHD(PATH)  set default (if not called, default is 'STPC')
C-
C-   ENTRY CPATHG(PATH)  get PATH value
C-
C-   ENTRY CPATHR        reset PATH to default
C-
C-   Created  30-MAY-1989   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*4 PATH,CPATDF,CPATST
      SAVE CPATDF,CPATST
      DATA CPATDF,CPATST/'STPC','STPC'/
C----------------------------------------------------------------------
C
      CPATST = PATH
      RETURN
C
      ENTRY CPATHG(PATH)
      PATH = CPATST
      RETURN
C
      ENTRY CPATHD(PATH)
      CPATDF = PATH
      RETURN
C
      ENTRY CPATHR
      CPATST = CPATDF
      RETURN
C
      END


