      LOGICAL FUNCTION NEWRUN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Return true for new runs
C-
C-   ENTRY STNRUN(FL)
C-   FL= sets value of NEWRUN    
C-   Created  29-SEP-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FL,STNRUN,NRUNFL
C----------------------------------------------------------------------
C
      NEWRUN=NRUNFL
      RETURN
C
      ENTRY STNRUN(FL)
      NRUNFL=FL
C
  999 RETURN
      END
