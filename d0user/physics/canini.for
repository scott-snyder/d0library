      FUNCTION CANINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Initialize for CANALYSIS package
C-   Returned value  : true if succesful
C-
C-   Created   7-JUL-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CANINI,CALOR_INI,CHTINI
C----------------------------------------------------------------------
      CANINI=CALOR_INI().AND.CHTINI()
  999 RETURN
      END
