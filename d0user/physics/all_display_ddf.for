      FUNCTION ALL_DISPLAY_DDF()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      define dumps
C-   Returned value  : true
C-
C-   Created  25-OCT-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ALL_DISPLAY_DDF,ZTRAKS_DEFD,CADEFD,MURECO_DEFD
C----------------------------------------------------------------------
C
      CALL ISA_DEFD
      ALL_DISPLAY_DDF=ZTRAKS_DEFD().AND.CADEFD().AND.
     &  MURECO_DEFD()
  999 RETURN
      END
