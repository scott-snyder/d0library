      FUNCTION ALL_DISPLAY_DMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      event dump for full display
C-   Returned value  : true
C-
C-   Created  25-OCT-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ALL_DISPLAY_DMP
      LOGICAL CALDMP,ZTRDMP,MURECO_DUMP,VERDMP,VEEDMP
C----------------------------------------------------------------------
      ALL_DISPLAY_DMP=VERDMP().AND.CALDMP().AND.ZTRDMP()
     &  .AND.MURECO_DUMP().AND.VEEDMP()
      CALL ISA_DUMP
  999 RETURN
      END
