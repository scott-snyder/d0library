      FUNCTION GZFAKE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      get pointer to FAKE bank
C-   Returned value  : pointer
C-
C-   Created  28-JUN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZFAKE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFAKE.LINK'
C----------------------------------------------------------------------
      GZFAKE=LQ(LHEAD-IZFAKE)
  999 RETURN
      END
