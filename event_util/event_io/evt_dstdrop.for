      FUNCTION EVT_DSTDROP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     dummy hook. Called to drop banks from DST stream
C-   Returned value  : true
C-
C-   Created  11-FEB-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EVT_DSTDROP
C----------------------------------------------------------------------
      EVT_DSTDROP=.TRUE.
  999 RETURN
      END
