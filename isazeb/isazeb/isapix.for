      FUNCTION ISAPIX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-    Provide an event display for ISAJET events.
C-    There are 2 possible displays:
C-      PLZEXE displays lego plots
C-      ISZGEX displays tracks
C-    they cannot run simultaneously, one is chosen by assigning
C-    a screen file before running the program:
C-    DEFINE PXSCREEN D0$PIXIE$DATA:LEGO.SCREEN for lego plots
C-    DEFINE PXSCREEN D0$PIXIE$DATA:ISZGRF.SCREEN
C-    
C-
C-   Returned value  : always .TRUE.
C-
C-   Created   8-MAR-1989   Serban D. Protopopescu
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISAPIX,PLZEXE
C----------------------------------------------------------------------
      IF(.NOT.PLZEXE()) CALL ISZGEX
  999 RETURN
      END
