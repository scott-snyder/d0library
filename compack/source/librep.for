      INTEGER FUNCTION LIBREP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Repaint screen in case it has been messed up.
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    27-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$REPAINT_SCREEN
C----------------------------------------------------------------------
      LIBREP=SMG$REPAINT_SCREEN(PASTID)
      RETURN
      END
