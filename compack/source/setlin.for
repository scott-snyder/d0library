C&IF VAXVMS
      SUBROUTINE SETLIN(INUM)
C&ELSE
C&      SUBROUTINE SETLIN_UNIX(INUM)
C&ENDIF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set number of lines in upper part of split
C-                         screen. The actual split is done in SPLTIT.
C-
C-   Inputs  : INUM: Number of lines in upper part of screen
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   modified   29-OCT-1990   Scott Snyder
C-    don't give an error if INUM = SPLLIN and things are otherwise ok.
C-   Updated 13-Jul-1992   Herbert Greenlee
C-     Cannot have setlin defined on sgi because of naming conflict.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUM
C----------------------------------------------------------------------
      CALL SET_SPLIT_SCREEN(INUM)
      RETURN
      END
