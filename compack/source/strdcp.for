      SUBROUTINE STRDCP(SRC, DST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Copy the descriptor SRC to DST.
C-
C-   Inputs  :
C-    SRC : The descriptor to copy.
C-    
C-   Outputs :
C-    DST : The descriptor to which it should be copied.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-   Updated  18-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE '($DSCDEF)'
      RECORD /DSCDEF1/ SRC, DST
C&ELSE
C&      REAL*8 SRC,DST               ! 8-bytes long
C&ENDIF
C----------------------------------------------------------------------
      DST = SRC
  999 RETURN
      END
