      SUBROUTINE STRCOP(SRC, DST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Copy SRC to DST.
C-
C-   Inputs  :
C-    STR : The string to copy.
C-    
C-   Outputs :
C-    DST : The string to which it should be copied.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-   Updated  18-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) SRC, DST
C----------------------------------------------------------------------
      DST = SRC
  999 RETURN
      END
