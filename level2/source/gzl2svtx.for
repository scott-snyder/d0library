      FUNCTION GZL2SVTX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to VTX constants header bank SVTX
C-
C-   Returned value  : Pointer to SVTX bank; 0 if nonexistent
C-
C-   Created   4-OCT-1992   Peter M. Grudberg
C-   Updated  27-APR-1994   Liang-ping Chen  Danilo Puseljic 
C-              modified from GZSVTX for Level 2 use 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER GZL2SVTX
      INTEGER LSL2H, GZSL2H
C----------------------------------------------------------------------
      LSL2H=GZSL2H()
      IF ( LSL2H .LE. 0 ) THEN
        GZL2SVTX = 0
        GO TO 999
      ELSE
        GZL2SVTX = LC(LSL2H - 18 )
      ENDIF
C
  999 RETURN
      END
