      SUBROUTINE FGETZ0(ITRK,Z0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value of Z0 for an FDC track.
C-
C-   Inputs  : ITRK = FDC track number
C-   Outputs : Z0
C-   Controls: 
C-
C-   Created  10-OCT-1991   Susan K. Blessing
C-   Updated  12-DEC-1991   Susan K. Blessing  Check LFDCT value.  If
C-    0, return Z0=99999.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER ITRK
      INTEGER LFDCT,GZFDCT
C
      INTEGER ICONT(10)
      REAL CONT(10)
      EQUIVALENCE(CONT,ICONT)
C
      REAL Z0
C
C----------------------------------------------------------------------
C
      CALL GTFTRH(ICONT)
      LFDCT = GZFDCT(ITRK)
      IF (LFDCT.GT.0) THEN
        Z0 = CONT(3+IBITS(IQ(LFDCT+1),0,1))
      ELSE
        Z0 = 99999.
      END IF
C
  999 RETURN
      END
