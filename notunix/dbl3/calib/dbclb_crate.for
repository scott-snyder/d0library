      SUBROUTINE DBCLB_CRATE(DECT, NCR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns total number of crates
C-
C-   Inputs  : DECT   detector name
C-   Outputs : NCR    number of crates/modules
C-   Controls: 
C-
C-   Created  24-MAR-1991   SHAHRIAR ABACHI
C-   Updated  15-DEC-1992   Haowei XU Added Level0
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DECT
      INTEGER NCR
C
      NCR = 1
      IF(DECT(1:3) .EQ. 'CAL') NCR = 6
      IF(DECT(1:3) .EQ. 'MUO') NCR = 307
      IF(DECT(1:3) .EQ. 'CDC') NCR = 6
      IF(DECT(1:3) .EQ. 'FDC') NCR = 12
      IF(DECT(1:3) .EQ. 'TRD') NCR = 8
      IF(DECT(1:3) .EQ. 'VTX') NCR = 10
      IF(DECT(1:3) .EQ. 'SAM') NCR = 36
      IF(DECT(1:3) .EQ. 'LV0') NCR = 1
C
C----------------------------------------------------------------------
  999 RETURN
      END
