      SUBROUTINE LNKENP(L,IDCLGA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to link CENP banks to appropriate CLGA bank
C-
C-   Inputs  :       L       pointer to CENP bank
C-                   IDCLGA  identification of the 1st CLGA region
C-   Outputs :       NONE
C-   Controls:       NONE
C-   Zebra Banks modified:   CENP
C-
C-   Created   8-SEP-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CALGEO.INC'
      INCLUDE 'D0$INC:CLGA.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
C
      COMMON / LCLGA / LCLGA, LFIRST, LTUBE, LORIG
      INTEGER LCLGA(2), LFIRST, LTUBE, LORIG, L, LZFIND, IDCLGA
      INTEGER IDEN, LLAST
      CHARACTER*4 BANK
C
      LFIRST = L               ! Save first bank 
      LQCENP = L
  100 IF(LQCENP .EQ. 0) GO TO 200
      WRITE (BANK,'(A4)') IQ(LQCENP-4)
      IF( BANK .NE. 'CENP') GO TO 150
      IDEN = IQ(LQCENP + IGIDEN) - IQ(LFIRST + IGIDEN) + IDCLGA
      LQCLGA = LZFIND(IXCDV,LQ(LQCREG-IXCLGA),IDEN,IGIDEN)
      LQ(LQCENP - IZELGA) = LQCLGA    ! link CLGA to CENP
      LLAST = LQCENP
  150 LQCENP = LQ(LQCENP)
      GO TO 100
  200 LQCENP = LFIRST          ! finished
C----------------------------------------------------------------------
  999 RETURN
      END
