      SUBROUTINE TLAYER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DETERMINE THE TRD STACK NUMBER
C-
C-   Inputs  : IUSET,IUDET
C-   Outputs : TSTACK(IN COMMON BLOCK /POSIT/)
C-
C-   Created  26-NOV-1987   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:POSIT.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
C----------------------------------------------------------------------
      INTEGER CEL(3),IUCOMP
      DATA CEL/4hTXC1,4hTXC2,4hTXC3/
C
      TSTACK = IUCOMP(IHDET,CEL,3)
  999 RETURN
      END
