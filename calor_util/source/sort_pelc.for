      SUBROUTINE SORT_PELC(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SORT A LINEAR CHAIN OF PELC BANKS
C-                         BY ET IN DECENDING ORDER
C-   Inputs  : NONE
C-   Outputs : POINTER TO FIRST BANK IN SORTED CHAIN
C-   Controls: 
C-
C-   Created  13-APR-1993   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPELC,GZPELC
C----------------------------------------------------------------------
      LPELC=GZPELC()
      CALL ZSORT(IXCOM,LPELC,7)
      LPELC=GZPELC()
      CALL ZTOPSY(IXCOM,LPELC)
      LPELC=GZPELC()
  999 RETURN
      END
