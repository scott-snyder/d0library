      SUBROUTINE MTC_ISWAP(A,B)
C----------------------------------------------------------------------
C- MTC_ISWAP: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Swap the values of integer entries A and B
C-
C-   Inputs  : A and B
C-   Outputs : B and A
C-
C-   Created   4-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      integer a,b
C- local 
      integer temp
C----------------------------------------------------------------------
      temp = b
      b    = a
      a    = temp
C----------------------------------------------------------------------
  999 RETURN
      END
