      SUBROUTINE MTC_RSWAP(A,B)
C----------------------------------------------------------------------
C- MTC_RSWAP: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Swap the values of entries A and B
C-
C-   Inputs  : A and B
C-   Outputs : B and A
C-
C-   Created   4-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      real a,b
C- local 
      real temp
C----------------------------------------------------------------------
      temp = b
      b    = a
      a    = temp
C----------------------------------------------------------------------
  999 RETURN
      END
