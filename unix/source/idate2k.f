      subroutine idate2k (i, j, k)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Like the IDATE (vax) intrinsic, except that the K (year) argument
C-    is returned as YEAR-1900 instead of YEAR%100.  For this call,
C-    K has a maximum value of 8099.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-MAR-2004   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer i, j, k
      integer tarray(3)
C----------------------------------------------------------------------
      call idate (tarray)
      i = tarray(1)
      j = tarray(2)
      k = tarray(3) - 1900
  999 RETURN
      END
