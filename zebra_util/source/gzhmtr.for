      FUNCTION GZHMTR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the address of the currently 
C-   selected HMTR bank.
C-
C-   Returned value  : Bank address
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 14:17:30.58  Rajendran Raja
C-   Updated  24-JAN-1991   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZHMTR
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      GZHMTR= LHMTR
  999 RETURN
      END
