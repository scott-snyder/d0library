      FUNCTION READI(str,tlen)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : translates "str" into "integer" - works on unix
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      integer readi
      character*(*) str
c
      integer tlen,idot,i
      character*80 string
      real readr,xval
c
      xval = readr(str,tlen)
      readi = xval
c
      return
      end
