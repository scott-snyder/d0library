      FUNCTION READH(str,tlen)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : translates "str" into "hex integer" - works on unix
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
      integer readh,tlen
      character*(*) str
c
      character*80 string
      integer i
c
      write(string,'(''(z'',i8.8,'')'')') tlen
      read(str,string) readh
c
      return
      end
