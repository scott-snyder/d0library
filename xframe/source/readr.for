      FUNCTION READR(str,tlen)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : translates "str" into "real" - works on unix
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
      real readr
      character*(*) str
c
      integer idot,i,tlen,e
      logical sci
      character*80 string
c
c     look for the exponential notation ('e' or 'E')
c
      sci = .false.
      idot = 0
      do i=1,tlen
        if (str(i:i).eq.'e'.or.str(i:i).eq.'E') then
          sci = .true.
          e = i
        endif
        if (str(i:i).eq.'.') idot = i
      enddo
c
      if (sci) then
        write(string,'(''(f'',i8.8,''.'',i8.8,'')'')') tlen,e-1-idot
        read(str,string) readr
      else
        write(string,'(''(f'',i8.8,''.'',i8.8,'')'')') tlen,idot
        read(str,string) readr
      endif
c
      return
      end
