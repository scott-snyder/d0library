      function TLEN(string)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
c     returns length of string, where length is that part starting at
c     the first character in the string and ending at the first
c     character which is either less than 32 or greater than 126
c
c     note that this works for string which are no greater than 512
c     characters (arbitrary)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      integer tlen
      character*(*) string
c
      integer i,ival
      character*4 cval
c
      equivalence (cval,ival)
c
      tlen = 0
      do i=1,512
        ival = 0
C&IF VAXVMS
        cval(1:1) = string(i:i)
C&ELSE
C&        cval(4:4) = string(i:i)
C&ENDIF
C
C       32 is space, 126 is ~ and that should be the limits
C
        if (ival.lt.33.or.ival.gt.126) return
        tlen = tlen + 1
      enddo
c
      return
      end
