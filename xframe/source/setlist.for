      SUBROUTINE setLIST(length,istr,dork)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets up in common for bank list dropping
C-
C-   SETLIST sets the list, CLLIST clears it
C-
C-   Inputs  : length=length of c-string,
C-             istr=c string of banks
C-             dork = 0 DROP, 1 KEEP
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JAN-1993   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer length,istr(*),dork
c
      character*200 tlist
      integer i,ilen,n,nl
c
      call cfchar(0,istr,tlist,ilen)
c
      n = 0
      i = (length+2)/5
      nl = 5*i
      do i=1,nl,5
        n = n + 1
        nbdrop = nbdrop + 1
        cbdrop(n) = tlist(i:i+3)
        call str$upcase(cbdrop(n),cbdrop(n))
      enddo
c
      return
c
      entry cllist(length,istr)
c
      nbdrop = 0
c
      return
      end
