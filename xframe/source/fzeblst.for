      SUBROUTINE FZEBLST(ichar)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets d0$zeblst
C-
C-   Inputs  : ichar - pointer to c char array
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer ichar
c
      integer len
      character*80 name
c
c     convert "char" to character*
c
      call cfchar(0,ichar,name,len)
      if (len.lt.1) then
        call xerrmsg('D0$ZEBLST not specified!!!')
        return
      endif
c
      d0zeblst(1:len) = name(1:len)
      d0zeblst_length = len
c
      return
      end
