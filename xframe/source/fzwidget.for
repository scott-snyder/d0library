      SUBROUTINE FZWIDGET(head)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given the bank head, this routine builds a
C-   motif "tree" widget
C-
C-   Inputs  : head - pointer to "head" bank
C-   Outputs :
C-   Controls:
C-
C-   Created  28-OCT-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
      INCLUDE 'D0$INC:ZEBQ.INC'
      include 'd0$inc:mzca.inc'
      include 'd0$inc:mzcb.inc'
c
      integer head
c
      integer i4,widget
      character*4 c4
c
      equivalence (i4,c4)
c
      if (head.lt.1) then
        call xerrmsg('Illegal pointer to HEAD bank')
        return
      endif
c
c     bank exists - make tree
c
      i4 = iqq(head+kqs-4)
c
c     bank exists...make tree and do down the list....
c
      widget = 0
      call fbranch(%val(0),dbdiv(pstore),%val(0),iqq(kqs),lqq(kqs),head)
c
      return
      end
