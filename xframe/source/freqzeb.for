      SUBROUTINE FREQZEB(widget,bank)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : called by d0xquery to fetch a .zeb file
C-
C-   Inputs  : widget,bank
C-   Controls: 
C-
C-   Created   3-MAY-1993   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      integer bank,ierr,widget
c
      integer ichar
      character*4 cchar
      integer maxzeb
      parameter (maxzeb=80)
      integer nlinezeb
      character*84 zebfile(maxzeb)
      character*4 cline4(21*maxzeb)
      integer iline4(21*maxzeb)
c
      equivalence (ichar,cchar)
      equivalence (zebfile(1),cline4(1))
      equivalence (cline4(1),iline4(1))
c
c     do it
c
      ichar = bank
      d0zeblst = 'D0$ZEBLST:'
      d0zeblst_length = 10
C&IF VAXVMS
C&ELSE
C&      call cutol(cchar)
C&      ierr = -1
C&ENDIF
      call getzeblst(cchar,maxzeb,zebfile,nlinezeb,ierr)
      if (ierr.eq.0) then
        call fxmtss(widget,iline4)
      else
        call fwarning(%ref('No D0 reference for this bank'))
      endif
c
      return
      end
