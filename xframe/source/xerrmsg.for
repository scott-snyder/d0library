      SUBROUTINE XERRMSG(string)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : pops up a box with error message
C-
C-   Inputs  : string - limit to 80*4 characters please
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-AUG-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      character*(*) string
      character*512 string2
c
      integer tlen,trulen
      character*1 nl,null
c
c     get null ('\0')
c
      call getnlnull(nl,null)
c
c     get "trulen" of string
c
      tlen = trulen(string)
c
c     put the c NULL at the end
c
      string2(1:tlen) = string(1:tlen)
      string2(tlen+1:tlen+1) = nl
c
c     and pass it on
c
      call cxerrmsg(%ref(string2))
c
      return
      end
