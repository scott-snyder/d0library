      SUBROUTINE FZEBRA(tag,path)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c     tag = 0, dump
c           1, verify
c           2, obsolete
c           3, set/show path
C-
C-   Inputs  : tag  - from the widget
C-             path - character*4
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer tag,path(*)
c
      character*80 msg
      character*4 cpath
      integer len,ipath,tlen
c
      equivalence (ipath,cpath)
c
      if (tag.eq.0) then
c
c       dump via dzsurv
c
        if (dbhead(pstore).eq.0) then
          call xerrmsg('Nothing read into '//dbname(pstore)//' yet?')
          return
        else
          call dzsurv('Bank chain '//dbname(pstore)//
     &      ' courtesy of DZSURV:',dbstore(pstore),dbhead(pstore))
        endif
c
      else if (tag.eq.1) then
c
c       verify
c
        if (dbhead(pstore).eq.0) then
          call xerrmsg('Nothing read into '//dbname(pstore)//' yet?')
          return
        else
          call DZVERI(
     &    'Zebra verification '//dbname(pstore)//' courtesy of DZVERI',
     &    dbstore(pstore),'LSU')
          call dzstor(
     &    'Store parameters '//dbname(pstore)//' courtesy DZSTOR',
     &    dbstore(pstore))
        endif
      else if (tag.eq.2) then
c
c       obsolete
c
        call xerrmsg('Tag 2 OBSOLETE!!!')
      else if (tag.eq.3) then
c
c       set/show path
c
        ipath = path(1)
        len = tlen(cpath)
        if (cpath(1:1).eq.'?'.or.len.ne.4) then
          call pathgt(cpath)
          path(1) = ipath
        else
          call str$upcase(cpath,cpath)
          call pathst(cpath)
          call pathgt(cpath)
        endif
      endif
c
      return
      end
