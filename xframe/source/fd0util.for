      SUBROUTINE FD0UTIL(tag,baddr,temp)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    tag = -3, NAVIGATE address AND bank name (data)
c           -2,    "     TREE widget
c           -1,    "     .ZEB file
c            0, XDBANK   bank name only (data)
c            1, DZSURV at address
c            2, PRBANK
c            3, EZBANK
c            4, DZFORM
c            5, DBANK
c            6, DADDR
c            7, SET_CAPH
C-
C-   Inputs  : tag - from widget
C-             temp - integer/char pointer
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer tag,temp,baddr
c
      character*50 algo,which_algo(3)
      character*80 msg
      integer maxzeb
      parameter (maxzeb=80)
      integer nlinezeb
      character*84 zebfile(maxzeb)
      character*4 cline4(21*maxzeb)
      character*4 cbank
      character*136 outchr(200)
      integer maxline
      parameter (maxline = 400)
      integer jline4(34*maxline)
      character*4 jcline4(24*maxline)
      integer iline4(21*maxzeb)
      integer len,ibank,tlen,ier,pointer
      integer nl,null,type,nline,vers,which,which_cone
      real cone(3),template(3)
c
      data cone/.3,.5,.7/,which_algo/'ELECTRON','CONE_JET','NN_JET'/
c
      equivalence (ibank,cbank)
      equivalence (zebfile(1),cline4(1))
      equivalence (cline4(1),iline4(1))
      equivalence (jcline4(1),jline4(1))
      equivalence (outchr(1),jcline4(1))
c
      if (tag.le.0) then
c
c       xdbank
c
        ibank = temp
        len = tlen(cbank)
        if (len.eq.0) then
          cbank = 'HEAD'
          len = 4
        endif
        if (len.ne.4) then
          call xerrmsg('4 Letters ONLY for "BANK"')
          return
        endif
        call str$upcase(cbank,cbank)
c
c
        if (tag.eq.-3) then
c
c         get the data and stuff it into the appropriate text widget
c
          call getzebd(1,baddr,cbank,21*maxzeb,cline4,nlinezeb,ier)
          type = 1
          if (ier.eq.0) then
            call cstuffzeb(iline4,type)
ccccccc            call cstufflist(iline4)
          endif
c
        else if (tag.eq.-2) then
c
c         put the "dzsurv" into window
c
cc          call dzsurvme(outchr,nline,
cc     &      'Bank chain '//dbname(pstore),
cc     &      dbstore(pstore),dbhead(pstore))
cc          type = 2
cc          call cstuffzeb(jline4,type)
c
c         make tree widget
c
          call fzwidget(dbhead(pstore))
c
        else if (tag.eq.-1) then
c
c         put the .zeb file into the window
c
          call getzeblst(cbank,maxzeb,zebfile,nlinezeb,ier)
          type = 0
          if (ier.eq.0) then
            call cstuffzeb(iline4,type)
          else
            call xerrmsg('.ZEB file not found')
          endif
c
        else if (tag.eq.0) then
c
c         get the data and stuff it into the appropriate text widget
c
          call getzebd(0,0,cbank,21*maxzeb,cline4,nlinezeb,ier)
          type = 1
          if (ier.eq.0) then
            call cstuffzeb(iline4,type)
ccccccc            call cstufflist(iline4)
          endif
c
        endif
c
      else if (tag.eq.1) then
c
c       xdaddr (equivalent)
c
c       print format
c
        call dzform(dbstore(pstore),temp)
c
c       print bank
c
        write(msg,'(''Dump of bank at address '',i9)') temp
        call dzshow(msg,dbstore(pstore),temp,'BLD',0,0,0,0)
c
      else if (tag.eq.2) then
c
c       call prbank
c
        ibank = temp
        len = tlen(cbank)
        if (len.eq.1.and.cbank(1:1).eq.'?') then
          len = 4
          cbank(1:4) = 'XXXX'
        endif
        if (len.ne.4) then
          call xerrmsg('4 Letters ONLY for "BANK"')
          return
        endif
        call str$upcase(cbank,cbank)
        call prbank(cbank,out_lun)
c
      else if (tag.eq.3) then
c
c       call ezbank
c
        call ezbank
c
      else if (tag.eq.4) then
c
c       check MZFORM via DZFORM
c
        ibank = temp
        len = tlen(cbank)
        if (len.ne.4) then
          call xerrmsg('4 Letters ONLY for "BANK"')
          return
        endif
        call str$upcase(cbank,cbank)
        call blocat(dbdiv(pstore),cbank,pointer)
        call dzform(dbstore(pstore),pointer)
c
      else if (tag.eq.5) then
c
c       dbank (on vax only for old farts)
c
C&IF VAXVMS
        call dbank
C&ENDIF
      else if (tag.eq.6) then
c
c       dbaddr (on vax only for old farts)
c
C&IF VAXVMS
        call daddr
C&ENDIF
c
      else if (tag.eq.7) then
        call show_caph(algo,vers,ier)
        write(*,'('' Current CAPH path is:'',/,
     &    '' Algorithm: '',a50,/,'' Version: '',i4)') algo,vers
  300   continue
        write(*,'('' Pick one of the following algorithms: '',/,
     &    '' 1    ELECTRON'',/,
     &    '' 2    CONE_JET'',/,
     &    '' 3    NN_JET'')')
        call getpar(1,'Which one (1-3)? ','I',which)
        if (which.lt.1.or.which.gt.3) goto 300
        if (which.eq.2) then
  301     continue
          write(*,'('' Pick one of the following cone sizes: '',/,
     &      '' 1    .3'',/,
     &      '' 2    .5'',/,
     &      '' 3    .7'')')
          call getpar(1,'Which one (1-3)? ','I',which_cone)
          if (which_cone.lt.1.or.which_cone.gt.3) goto 301
          template(1) = 1.
          template(2) = 6.
          template(3) = cone(which_cone)
        else
          template(1) = 0.0
        endif
c
c       set caph and read back to be sure
c
        call set_caph(which_algo(which),template,ier)
        call show_caph(algo,vers,ier)
        write(*,'('' Current CAPH path is:'',/,
     &    '' Algorithm: '',a50,/,'' Version: '',i4)') algo,vers
c
      endif
c
      return
      end
