      subroutine fxhist(mode,pass)
c
c
c     input:  pass is the result of cuts - if histos are marked for being
c             filled after cuts, then we pay attention to this
c             mode = 0 do histos, otherwise do ntuples
c
c     this is the routine called by fcontrol which fills the histograms
c
c     this file contains subroutine fxhist (which sets up the histogramming
c     of stuff in banks) and alot of utility routines for histogramming
c     where are basically an interface between d0x and hbook
c
c     nhsts > 0 for this to be called
c
c     hist code is similar to REQMET - someday we will merge them into
c     a bunch of functions....but until then....
c
      implicit none
c
      logical pass
      integer mode
c
      include 'd0$xframe$source:d0map.inc'
c
      integer i,ipnut,lverh,lbank,lcaph,itrak,kbank,nlen,num
      integer gzpnut,gzpelc,gzppho,gzverh,gzproc,lproc,gzpmuo,hid
      integer offset,type,lzfidh,ibnk,gzptau
      character*4 bank,cbnk
      character*12 spec
      real cone,xdum
      logical dojnep,dohmte,generic
c
      integer fint,fhex,pelcppho
      real freal
      character*4 fchar
      logical ftf
c
      equivalence (fint,fhex,freal,fchar,ftf)
      equivalence (ibnk,cbnk)
c
      if (mode.eq.0) then
c
c       histograms
c
        num = nhsts
      else
c
c       ntuples
c
        num = ntups
c
      endif
c
c     anything here?
c
      if (num.lt.1) return
c
c     loop over num, looking for non-zero hist ids
c
      do i=1,num
        if (mode.eq.0) then
          hid = hhids(i)
          bank = hbanks(i)
          spec = hspecs(i)
          offset = hoffsets(i)
          type = htypes(i)
        else
          hid = hnids(i)
          bank = hnbanks(i)
          spec = hnspecs(i)
          offset = hnoffsets(i)
          type = hntypes(i)
        endif
        if (hid.gt.0) then
c
c         marked for application after passing cuts? this is screwy but
c         it works - if the 1st digit is a 2, then we pay attention.
c         use logs to dig it out
c
          xdum = log10( float(hid) )
          nlen = xdum
          xdum = xdum - nlen
          if ((xdum.gt.0.1.and.xdum.lt.0.47).and.(.not.pass)) go to 400
c
c         which bank?
c
          dojnep = .false.
          dohmte = .false.
          generic = .false.
          if (spec(1:1).eq.'*') then
            generic = .true.
            cbnk = bank
            lbank = lzfidh(dbdiv(pstore),ibnk,0)
          else
            if (bank.eq.'JETS') then
c
c             dig out cone size
c
              read(spec,*) cone
c
c             if cone is < 0, we want to use jnep if appropriate
c
              if (cone.lt.0.0) then
                cone = -cone
                dojnep = .true.
              endif
c
c             find caph 
c
              lproc = gzproc()
              if (lproc.lt.1) go to 200
              lcaph = lq(lproc-4)
              if (lcaph.lt.1) goto 200
              do while (lcaph.gt.0)
                if (cone.lt.0.1) then
c
c                 nn algo
c
                  if (iq(lcaph+4).eq.3) then
                    lbank = lq(lcaph-2)
                    goto 300
                  endif
                else if (cone.gt.4.5) then
c
c                 kt algo
c
                  if (iq(lcaph+4).eq.5) then
                    lbank = lq(lcaph-2)
                    goto 300
                  endif
                else
                  if (iq(lcaph+4).eq.2.and.
     &                abs(q(lcaph+6)-cone).lt.0.01) then
                    lbank = lq(lcaph-2)
                    goto 300
                  endif
                endif
                lcaph = lq(lcaph)
              enddo
c
c             if it got here, oops
c
              return
300         continue
            else if (bank.eq.'PNUT') then
c
c             dig out which pnut size
c
              read(spec,*) ipnut
              lbank = gzpnut(ipnut)
            else if (bank.eq.'PELC') then
              lbank = gzpelc()
            else if (bank.eq.'PTAU') then
              lbank = gzptau()
            else if (bank.eq.'PMUO') then
              lbank = gzpmuo(0)
            else if (bank.eq.'PPHO') then
              lbank = gzppho()
            else if (bank.eq.'HMTE') then
c
c             dig out wether PELC (1) or PPHO (0)
c
              read(spec,*) pelcppho
              if (pelcppho.eq.0) then
                lbank = gzppho()
              else
                lbank = gzpelc()
              endif
              dohmte = .true.
            else if (bank.eq.'HEAD') then
              lbank = lhead
            else if (bank.eq.'VERT') then
              lverh = gzverh()
              if (iq(lverh+2).lt.1) goto 200   !no primary banks
              lbank = lq(lverh-1)
            endif
          endif
          if (lbank.lt.1) goto 200
c
c         loop over the banks here, histogram the appropriate variable
c
          do while (lbank.gt.0)
            kbank = lbank
c
c           for jnep, must be at least version 2
c
            if (dojnep) then
              if (iq(lbank+1).gt.1.and.lq(lbank-2).gt.0) then
                kbank = lq(lbank-2)
              endif
            endif
            if (dohmte) then
              if (lq(lbank-1).lt.1) goto 700
              kbank = lq(lbank-1)
            endif
            if (type.eq.isreal) then
              freal = q(kbank+offset)
              if (mode.eq.0) then
                call hfill(hid,freal,0.,1.)
              else
                call hfn(hid,freal)
              endif
            else if (type.eq.isint) then
              fint = iq(kbank+offset)
              if (mode.eq.0) then
                call hfill(hid,fint+.5,0.,1.)
              else
                call hfn(hid,fint+.5)
              endif
            else if (type.eq.ishex) then
              fhex = iq(kbank+offset)
              if (mode.eq.0) then
                call hfill(hid,fhex+.5,0.,1.)
              else
                call hfn(hid,fhex+.5)
              endif
            else if (type.eq.istf) then
              ftf = iq(kbank+offset)
              if (mode.eq.0) then
                call hfill(hid,fint+.5,0.,1.)
              else
                call hfn(hid,fint+.5)
              endif
            endif
c
            if (generic) then
              lbank = lzfidh(dbdiv(pstore),ibnk,lbank)
            else
              lbank = lq(lbank)
            endif
c
c           for met requirementes don't loop over PNUT banks
c
            if (bank.eq.'PNUT') lbank = 0
  700       continue
          enddo
c
  200     continue
c
        endif
  400   continue
      enddo
c
c     that's all folks
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE CHBOOK1(ID,ITIT,LEN,NBINS,FBIN,LBIN,STORE)
C
C     you can call this from C,and it'll book a histogram - note that
C     the "title" is INTEGER ITIT(*) and you need to tell it how many
C     bytes (use strlen for this)
C
      IMPLICIT NONE
C
      INTEGER ID,ITIT(*),LEN,NBINS
      REAL FBIN,LBIN,STORE
C
      CHARACTER*80 TITLE
C
      call cfchar(0,itit,title,len)
C
      call hbook1(id,title(1:len),nbins,fbin,lbin,store)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE CHFILLN(ID,IDN,ITIT,LEN,NBINS,FBIN,LBIN,STORE)
C
C     you can call this from C,and it'll book a histogram and fill the
C     thing from the ntuple created - note that
C     the "title" is INTEGER ITIT(*) and you need to tell it how many
C     bytes (use strlen for this)
C
      IMPLICIT NONE
C
      INTEGER ID,ITIT(*),LEN,NBINS,IDN
      REAL FBIN,LBIN,STORE
C
      CHARACTER*80 TITLE
C
      call cfchar(0,itit,title,len)
C
      call hbook1(id,title(1:len),nbins,fbin,lbin,store)
c
      call hproj1(id,idn,0,0,1,1000000,1)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE CHBOOKN(ID,ITIT,LEN)
C
C     you can call this from C,and it'll book an ntuple (new form) - note that
C     the "title" is INTEGER ITIT(*) and you need to tell it how many
C     bytes (use strlen for this)
C
      IMPLICIT NONE
C
      INTEGER ID,ITIT(*),LEN
C
      CHARACTER*80 TITLE
      integer ierr
      character*10 tag(1)
C
      call cfchar(0,itit,title,len)
C
      tag(1) = title
      call hbookn(id,'D0X TUPLE OF '//TITLE(1:LEN),1,' ',8192,TAG)
ccccc      call hbnt(id,'M',title)
c
c     reserve 8192 words for this tuple
c
ccccc      call hbset('BSIZE',8192,ierr)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE CHGIVE(ID,ITIT,LEN,NBINS,FBIN,LBIN,BINMIN,BINMAX,
     &  SUM,MEAN,SIGMA,VALUES,ERRORS,NENTRIES)
C
C     same as above, only it goes the other way
C
      IMPLICIT NONE
C
      INTEGER ID,ITIT(*),LEN,NBINS,NENTRIES
      REAL FBIN,LBIN,BINMIN,BINMAX,SUM,MEAN,SIGMA,VALUES(*),ERRORS(*)
C
      INTEGER NX,NY,NWT,LOC,I
      REAL XMI,XMA,YMI,YMA,HMAX,HMIN,HSUM,HSTATI,HIE
      CHARACTER*80 TITLE
C
C     get limits, title, etc.
C
      CALL HGIVE(ID,TITLE,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
C
C     if nx and ny are both zero, it's probably a mistake?
C
      if (nx.lt.1) return
      NBINS = NX
      FBIN = XMI
      LBIN = XMA
      call cfchar(1,itit,title,len)
C
C     get max, sum, and netries
C
      BINMAX = HMAX(ID)
      BINMIN = HMIN(ID)
      SUM = HSUM(ID)
      MEAN = HSTATI(ID,1,'HIST',0)
      SIGMA = HSTATI(ID,2,'HIST',0)
      CALL HNOENT(ID,NENTRIES)
C
C     get contents
C
      CALL HUNPAK(ID,VALUES,'HIST',0)
      DO I=1,NX
        ERRORS(I) = HIE(ID,I)
      ENDDO
C
C     if it's a 2-d histo, forget it (for now)
C
      if (ny.gt.0) nx = -1
C
C     that's all folks      
C
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chcanh(hid)
c
c     cancel filling this histogram
c
      implicit none
c
      integer hid
c
      include 'd0$xframe$source:d0map.inc'
c
      integer i
c
      do i=1,nhsts
        if (hhids(i).eq.hid) then
          hhids(i) = 0
          return
        endif
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chcann(hid)
c
c     cancel filling this tuple
c
      implicit none
c
      integer hid
c
      include 'd0$xframe$source:d0map.inc'
c
      integer i
c
      do i=1,ntups
        if (hnids(i).eq.hid) then
          hnids(i) = 0
          return
        endif
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chsetn(hid,ibank,ispec,len,offset,type)
c
c     tell d0x to fill this tuple
c
      implicit none
c
      integer hid,ibank,ispec(*),offset,type,len
c
      include 'd0$xframe$source:d0map.inc'
c
      integer ichar
      character*4 cchar
      character*80 string
c
      equivalence (ichar,cchar)
c
      ntups = ntups + 1
      hnids(ntups) = hid
      call cfchar(0,ispec,string,len)
      if (len.eq.0) then
        hnspecs(ntups) = '.'
      else
        hnspecs(ntups) = string(1:len)
      endif
      hnoffsets(ntups) = offset
      hntypes(ntups) = type
      ichar = ibank
      hnbanks(ntups) = cchar
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chseth(hid,ibank,ispec,len,offset,type)
c
c     tell d0x to fill this tuple
c
      implicit none
c
      integer hid,ibank,ispec(*),offset,type,len
c
      include 'd0$xframe$source:d0map.inc'
c
      integer ichar
      character*4 cchar
      character*80 string
c
      equivalence (ichar,cchar)
c
      nhsts = nhsts + 1
      hhids(nhsts) = hid
      call cfchar(0,ispec,string,len)
      if (len.eq.0) then
        hspecs(nhsts) = '.'
      else
        hspecs(nhsts) = string(1:len)
      endif
      hoffsets(nhsts) = offset
      htypes(nhsts) = type
      ichar = ibank
      hbanks(nhsts) = cchar
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chout(ifile,len)
c
c     writes all histograms to output file ifile
c
      implicit none
c
      integer ifile(*),len
c
      character*80 file
c
      if (len.lt.1) then
        call fwarning(%ref('Output filename confusing - try again'))
        return
      endif
c
      call cfchar(0,ifile,file,len)
      call hrput(0,file(1:len),'T')
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chreset(id)
c
c     reset the histogram id 
c
      implicit none
c
      integer id
      call hreset(id,' ')
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chfithn(id,title,len,npar,xpar,sigpar,chi2)
c
c     fit histogram ala hfithn
c
      implicit none
c
      integer id,title(*),len,npar
      real xpar(*),sigpar(*),chi2
c
      character*80 string
      real dum
c
      call cfchar(0,title,string,len)
      call hfithn(id,string(1:len),'NQ',npar,
     &  xpar,dum,dum,dum,sigpar,chi2)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
