      SUBROUTINE GETZEBd(mode,baddr,BANK,MWV,WRUP,LWV,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets the data
C-
C-   Inputs  : BANK- Character*4 BANK name
C-             MWV - Maximum number words we can accommodate
C-             MODE - 0 for keying on bank name
C-                    1 for address (better be a valid one, BANK will
C-                    be checked against value in common)
C-   Outputs : WRUP- Character array containing LWV lines of text
C-             IERR - not zero implies cannot find file
C-   Controls: None
C-
C-   Created  22-APR-1989   Rajendran Raja
C-   Updated   3-SEP-1991   Herbert Greenlee
C-   Bastardized Aug-1992   me
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 BANK,WRUP(*)
      INTEGER LWV,MWV,LUN,IERR,MODE,BADDR
C
      INCLUDE 'D0$INC:ZEBQ.INC'
      include 'd0$inc:mzca.inc'
      include 'd0$inc:mzcb.inc'
      include 'D0$XFRAME$SOURCE:d0map.inc'
      include 'd0$inc:autof.inc'
c
      integer linear,iok,gtchfr,drpfrm,nchain,kbank,backup
      integer iline4,i,j,nd,ns,nl,num,idat,nr,nlin
      integer points(2000)
      character*4 cline4,name,obank
C
      character*1 ctemp
      character*5 tflag
      character*12 cdat
      integer iauto_fmt,type
      integer inl,inull
      character*4 cnl,cnull
      character*80 msg
c
c     the following number 538976266 is 2020200A hex, which means
c     '   '\nl    or 3 spaces followed by a carriage return.  on the
c     ibm (unix?) that will mean a cr followed by 3 spaces...
c
      data inl/538976266/,inull/0/
c
      equivalence (iline4,cline4)
      equivalence (inl,cnl),(inull,cnull)
c
c     this routine consists of a call to a bastardized
c     version of zb_smg_pn2...
c
      ierr = 0
c
c     get the \nl and \0 (NULL) characters
c     remember - on vax, it's cnl(1:1) and on unix, it's cnl(4:4)
c
ccccc      call getnlnull(inl,inull)
c
c     if we are navigating, go there and reset bank name and address
c
      if (accoff) then
        accoff = .false.
        kbank = lqq(lbank+kqs+ifoff)
c
c       check that we have gone to a legal address
c
        if (kbank.le.0) then
          write(msg,'(''Nothing at offset '',i3)') ifoff
          call xerrmsg(msg)
          ifoff = 0
          call creset(iline4,ifoff)
          ierr = 1
          return
        endif
c
c       can be >0 and still bogus....
c       check if the offset is 0...same bank name
c
        if (ifoff.eq.0) then
          iline4 = iqq(lbank+kqs-4)
          name = cline4
          iline4 = iqq(kbank+kqs-4)
          obank = cline4
          if (name.ne.obank) then
            ierr = 1
            ifoff = 0
            call creset(iline4,ifoff)
            return
          endif
        endif
c
c       now check if the offset is 1, then we have gone UP the
c       bank chain
c
        if (ifoff.eq.1) then
cc          backup = iqq(kbank+kqs
cc          if (backup.ne.lbank) then
cc            write(msg,'(''Nothing at offset '',i3)') ifoff
cc            call xerrmsg(msg)
cc            ierr = 1
cc            ifoff = 0
cc            call creset(iline4,ifoff)
cc            return
cc          endif
        endif
c
c       if we got here, and it's STILL not a legal address,
c       then whoa is to you!!!
c
        lbank = kbank
        iline4 = iqq(lbank+kqs-4)
        name = cline4
        obank = name
        ifoff = 0
        call creset(iline4,ifoff)
        cline4 = name     !creset does "funny" things...
c
c       new bank?
c
        if (bank(1:4).ne.name(1:4)) then
c
c         read in new .zeb file if appropriate - try recursive!
c
cccc          cline4 = name
cccc          if (zstate.eq.1) call fd0util(-1,baddr,iline4)
c
c         calculate chain length and set pointers and report
c
          kbank = lbank
          nchain = 0
          do while (kbank.gt.0)
            nchain = nchain + 1
            points(nchain) = kbank
            kbank = lqq(kbank+kqs)
          enddo
ccc          call csetlin(nchain)
        endif
c
      else if (acclin) then
        acclin = .false.
c
c       check if we have selected linear - if so, service
c
        if (plinear.gt.nchain) then
          plinear = nchain
          write(msg,'(''Selection '',I3,''>'',I3,'' (Max Set)'')')
     &      plinear,nchain
          call xerrmsg(msg)
        endif
        lbank = points(plinear)
c
      else
c
c       get pointer to the bank
c
        if (bank.eq.'HEAD'.or.bank.eq.'STPH') then
          lbank = dbhead(pstore)
        else
          if (mode.eq.0) then
            call blocat(dbdiv(pstore),bank,lbank)
          else if (mode.eq.1) then
            lbank = baddr
          endif
        endif
c
c       calculate length of the chain
c
        kbank = lbank
        nchain = 0
        do while (kbank.gt.0)
          nchain = nchain + 1
          points(nchain) = kbank
          kbank = lqq(kbank+kqs)
        enddo
c
c       report length
c
ccc        call csetlin(nchain)
      endif
c
c     check on bank length
c
      if (lbank.eq.0) then
        call xerrmsg(
     &    'NO Pointer For '//BANK(1:4)//' Bank')
        ierr = 1
        return
      endif
c
c     report address and chain length
c
      call csetadd(lbank,nchain)
      call fbankaddr(lbank)
c
c     get format
c
      bnklen = iqq(lbank+kqs-1)
      istor = dbstore(pstore)
      iok = gtchfr()
c
c     put the data into the character array
c     here, each line is a c*28 length, which means 7 c*4 words
c
      nr = 1
      nd = iqq(lbank+kqs-1)
      ns = iqq(lbank+kqs-2)
      nl = iqq(lbank+kqs-3)
      iline4 = iqq(lbank+kqs-4)
      name = cline4
      num = iqq(lbank+kqs-5)
c
c     fixup (don't ask)
c
      wrup(1) = cnl
      nr = nr + 1
c
c     start with the reference links
c
      if (nl.ne.ns) then
        do i=nl,ns+1,-1
          if (nr.ge.mwv) then
            write(msg,'(
     &        ''BUFFER MAXED OUT - NO MORE DATA READ AFTER '',I5)') i
            call xerrmsg(msg)
            nr = nr - 1
            goto 200
          endif
          idat = lqq(lbank+kqs-i)
          call gtline(wrup,nr,'REF LQ',-i,idat,1)
        enddo
      endif
c
c     now do the structural links
c
      if (ns.gt.0) then
        do i=ns,1,-1
          if (nr.ge.mwv) then
            write(msg,'(
     &        ''BUFFER MAXED OUT - NO MORE DATA READ AFTER '',I5)') i
            call xerrmsg(msg)
            nr = nr - 1
            goto 200
          endif
          idat = lqq(lbank+kqs-i)
          call gtline(wrup,nr,'STR LQ',-i,idat,1)
        enddo
      endif
c
c     now do the regular stuff
c
c     next link
c
      idat = lqq(lbank+kqs)
      call gtline(wrup,nr,'NXL LQ',0,idat,1)
c
c     up link
c
      idat = lqq(lbank+kqs+1)
      call gtline(wrup,nr,'UPL LQ',1,idat,1)
c
c     down link
c
      idat = lqq(lbank+kqs+2)
      call gtline(wrup,nr,'ORG LQ',2,idat,1)
c
c     bank number
c
      idat = iqq(lbank+kqs-5)
      call gtline(wrup,nr,'IDN IQ',-5,idat,1)
c
c     bank name
c
      iline4 = iqq(lbank+kqs-4)
      call gtline(wrup,nr,'IDH IQ',-4,iline4,4)
c
c     nl
c
      idat = iqq(lbank+kqs-3)
      call gtline(wrup,nr,'NL  IQ',-3,idat,1)
c
c     ns
c
      idat = iqq(lbank+kqs-2)
      call gtline(wrup,nr,'NS  IQ',-2,idat,1)
c
c     nd
c
      idat = iqq(lbank+kqs-1)
      call gtline(wrup,nr,'ND  IQ',-1,idat,1)
c
c     status word
c
      idat = iqq(lbank+kqs)
      call gtline(wrup,nr,'STA IQ',0,idat,1)
c
c     now do the data all floats to see if it works...
c
      do i=1,nd
c
c       check - don't read too many now...
c
        if (nr.ge.mwv) then
          write(msg,'(
     &    ''BUFFER MAXED OUT - NO MORE DATA READ AFTER '',I5)') i
          call xerrmsg(msg)
          nr = nr - 1
          goto 200
        endif
        idat = iqq(lbank+kqs+i)
        call gtline(wrup,nr,'DTA IQ',i,idat,iformt)
      enddo
c
c     eof and we are done
c
  200 continue
      wrup(nr) = cnull
      nr = nr + 1
      lwv = nr
c
      return
      end
