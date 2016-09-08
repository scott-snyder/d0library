      SUBROUTINE SETREQ(which,what)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : which=-1 true, 0=false, for implementing
C-   requirements
C-
C-   Inputs  : which,what
C-   Outputs : none
C-   Controls: 
C-
C-   Created  19-APR-1993   Drew Baden
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      integer which,what
c
      if (which.eq.0) then
c
c       cancel requirements
c
        nreqs = 0
        nreq2 = 0
        doreq = .false.
      else if (which.eq.-1) then
c
c       enable requirements
c
        doreq = .true.
      else if (which.eq.1) then
c
c       preset stop/continue
c
        reqstop = what
      else if (which.eq.2) then
c
c       preset positive/negative
c
        doreqtf = what
      else if (which.eq.3) then
c
c       preset and/or
c
        doandor = what
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadtreq(op2,ival2,len2,index)
c
c     shove the #bank requirement into here
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      integer op2,ival2(*),len2,index
c
      character*80 string
c
      nreq2 = nreq2 + 1
      rops2(nreq2) = op2
c
c     2nd value (always integer)
c
      call cfchar(0,ival2,string,len2)
      read(string(1:len2),*) rvals2(nreq2)
c
c     and that's all folks
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadreq(tf,ao,stop,index,ibank,ispec,len,offset,
     &  type,op1,ival1,len1)
c
c     shove the right stuff into the common block
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      integer tf,ao,stop,index,ibank,ispec(*),len
      integer offset,type,op1,ival1(*),len1
c
      integer ichar
      character*4 cchar
      character*80 string
c
      equivalence (ichar,cchar)
c
      nreqs = nreqs + 1
      if (tf.eq.dopos) then
        doreqtf = .true.
      else
        doreqtf = .false.
      endif
      if (ao.eq.doand) then
        doandor = .true.
      else
        doandor = .false.
      endif
      if (stop.eq.dostop) then
        reqstop = .true.
      else
        reqstop = .false.
      endif
c
c     index
c
      rpointer(nreqs) = index
c
c     bank name
c
      ichar = ibank
      rbanks(nreqs) = cchar
c
c     special (if any) - put a '.' in first character if not
c
      if (len.eq.0) then
        rspecs(nreqs) = '.'
      else
        call cfchar(0,ispec,string,len)
        rspecs(nreqs) = string(1:len)
      endif
c
c     offset
c
      roffsets(nreqs) = offset
c
c     data type
c
      rdtypes(nreqs) = type
c
c     1st operation
c
      rops1(nreqs) = op1
c
c     decode values from character strings
c     1st value
c
      call cfchar(0,ival1,string,len1)
      if (type.eq.isreal) then
        read(string(1:len1),*) rrvals1(nreqs)
      else if (type.eq.isint) then
        read(string(1:len1),*) rivals1(nreqs)
      else if (type.eq.ishex) then
        read(string(1:len1),'(8x)') rivals1(nreqs)
      else if (type.eq.istf) then
        if (string(1:len1).eq.'T') then
          rbvals1(nreqs) = .true.
        else
          rbvals1(nreqs) = .false.
        endif
      else if (type.eq.ischar) then
        rcvals1(nreqs) = string(1:4)
      endif
c
c     and that's all folks
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function reqmet()
c
c     this guy is the one to determine if the requirement has been met
c     here's how it's done:
c     
c     1. loop over nreq2 - this is a list of overall requirements on the
c        number of banks which meet a certain requirement.  example is that
c        you require et PELC>20 and |eta|<2, and you want at least 1 of
c        these.  so the requirement here is {...}>0
c     2. loop over nreqs - this is the requirement on a definition and find
c        those which have the same index corresponding to loop 1 above.  each
c        of these is a requirement on a word in a bank.  form the requirements
c        on this bank as an AND of all of these which match the index in 1.
c     3. count the number of banks which satisfy #2 and make this number
c        satisfy #1
c
c     i'll be hog tied
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      logical satisfied,thisone,checkoptr,checkopti,checkoptl,checkoptc
      integer i,nummet,index,nbanks,ibank(100),npass,lbank,j
      integer type,offset
      character*4 bank,fchar
      character*12 spec
      logical ltmp
c
c     preset to false
c
      reqmet = .false.
      if (nreq2.lt.1.or.nreqs.lt.1) return
c
c     loop over requests - service each one at a time
c
      do index=1,nreq2
c
c       index now is used to get those definition requirements
c
        do i=1,nreqs
          if (rpointer(i).eq.index) then
c
c           ok, this one belongs. get bank name and if there's any
c           "special" attributes (e.g. jet cone, pnut type, etc.)
c
c           note that we assume ALL banks in this requirement are exactly
c           alike
c
            bank = rbanks(i)
            spec = rspecs(i)
            goto 500
          endif
        enddo
c
c       if we got here something bad happened!
c
        call fwarning(%ref('SERIOUS error in REQMET - tell DREW!!!'))
        return
  500   continue
c
c       gather up list of indices to try (limit to 100 banks!)
c
        call reqblist(bank,spec,nbanks,ibank)
        npass = 0
        if (nbanks.lt.1) goto 600
c
c       how many banks satisfied this requirement?
c
        do i=1,nbanks
          lbank = ibank(i)
c
c         inner loop over requirements
c
          satisfied = .true.
          do j=1,nreqs
            if (rpointer(j).eq.index) then
c
c             ok, this is one we are interested in
c
              type = rdtypes(j)
              offset = roffsets(j)
              if (type.eq.isreal) then
                thisone = checkoptr(
     &            q(lbank+offset),rrvals1(j),rops1(j))
              else if (type.eq.isint) then
                thisone = checkopti(
     &            iq(lbank+offset),rivals1(j),rops1(j))
              else if (type.eq.ishex) then
                thisone = checkopti(
     &            iq(lbank+offset),rivals1(j),rops1(j))
              else if (type.eq.istf) then
                ltmp = iq(lbank+offset)
                thisone = checkoptl(
     &            ltmp,rbvals1(j),rops1(j))
              else if (type.eq.ischar) then
                call uhtoc(iq(lbank+offset),4,fchar,4)
                thisone = checkoptc(
     &            fchar,rcvals1(j),rops1(j))
              endif
c
c             is this satisfied?
c
              if (.not.thisone) satisfied = .false.
            endif
          enddo
c
c         increment if ok
c
          if (satisfied) npass = npass + 1
c
c         next bank
c
        enddo
  600   continue
c
c       how many banks do we require to be satisfied?
c
        rsatis(index) = checkopti(npass,rvals2(index),rops2(index))
      enddo
c
c     take the .or. OR .and. of all requirements in the list 
c
      if (doandor) then
c
c       do .and. of all requirements
c
        satisfied = .true.
        do i=1,nreq2
          if (.not.rsatis(i)) satisfied = .false.
        enddo
      else
c
c       do .or. of all requirements
c
        satisfied = .false.
        do i=1,nreq2
          if (rsatis(i)) satisfied = .true.
        enddo
      endif
c
c     ok, see if we required the requirement to be satisfied or NOT 
c     satisfied
c
      if (doreqtf) then
        if (satisfied) reqmet = .true.
      else
        if (.not.satisfied) reqmet = .true.
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function checkoptr(freal,treal,op)
c
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
c      character*(*) opt
      real freal,treal
      integer op
c
c     preset
c
      checkoptr = .false.
c
c     do it
c
      if (op.eq.islt) then
        checkoptr = freal.lt.treal
      else if (op.eq.isle) then
        checkoptr = freal.le.treal
      else if (op.eq.iseq) then
        checkoptr = freal.eq.treal
      else if (op.eq.isge) then
        checkoptr = freal.ge.treal
      else if (op.eq.isgt) then
        checkoptr = freal.gt.treal
      else if (op.eq.isne) then
        checkoptr = freal.ne.treal
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function checkopti(fint,tint,op)
c
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
c      character*(*) opt
      integer fint, tint
      integer op
c
c     preset
c
      checkopti = .false.
c
c     do it
c
      if (op.eq.islt) then
        checkopti = fint.lt.tint
      else if (op.eq.isle) then
        checkopti = fint.le.tint
      else if (op.eq.iseq) then
        checkopti = fint.eq.tint
      else if (op.eq.isge) then
        checkopti = fint.ge.tint
      else if (op.eq.isgt) then
        checkopti = fint.gt.tint
      else if (op.ne.isgt) then
        checkopti = fint.ne.tint
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function checkoptl(ftf,ttf,op)
c
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
c      character*(*) opt
      logical ftf, ttf
      integer op
c
c     preset
c
      checkoptl = .false.
c
c     do it
c
      if (op.eq.istrue) then
        checkoptl = ftf
      else if (op.eq.isfalse) then
        checkoptl = .not.ftf
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function checkoptc(fchar,tchar,op)
c
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
c      character*(*) opt
      character*(*) fchar, tchar
      integer op
c
c
c     preset
c
      checkoptc = .false.
c
c     do it
c
      if (op.eq.islt) then
        checkoptc = fchar.lt.tchar
      else if (op.eq.isle) then
        checkoptc = fchar.le.tchar
      else if (op.eq.iseq) then
        checkoptc = fchar.eq.tchar
      else if (op.eq.isge) then
        checkoptc = fchar.ge.tchar
      else if (op.eq.isgt) then
        checkoptc = fchar.gt.tchar
      else if (op.eq.isne) then
        checkoptc = fchar.ne.tchar
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer function nummet(bank,spec,offset,type,op,ival)
c
c     determines how many banks pass requirement "i" 
c
      implicit none
c
      character*4 bank
      character*12 spec
      integer offset,type,op,ival
c
      include 'd0$xframe$source:d0map.inc'
c
      logical satisfied,checkoptr,checkoptl,checkoptc,checkopti
c
      integer numv,i,var(100)
      integer f2int,f2hex
      real f2real
      character*4 f2char
      logical f2tf
      integer f1int,f1hex
      real f1real
      character*4 f1char
      logical f1tf
c
      equivalence (f2int,f2hex,f2real,f2char,f2tf)
      equivalence (f1int,f1hex,f1real,f1char,f1tf)
c
c     preset 
c
      f1int = ival
      nummet = 0
c
c     get list of the variables we are going to cut on
c
      call reqvar(bank,spec,offset,type,numv,var)
      if (numv.lt.1) return
c
c     loop and see how many meet requirements
c
      do i=1,numv
        f2int = var(i)
        if (type.eq.isreal) then
          satisfied = checkoptr(f2real,f1real,op) 
        else if (type.eq.isint) then
          satisfied = checkopti(f2int,f1int,op)
        else if (type.eq.ishex) then
          satisfied = checkopti(f2hex,f1hex,op)
        else if (type.eq.istf) then
          satisfied = checkoptl(f2tf,f1tf,op)
        else if (type.eq.ischar) then
          satisfied = checkoptc(f2char,f1char,op)
        endif
        if (satisfied) nummet = nummet + 1
      enddo
c
c     that's all folks
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine reqvar(bank,spec,offset,type,num,var)
c
c     fill list of variables that occur in bank/spec/offset/type
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      character*4 bank,cbnk
      character*12 spec
      integer offset,type,num,var(*)
c
      integer ipnut,lbank,gzproc,lproc,lcaph,gzpnut,lverh,gzverh
      integer gzppho,gzpelc,kbank,gzpmuo,itrak,pelcppho,gzptau
      integer lzfidh,ibnk
      real cone
      logical dojnep,dohmte,generic
c
      integer fint,fhex
      real freal
      character*4 fchar
      logical ftf
c
      equivalence (cbnk,ibnk)
      equivalence (fint,fhex,freal,fchar,ftf)
c
c     which bank?  JETS and PNUT are "special" find if it exists
c
      dojnep = .false.
      dohmte = .false.
      num = 0
      generic = .false.
      if (spec(1:1).eq.'*') then
        generic = .true.
        cbnk = bank
        lbank = lzfidh(dbdiv(pstore),ibnk,0)
      else
        if (bank.eq.'JETS') then
c
c         dig out cone size
c
          read(spec,*) cone
c
c         if cone is < 0, we want to use jnep if appropriate
c
          if (cone.lt.0.0) then
            cone = -cone
            dojnep = .true.
          endif
c
c         find caph 
c
          lproc = gzproc()
          if (lproc.lt.1) go to 200
          lcaph = lq(lproc-4)
          if (lcaph.lt.1) goto 200
          do while (lcaph.gt.0)
            if (cone.lt.0.1) then
c
c             nn algo
c
              if (iq(lcaph+4).eq.3) then
                lbank = lq(lcaph-2)
                goto 300
              endif
            else if (cone.gt.4.5) then
c
c             kt algo
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
c         if it got here, oops
c
          return
  300     continue
        else if (bank.eq.'PNUT') then
c
c         dig out which pnut size
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
c         dig out wether PELC (1) or PPHO (0)
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
c     loop over the banks here, count the number which satisfy the
c     "internal" requirement (requirement per bank)
c
      do while (lbank.gt.0)
        kbank = lbank
c
c       for jnep, must be at least version 2
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
        num = num + 1
        if (type.eq.isreal) then
          freal = q(kbank+offset)
        else if (type.eq.isint) then
          fint = iq(kbank+offset)
        else if (type.eq.ishex) then
          fhex = iq(kbank+offset)
        else if (type.eq.istf) then
          ftf = iq(kbank+offset)
        else if (type.eq.ischar) then
          call uhtoc(iq(kbank+offset),4,fchar,4)
        endif
        var(num) = fint
        lbank = lq(lbank)
c
c       for met requirementes don't loop over PNUT banks
c
        if (bank.eq.'PNUT') lbank = 0
  700   continue
      enddo
c
  200 continue
c
c     that's all folks
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine reqblist(bank,spec,nb,ib)
c
c     collects banks with name "bank" and attribute in c*12 spec
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      character*4 bank,cbnk
      character*12 spec
      integer nb,ib(*)
c
      logical dojnep,dohmte,generic
      real cone
      integer lzfidh,ibnk
      integer ipnut,lbank,gzproc,lproc,lcaph,gzpnut,lverh,gzverh
      integer gzppho,gzpelc,kbank,gzpmuo,itrak,pelcppho,gzptau
c
      equivalence (cbnk,ibnk)
c
      nb = 0
c
c     which bank?  JETS and PNUT are "special" find if it exists
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
c         dig out cone size
c
          read(spec,*) cone
c
c         if cone is < 0, we want to use jnep if appropriate
c
          if (cone.lt.0.0) then
            cone = -cone
            dojnep = .true.
          endif
c
c         find caph 
c
          lproc = gzproc()
          if (lproc.lt.1) return
          lcaph = lq(lproc-4)
          if (lcaph.lt.1) return
          do while (lcaph.gt.0)
            if (iq(lcaph+4).eq.2.and.abs(q(lcaph+6)-cone).lt.0.01) then
              lbank = lq(lcaph-2)
              goto 300
            endif
            lcaph = lq(lcaph)
          enddo
c
c         if it got here, oops
c
          return
  300     continue
        else if (bank.eq.'PNUT') then
c
c         dig out which pnut size
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
c         dig out wether PELC (1) or PPHO (0)
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
c       loop here, collect pointers
c
      do while (lbank.gt.0)
        kbank = lbank
c
c       for jnep, must be at least version 2
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
        nb = nb + 1
        ib(nb) = kbank
        lbank = lq(lbank)
c
c       for met requirementes don't loop over PNUT banks
c
        if (bank.eq.'PNUT') lbank = 0
  700   continue
      enddo
c
  200 continue
      return
      end
