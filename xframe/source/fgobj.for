      SUBROUTINE FGOBJ(threshold,use_ecor,dojnep,nobj,maxobj,baddr,
     &  type,et,eta,phi,mass,e,px,py,pz,pt,theta,emf,rjet,mt,
     &  nvert,zvert,dzvert,irun,iev)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns object info (pelc, ppho, muon, jets)
C-
C-   NOTE:  if path is set to ISAE then we fetch ISAQ and ISAJ (in the future)
C-
C-   Inputs  : none
C-   Outputs : obj info
C-   Controls: 
C-
C-   Created  16-DEC-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      integer use_ecor,dojnep,nobj,maxobj,type(*),baddr(*)
      integer nvert,irun,iev
      real et(*),eta(*),phi(*),mass(*),e(*),px(*),py(*),pz(*),theta(*)
      real emf(*),rjet(*),mt(*),zvert(*),dzvert(*),pt(*),threshold
c
      integer lpoint,gzpelc,gzppho,gzpmuo,gzjets,i,itype,gzpnut,gzverh
      integer icont(10),kvert(14),ivert,ipoint
      real vert(14),etout,corr,excorr,eycorr
      logical primary,secondary,ecorr,data,mc
      character*4 ctype
      real pi,twopi,halfpi,rad
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
      PARAMETER (RAD=0.017453293)
c
      equivalence (kvert,vert)
c
      ecorr = use_ecor.ne.0
      data = use_ecor.eq.2
      mc = use_ecor.eq.1
      nobj = 0
      irun = nrun 
      iev = nevo
c
c     get electrons
c
      lpoint = gzpelc()
      if (lpoint.lt.1) goto 100
      do while (lpoint.gt.0)
        if (q(lpoint+7).lt.threshold) goto 101
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT PELC '')')
     &        iq(lhead+9)
          return
        endif
        et(nobj) = q(lpoint+7)
        eta(nobj) = q(lpoint+9)
        phi(nobj) = q(lpoint+10)
        mass(nobj) = 0.
        e(nobj) = q(lpoint+6)
        px(nobj) = q(lpoint+3)
        py(nobj) = q(lpoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+5)
        theta(nobj) = q(lpoint+8)
        emf(nobj) = q(lpoint+17)/q(lpoint+15)   !em core/core energy
        rjet(nobj) = .01
        mt(nobj) = 0.
        type(nobj) = 0
c
  101   lpoint = lq(lpoint)          ! pointer to next electron
      enddo
  100 continue
c
c     get photons
c
      lpoint = gzppho()
      if (lpoint.lt.1) goto 200
      do while (lpoint.gt.0)
        if (q(lpoint+7).lt.threshold) goto 201
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT PHOTON'')') 
     &        iq(lhead+9)
          return
        endif
        baddr(nobj) = lpoint
        et(nobj) = q(lpoint+7)
        eta(nobj) = q(lpoint+9)
        phi(nobj) = q(lpoint+10)
        mass(nobj) = 0.
        e(nobj) = q(lpoint+6)
        px(nobj) = q(lpoint+3)
        py(nobj) = q(lpoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+5)
        theta(nobj) = q(lpoint+8)
        emf(nobj) = q(lpoint+17)/q(lpoint+15)   !em core/core energy
        rjet(nobj) = .01
        type(nobj) = 1
        mt(nobj) = 0.
c
  201   lpoint = lq(lpoint)          ! pointer to next photon
      enddo
  200 continue
c
c     get muons
c
      lpoint = gzpmuo(0)
      if (lpoint.lt.1) goto 300
      do while (lpoint.gt.0)
        if (q(lpoint+14).lt.threshold) goto 301
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT MUON '')') 
     &        iq(lhead+9)
          return
        endif
        et(nobj) = q(lpoint+14)
        eta(nobj) = q(lpoint+16)
        phi(nobj) = q(lpoint+17)
        mass(nobj) = 0.
        e(nobj) = q(lpoint+13)
        px(nobj) = q(lpoint+10)
        py(nobj) = q(lpoint+11)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+12)
        theta(nobj) = q(lpoint+15)
        emf(nobj) = 0.
        rjet(nobj) = .01
        type(nobj) = 2
        mt(nobj) = 0.
c
  301   lpoint = lq(lpoint)          ! pointer to next muon
      enddo
  300 continue
c
c     get jets - use jcone (0 means nn) - don't use set_caph
c
      call setcaph(lpoint)
      excorr = 0.
      eycorr = 0.
      if (lpoint.lt.1) goto 400
      do while (lpoint.gt.0)
        nobj = nobj + 1
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT JETS '')') 
     &        iq(lhead+9)
          return
        endif
c
c       remove ele/pho (version>1 only)?
c
        ipoint = lpoint
        if (dojnep.gt.0.and.iq(lpoint+1).ne.1) then
          if (lq(lpoint-2).gt.0) ipoint = lq(lpoint-2)
        endif
c
c       apply threshold?
c
        if (q(ipoint+6).lt.threshold) then
          nobj = nobj - 1
          goto 401
        endif
        baddr(nobj) = ipoint
        et(nobj) = q(ipoint+6)
        eta(nobj) = q(ipoint+9)
        phi(nobj) = q(ipoint+8)
        if (ecorr.and.(jcone.gt.0.1)) then
          if (mc) then
            call jet_et_mccorr(eta(nobj),et(nobj),jcone,etout)
            corr = etout/et(nobj)
            excorr = excorr + (et(nobj)-etout)*cos(phi(nobj))
            eycorr = eycorr + (et(nobj)-etout)*sin(phi(nobj))
          endif
          if (data) then
            call warning(
     &        %ref('JET_CAL corrections not yet implemented'))
            corr = 1.0
          endif
        else
          corr = 1.
        endif
        et(nobj) = corr*et(nobj)
        e(nobj) = corr*q(ipoint+5)
        px(nobj) = corr*q(ipoint+2)
        py(nobj) = corr*q(ipoint+3)
        pz(nobj) = corr*q(ipoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        mass(nobj) = sqrt( max(0., e(nobj)**2 
     &    - px(nobj)**2 - py(nobj)**2 - pz(nobj)**2)   )
        theta(nobj) = q(ipoint+7)
        emf(nobj) = q(ipoint+14)
        rjet(nobj) = sqrt( q(ipoint+12)**2 + q(ipoint+13)**2 )
        type(nobj) = 3
        mt(nobj) = sqrt( max(0.,et(nobj)**2 - pt(nobj)**2 ) )
c
  401   lpoint = lq(lpoint)          ! pointer to next jet
      enddo
  400 continue
c
c
c     do neutrinos
c
      lpoint = gzpnut(pnuttype)
      if (lpoint.lt.1) goto 500
      nobj = nobj + 1
      baddr(nobj) = lpoint
      if (nobj.gt.maxobj) then
        write(*,'('' EVENT '',i6,'' MAXED out OBJECT PNUT '')') 
     &    iq(lhead+9)
        return
      endif
      et(nobj) = q(lpoint+7)
      eta(nobj) = q(lpoint+9)
      phi(nobj) = q(lpoint+10)
      mass(nobj) = 0.
      e(nobj) = q(lpoint+6)
      px(nobj) = q(lpoint+3)
      py(nobj) = q(lpoint+4)
      pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
      pz(nobj) = q(lpoint+5)
      theta(nobj) = q(lpoint+8)
      emf(nobj) = 0.
      rjet(nobj) = 0.
      type(nobj) = 4
      mt(nobj) = 0.
c
c     energy scale corrections?
c
      if (ecorr) then
        px(nobj) = px(nobj) + excorr
        py(nobj) = py(nobj) + eycorr
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        et(nobj) = pt(nobj)
      endif
c
  500 continue
c
c     gather verticies
c
      call gtverh(icont)
c
c     primary vertices only please
c
      nvert = 0
      if (icont(2).lt.1) goto 600
c
c     loop over all vertices (but keep only primary ones (for now?))
c
      do ivert=1,icont(2)+icont(3)
        call gtvert(ivert,vert)
        primary = (ibits(kvert(2),31,1).eq.1).or.
     &            (ibits(kvert(2),30,1).eq.1)
        if (primary) then
          nvert = nvert + 1
          zvert(nvert) = vert(5)
          dzvert(nvert) = vert(8)
        endif
      enddo
c
  600 continue
      return
      END
c
      subroutine fgeml(addr,depths,cquans)
c
c     input: ADDR   address of PELC/PPHO bank
c     output: depths(5) fractional energy in each of 5 EM layers (1-4+fh1)
c             cquans(*) various quantities from CLEANEM
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer addr
      real depths(5),cquans(*)
c
      integer icacl,icash,i,ieta_hot(5),iphi_hot(5),nv,status
      logical ok
      real edepth(5),e_hot(5)
c
      call uzero(depths,1,5)
      call uzero(cquans,1,100)
c
c     get list of various quantities (ala CLEANEM)
c
      call emstuff(addr,cquans)
c
      icacl = lq(addr-2)
      if (icacl.lt.1) return
      icash = lq(icacl-2)
      if (icash.lt.1) return
c
c     got index to cash - collect and return
c
      call cemdpth(icash,ieta_hot,iphi_hot,edepth,depths,e_hot)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgemlist(addr,quans,inamquans,nquans,depths)
c
c     input: ADDR   address of PELC/PPHO bank
c     output: depths(5) fractional energy in each of 5 EM layers (1-4+fh1)
c             quans,inamquans,nquans various quantities 
c
c     note: cphys calls this - it is an interface to emdispquans because
c           that routine returns character*10 words per quantity for
c           the title, but i have to pack it into integers which is a
c           pain - note that the packing will be 3 integers per title,
c           which means 12 bytes:  10 for the title, 1 for the '\0',
c           and one for spare
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer addr
      real depths(5),quans(*)
      integer nquans,inamquans(*)
c
      character*10 namquans(100)
      character*4 cname
      integer i,iname,ipt
c
      equivalence (cname,iname)
c
      call emdispquans(addr,quans,namquans,nquans,depths)
c
      if (nquans.lt.1) return
c
c     now unpack the suckers
c
      ipt = 1
      do i=1,nquans
        cname = namquans(i)(1:4)
        inamquans(ipt) = iname
        cname = namquans(i)(5:8)
        inamquans(ipt+1) = iname
        cname = namquans(i)(9:10)
        cname(3:3) = char(0)
        cname(4:4) = char(0)
        inamquans(ipt+2) = iname
        ipt = ipt + 3
      enddo
c
      return
      end
