      SUBROUTINE FGOBJMC(nobj,maxobj,isaqj,jetid,
     &  type,et,eta,phi,mass,e,px,py,pz,pt,theta,
     &  nvert,zvert,threshold)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns object info from ISAJET (ISAJ/Q/L)
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
      integer nobj,maxobj,type(*),nvert,irun,iev,jetid(*),isaqj(*)
      real et(*),eta(*),phi(*),mass(*),e(*),px(*),py(*),pz(*),theta(*)
      real zvert(*),pt(*),threshold
      character*8 label,name
      character*4 str
      integer istr
c
      integer lisae,lisaj,lpoint,gzisae,lisaq,gzisv1,lisal
      real pi,twopi,halfpi,rad,cut
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
      PARAMETER (RAD=0.017453293)
c
      equivalence (str,istr)
c
      nobj = 0
      nvert = 0
c
c     anything there?
c
      lisae = gzisae()
      if (lisae.lt.1) return
c
c     get vertex
c
      lpoint = gzisv1()
      do while (lpoint.gt.0)
          if (q(lpoint+6).eq.1800.) goto 100
          lpoint = iq(lpoint)
      enddo
  100 continue
      if ( lpoint.gt.0 ) then
        nvert = 1
        zvert(1) = q(lpoint+9)
      endif
c
c     get ISAJ stuff
c
      lisaj = lq(lisae-1)
      do while (lisaj.gt.0)
        name = label(iq(lisaj+1))
        str(1:4) = name(1:4)
        cut = sqrt( q(lisaj+2)**2 + q(lisaj+3)**2 + q(lisaj+6)**2 )
        if (cut.gt.threshold) then
          nobj = nobj + 1
          isaqj(nobj) = 1
          type(nobj) = istr
          px(nobj) = q(lisaj+2)
          py(nobj) = q(lisaj+3)
          pz(nobj) = q(lisaj+4)
          mass(nobj) = q(lisaj+6)
          phi(nobj) = q(lisaj+7)
          theta(nobj) = q(lisaj+8)
          eta(nobj) = q(lisaj+9)
          pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
          et(nobj) = sqrt( pt(nobj)*pt(nobj) + mass(nobj)*mass(nobj) )
          e(nobj) = sqrt( mass(nobj)*mass(nobj) + q(lisaj+5)*q(lisaj+5))
          jetid(nobj) = iq(lisaj-5)
        endif
        lisaj = lq(lisaj)
      enddo
c
c     get ISAQ stuff
c
      lisaq = lq(lisae-2)
      do while (lisaq.gt.0)
        name = label(iq(lisaq+1))
        str(1:4) = name(1:4)
        cut = sqrt( q(lisaq+2)**2 + q(lisaq+3)**2 + q(lisaq+6)**2 )
        if (cut.gt.threshold) then
          nobj = nobj + 1
          isaqj(nobj) = 2
          type(nobj) = istr
          px(nobj) = q(lisaq+2)
          py(nobj) = q(lisaq+3)
          pz(nobj) = q(lisaq+4)
          mass(nobj) = q(lisaq+6)
          phi(nobj) = q(lisaq+7)
          theta(nobj) = q(lisaq+8)
          eta(nobj) = q(lisaq+9)
          pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
          et(nobj) = sqrt( pt(nobj)*pt(nobj) + mass(nobj)*mass(nobj) )
          e(nobj) = q(lisaq+5)
          jetid(nobj) = lq(lisaq-1)
          if ( jetid(nobj).ne.0 ) jetid(nobj) = iq(jetid(nobj)-5)
        endif
        lisaq = lq(lisaq)
      enddo
c
c     get ISAL stuff
c
      lisal = lq(lisae-7)
      do while (lisal.gt.0)
        name = label(iq(lisal+1))
        str(1:4) = name(1:4)
        cut = sqrt( q(lisal+2)**2 + q(lisal+3)**2 + q(lisal+6)**2 )
        if (cut.gt.threshold) then
          nobj = nobj + 1
          isaqj(nobj) = 3
          type(nobj) = istr
          px(nobj) = q(lisal+2)
          py(nobj) = q(lisal+3)
          pz(nobj) = q(lisal+4)
          mass(nobj) = q(lisal+6)
          phi(nobj) = q(lisal+7)
          theta(nobj) = q(lisal+8)
          eta(nobj) = q(lisal+9)
          pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
          et(nobj) = sqrt( pt(nobj)*pt(nobj) + mass(nobj)*mass(nobj) )
          e(nobj) = sqrt( mass(nobj)*mass(nobj) + q(lisal+5)*q(lisal+5))
          jetid(nobj) = lq(lisal-1)
          if ( jetid(nobj).ne.0 ) jetid(nobj) = iq(jetid(nobj)-5)
        endif
        lisal = lq(lisal)
      enddo
c
      return
      END
