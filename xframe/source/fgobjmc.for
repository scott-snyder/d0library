      SUBROUTINE FGOBJMC(nobj,maxobj,jetid,
     &  type,et,eta,phi,mass,e,px,py,pz,pt,theta,
     &  nvert,zvert)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns object info from ISAJET (ISAJ/Q)
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
      integer nobj,maxobj,type(*),nvert,irun,iev,jetid(*)
      real et(*),eta(*),phi(*),mass(*),e(*),px(*),py(*),pz(*),theta(*)
      real zvert(*),pt(*)
      character*8 label,name
      character*4 str
      integer istr
c
      integer lisae,lisaj,lpoint,gzisae,lisaq,gzisv1
      real pi,twopi,halfpi,rad
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
        nobj = nobj + 1
        type(nobj) = istr
        px(nobj) = q(lisaj+2)
        py(nobj) = q(lisaj+3)
        pz(nobj) = q(lisaj+3)
        mass(nobj) = q(lisaj+6)
        phi(nobj) = q(lisaj+7)
        theta(nobj) = q(lisaj+8)
        eta(nobj) = q(lisaj+9)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        et(nobj) = sqrt( pt(nobj)*pt(nobj) + mass(nobj)*mass(nobj) )
        e(nobj) = sqrt( mass(nobj)*mass(nobj) + q(lisaj+5)*q(lisaj+5) )
        jetid(nobj) = iq(lisaj-5)
        lisaj = lq(lisaj)
      enddo
c
c     get isaq stuff
c
      lisaq = lq(lisae-2)
      do while (lisaq.gt.0)
        name = label(iq(lisaq+1))
        str(1:4) = name(1:4)
        nobj = nobj + 1
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
        e(nobj) = sqrt( mass(nobj)*mass(nobj) + q(lisaq+5)*q(lisaq+5) )
        jetid(nobj) = lq(lisaq-1)
        if ( jetid(nobj).ne.0 ) jetid(nobj) = iq(jetid(nobj)-5)
        lisaq = lq(lisaq)
      enddo
c
      return
      END
