      SUBROUTINE transmich_msbar_pass(Isquark_mass, Igluino_mass)
c     Sample program to translate from the UofM solutions to ISASUSY-
c     compatible inputs.    C.Kolda  6/6/94
C
C     Modified R. J. Genik II 7-JAN-1995 use as subroutine to translate
C     gluino as squark mass for xsect calculation. Accesses the ntuple
C     common blocks for the current point (note I needed to change this
C     from D0$SPYTHIA$INC:VARS.INC used during generation) passed values are returned
C
C
C
C-   Updated   3-NOV-1995   R. J. Genik II  Modified from transmich_msbar
C-   to  use the umich common blocks.
c
      IMPLICIT NONE
c
      include 'D0$SPYTHIA$INC:VARS.INC'
      include 'D0$SPYTHIA$INC:MASSMAT.INC'
      include 'D0$SPYTHIA$INC:TUNING.INC'
      INTEGER Isquark_mass, Igluino_mass
      REAL*8 alpha3
      REAL XMG,XMS,XMTL,XMTR,XMLL,XMLR,XMNL,XTANB,XMHA,XMU,XMT,
     &     XAT,XMBL,XMBR
      INTEGER i,dnum,eof
      REAL inv_SSPOLE,c2beta
C
C ****  init
C
      Isquark_mass = 0
      Igluino_mass = 0
c
c     Set a few constants to correspond to ISASUSY.
c
      sw2=0.23
      alpha3=0.120
      mz=91.17
c     First do simple translations
c
      XMS = (msq(1)+msq(2)+msq(7)+msq(8))/4.! avg squark mass
      XMT = mq(5)                              ! top mass (pole?)
c
      c2beta=cos(2.*atan(tbeta))
c
c     I include the F&D-term contributions in the LH and RH stop and
c     sbottom masses, so subtract them out to avoid double-counting.
c
      XMBL = sqrt(msq(6)**2 + Mz**2*c2beta*(-sw2/3.+0.5))
      XMBR = sqrt(msq(12)**2 + Mz**2*c2beta*sw2/3.)
c
c     Finally, I output the MSbar (actually DRbar, though it matters little)
c     value for the gluino mass, while ISASUSY expects the pole mass. Lift
c     pole mass routines from ISASUSY and invert them so ISASUSY will get back
c     what we started with when it calculates MSbar gluino mass.
c
      XMG = inv_SSPOLE(mgf(3),mz**2,-alpha(3),xmt,xms,mstop(1),
     &        mstop(2),xmbl,xmbr)
c
c     That's all the definitions, return the integer masses
      Isquark_mass = INT(XMS+0.5)      ! no correction, just rounding
      Igluino_mass = ABS(INT(XMG+0.5)) ! - sign is just a phase factor for
                                       ! fermions and it screws up the
                                       ! cross-section calc.
      RETURN
      END
