      SUBROUTINE HIGGS_INIT
      implicit none
      include 'D0$SPYTHIA$INC:LUDAT1.INC'
      include 'D0$SPYTHIA$INC:PYPARS.INC'
      include 'D0$SPYTHIA$INC:PARSUSY.INC'
      real tanb,al,be,cosa,cosb,sina,sinb,xw
C.....Set up the higgs couplings....
      XW=paru(102)
      tanb=psusy(11)
      AL=psusy(50)
      BE=ATAN(tanb)
      sina=sin(AL)
      cosa=cos(AL)
      sinb=SIN(BE)
      cosb=cos(BE)
C.....First, h
C.....coupling to d-type quarks
      paru(161)=-sina/cosb
C.....coupling to u-type quarks
      paru(162)=cosa/sinb
C.....coupling to leptons
      paru(163)=paru(161)
C.....coupling to Z0....sin(b-a)
      paru(164)=sin(BE-AL)
C.....coupling to W
      paru(165)=paru(164)
C.....gamma-gamma
      paru(168)=sin(BE-AL)+COS(2.*BE)*SIN(BE+AL)/2./(1.-XW)
C.....Secondly, H
      paru(171)=cosa/cosb
      paru(172)=sina/sinb
      paru(173)=paru(171)
      paru(174)=cos(BE-AL)
      paru(175)=paru(174)
      paru(176)=cos(2.*AL)*cos(BE+AL)-2.*SIN(2.*AL)*sin(BE+AL)
      paru(177)=cos(2.*BE)*cos(BE+AL)
      paru(178)=cos(BE-AL)-cos(2.*BE)*cos(BE+AL)/2./(1.-XW)
C.....Thirdly, A
      paru(181)=tanb
      paru(182)=1./paru(181)
      paru(183)=paru(181)
      paru(184)=0.0
      paru(185)=0.0
      paru(186)=cos(BE-AL)
      paru(187)=sin(BE-AL)
      paru(188)=0.0
      paru(189)=0.0
      paru(190)=0.0
C.....Finally, H+
C.....coupling to W h
      paru(195)=cos(BE-AL)
C.....Users sets all couplings for Higgses
      MSTP(4)=1
C.....Fix the running of the coupling for higgs
      MSTP(37)=1
      PARP(37)=1.0
C
      RETURN
      END
