      subroutine decay_init
      implicit none
      include 'D0$SPYTHIA$INC:LUDAT1.INC'
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:LUDAT3.INC'
      include 'D0$SPYTHIA$INC:LUDAT4.INC'
      include 'D0$SPYTHIA$INC:PYPARS.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
      include 'D0$SPYTHIA$INC:PARSUSY.INC'
      include 'D0$SPYTHIA$INC:SPYTHIA.INC'
      logical DEBUG
      data DEBUG/.false./
C.....
      integer NQLD,NQRD,NSTR,SKC
      parameter(NSTR=1301,NQLD=50,NQRD=50)
      real tanb,al,be,cosa,cosb,sina,sinb,xw
      integer I,J,J1,J2,I1,I2
      integer KC,LKNT,IDLAM(100,3),IDLAM0(100,3),LKNT0
      real XLAM(0:100),XLAM0(0:100),XALL
      REAL*8 ATERM,TAN2T,THETA
      REAL*8 R(2,2),B2(2,2),C2(2,2),M2(2,2)
      real br1e,br2e,br1s,br2s,br1st,br2st,br1w,br2z,br2h
      integer pr1,pr2,pr3
      integer AKF1,AKF2,AKF3
      Integer j3
      real delm

C.....Set top mass and bottom mass
      PMAS(5,1)=psusy(10)
      PMAS(6,1)=psusy(9)
C.....Read in sparticle masses
      J=20
      DO I=41,49,4
       J=J+1
       PMAS(I+2,1)=psusy(J)
       J=J+1
       PMAS(I,1)=psusy(J)
      ENDDO
      J=26
      DO I=42,50,4
       J=J+1
       PMAS(I+2,1)=psusy(J)
       J=J+1
       PMAS(I,1)=psusy(J)
      ENDDO
      J=32
      DO I=53,61,4
       J=J+1
       PMAS(I+2,1)=psusy(j)
       J=J+1
       PMAS(I,1)=psusy(j)
      ENDDO
      J=38
      DO I=54,62,4
       J=J+1
       PMAS(I+2,1)=psusy(j)
       J=J+1
       pmas(I,1)=psusy(j)
      ENDDO
C.....21 April 95
C.....Make right-handed neutrinos very heavy
      PMAS(56,1)=1.E6
      PMAS(60,1)=1.E6
      PMAS(64,1)=1.E6
C.....
      PMAS(25,1)=psusy(45)
      pmas(72,1)=pmas(25,1)
      pmas(35,1)=psusy(46)
      pmas(73,1)=pmas(35,1)
      pmas(36,1)=psusy(47)
      pmas(74,1)=pmas(36,1)
      pmas(37,1)=psusy(48)
      pmas(75,1)=pmas(37,1)
      pmas(65,1)=psusy(53)
      DO I=66,69
       pmas(I,1)=abs(psusy(i-11))
       smz(I-65)=psusy(i-11)
      ENDDO
      pmas(70,1)=abs(psusy(59))
      pmas(71,1)=abs(psusy(60))
      smw(1)=psusy(59)
      smw(2)=psusy(60)
c     psusy(61) = mstop1  :
c     psusy(62) = mstop2  :
c     psusy(63) = msbot1  :
c     psusy(64) = msbot2  :
c     psusy(65) = mstau1  :
c     psusy(66) = mstau2  :
      pmas(51,1)=psusy(61)
      pmas(52,1)=psusy(62)
C.....Determine the stop masses
      ATERM=psusy(2)*psusy(18)+psusy(13)/psusy(11)
      TAN2T=2.*ATERM*pmas(6,1)/(psusy(31)**2-psusy(25)**2)
      THETA=.5*ATAN(TAN2T)
      R(1,1)=cos(theta)
      R(2,2)=R(1,1)
      R(1,2)=-sin(theta)
      R(2,1)=-R(1,2)
      M2(1,1)=psusy(25)**2
      M2(2,2)=psusy(31)**2
      M2(1,2)=ATERM*psusy(9)
      M2(2,1)=M2(1,2)
C
      DO I1=1,2
       DO I2=1,2
        B2(I1,I2)=0.0
        C2(I1,I2)=0.0
       ENDDO
      ENDDO
      DO I1=1,2
       DO I2=1,2
        DO J1=1,2
         B2(I1,I2)=B2(I1,I2)+M2(I1,J1)*R(I2,J1)
        ENDDO
       ENDDO
      ENDDO
      DO I1=1,2
       DO I2=1,2
        DO J1=1,2
         C2(I1,I2)=C2(I1,I2)+R(I1,J1)*B2(J1,I2)
        ENDDO
       ENDDO
      ENDDO
*
      if(c2(1,1).gt.c2(2,2)) then
       R(1,1)=sin(theta)
       R(2,2)=R(1,1)
       R(1,2)=cos(theta)
       R(2,1)=-R(1,2)
      endif
      COSST=R(1,1)
      SINST=-R(1,2)
      if( abs(sqrt(min(c2(1,1),c2(2,2)))-psusy(61)).gt.1.5000) THEN
       print*,'mstop',sqrt(min(c2(1,1),c2(2,2))),psusy(61)
      endif
      beta=atan(psusy(11))
      alfa=psusy(50)
      ATRI_T=psusy(18)*psusy(2)
      ATRI_B=psusy(19)*psusy(2)
      ATRI_L=psusy(20)*psusy(2)
      MUZ=psusy(13)
C.....Define the mixing matrices
      DO I=1,4
       DO J=1,4
        z(i,j)=psusy(66+4*(i-1)+j)
       ENDDO
      ENDDO
      DO I=1,2
       DO J=1,2
        u(i,j)=psusy(82+2*(i-1)+j)
        v(i,j)=psusy(86+2*(i-1)+j)
       ENDDO
      ENDDO
C.....Stop the printing of the header
      MSTU(12)=0
      MSTP(122)=1
      MSTP(127)=0
C.....
      IF(DEBUG) CALL PRINT_SETUP
C.....Set up the decay information for the sparticles
      DO KC=41,71
       MDCY(KC,1)=1
       MDCY(KC,2) = MDCY(KC-1,2)+MDCY(KC-1,3)
       if(KC.eq.41) MDCY(KC,2)=NSTR
       CHAF(KC)=schaf(KC)
       DO J1=1,3
        KCHG(KC,J1) = skchg(KC,j1)
       ENDDO
C
       IF(KC.GE.41.and.KC.LE.71) THEN
        LKNT=0
        IF(KC.GE.41.and.KC.LE.64) THEN
C........21 April 95
C........First check to see if sneutrino is lighter than chi00
         IF(KC.EQ.55.or.KC.eq.59.or.KC.eq.63) THEN
          IF(PMAS(KC,1).LT.PMAS(66,1)) THEN
           PMAS(KC,2)=1.E-6
           MDCY(KC,1)=0
           GOTO 101
          ENDIF
         ENDIF
         CALL SFERMION(KC,XLAM,IDLAM,LKNT)
 101     CONTINUE
        ELSEIF(KC.EQ.65) THEN
         CALL GLUINO(KC,XLAM,IDLAM,LKNT)
        ELSEIF(KC.GE.70.AND.KC.LE.71) THEN
         CALL CHARGED_DECAY(KC,XLAM,IDLAM,LKNT)
        ELSEIF(KC.GE.66.AND.KC.LE.69) THEN
         CALL NEUTRAL_DECAY(KC,XLAM,IDLAM,LKNT)
        ENDIF
        IF(KC.EQ.66) THEN
         PMAS(KC,2)=1.E-6
         MDCY(KC,1)=0
        ENDIF
        IF(LKNT.eq.0) GOTO 1000
        MDCY(KC,3)=LKNT
C        IF(KC.NE.67.and.KC.NE.70) THEN
         DO J1=1,LKNT
          J2=MDCY(KC,2)+J1-1
          MDME(J2,1)=1
          MDME(J2,2)=0
          BRAT(J2) = XLAM(J1)/XLAM(0)
          DO I1=1,3
           KFDP(J2,I1)=IDLAM(J1,I1)
          ENDDO
         ENDDO
         PMAS(KC,2)=XLAM(0)
         IF(PMAS(KC,2).lt..1*pmas(kc,1)) THEN
          PMAS(KC,3)=PMAS(KC,2)*10.
         ELSE
          PMAS(KC,3)=.5*pmas(kc,1)
         ENDIF
C.........
         if(kc.ne.66) then
          delm=pmas(kc,1)-pmas(66,1)
          if(delm.le.pmas(kc,3)) pmas(kc,3)=.5*delm
         endiF
       ENDIF
 1000  CONTINUE

      ENDDO
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
      CALL TOP_TO_STOP
      RETURN
      END




