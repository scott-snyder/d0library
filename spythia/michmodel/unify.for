      SUBROUTINE UNIFY (GTOP,GBOT,GTAU,MPASS)

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:DIFFEQ.INC'
      INCLUDE 'D0$SPYTHIA$INC:WMASSES.INC'
      INCLUDE 'D0$SPYTHIA$INC:SOFT.INC'
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'      !Common blocks for global variables.
      INCLUDE 'D0$SPYTHIA$INC:GUTSCALE.INC'

      REAL*8 GBOT,GTAU,SGN
      REAL*8 GTOP,DA_INV1,DA_INV2,MPASS
      REAL*8 VARS(NUMEQS),EPS,SCALE0,SCALE1,H0,WORK(6*NUMEQS)
      EXTERNAL SGN,DIFFEQS


C       Initialize the vars(*) with initial conditions at weak scale.
C
      VARS(1)=ALPHA(1)
      VARS(2)=ALPHA(2)/(1.-ALPHA(2)*2./(12.*PI))   !MS-bar to DR-bar
      VARS(3)=ALPHA(3)/(1.-ALPHA(3)*3./(12.*PI))

      VARS(4)=GTOP
      VARS(5)=GBOT
      VARS(6)=GTAU

      VARS(7)=ATOPZ
      VARS(8)=ABOTZ
      VARS(9)=ATAUZ
      VARS(10)=BZ
      VARS(11)=MH1SQ
      VARS(12)=MH2SQ
      VARS(13)=MUZ/200.
      VARS(14)=WSQ(5)**2
      VARS(15)=WSQ(11)**2
      VARS(16)=WSQ(12)**2
      VARS(17)=WSL(6)**2
      VARS(18)=WSL(12)**2
      VARS(19)=WSL(2)**2
      VARS(20)=WSL(8)**2
      VARS(21)=WSQ(1)**2
      VARS(22)=WSQ(7)**2
      VARS(23)=WSQ(8)**2
      VARS(24)=WGF(1)
      VARS(25)=WGF(2)
      VARS(26)=WGF(3)

C       Call subroutine to run from Z scale to GUT scale.
C
      SCALE0=LOG(MPASS)
      SCALE1=LOG(10.E3)
      EPS=1.E-5
      H0=1.0
      NEQS=6
C       write(*,*) 'Ruuning up in UNIFY...'
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN

      SCALE0=SCALE1
      SCALE1=34.4  ! = log(8.7e14)
      H0=1.0
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN


      DA_INV1=1./VARS(1)-1./VARS(2)
      DO SCALE0=34.4,39.2,0.2   !log(8.7e14) to log(1.e17)
        SCALE1=SCALE0+0.2
        CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
        IF (GTOP_TOO_BIG) RETURN
        DA_INV2=1./VARS(1)-1./VARS(2)
C           write(*,*) 'In unify:'
C           write(*,*) '   scale,al1,al2,al3=',scale1,vars(1),vars(2),vars(3)
        IF (SGN(DA_INV1).NE.SGN(DA_INV2)) THEN
          MX=EXP((DA_INV1*SCALE1 - DA_INV2*SCALE0)/(DA_INV1-DA_INV2))
          GOTO 100
        ENDIF
        DA_INV1=DA_INV2
      ENDDO
      WRITE(*,*) 'GUT scale not found!!!!'
      RETURN

  100 SCALE0=LOG(MX)
      CALL DDEQMR(NUMEQS,SCALE1,SCALE0,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN

      ALPHAX=(VARS(1)+VARS(2))/2.
C       alphax= 1./25.31                !!!!!For testing purposes only!!!!
                                        !!1/25.2 seems to work best for his sample model!!
      ALPHA3X=VARS(3)


      GTOPX=VARS(4)
      GTAUX=VARS(6)
      GBOTX=VARS(5)

C..JTL..        write(*,*) '1/Alphax=',sngl(1./alphax),'    Mx=',sngl(Mx)
C..JTL..        write(*,*) '1/Alpha1-1/Alpha2=',sngl(1./vars(1)-1./vars(2))
C..JTL..        write(*,*) '1/Alpha3=',sngl(1./vars(3))
C..JTL..        write(*,*) 'Gtopx,gbotx,gtaux=',gtopx,gbotx,gtaux

      RETURN
      END
