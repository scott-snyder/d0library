      FUNCTION MASSEVE(TOPMASS,TANB,MZERO,MONEHALF,AZERO,SMU,PASS)

C     This version not for distribution - a distribution version will be
C     available in weeks to come. This version does not contain all the
C     most recent code and upgrades, nor is it commented as it should be
C     for use outside this group. Changes should be made only to the routines
C     batch.f and interactive.f; all the rest should be left as is. If you
C     find any errors (or simply suspect some) please contact me at
C     kolda@umich.edu.
C
C                                  Christopher Kolda
C                                  Univ. of Michigan
C                                  23 May 1994

      IMPLICIT NONE

      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:TUNING.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'
      INCLUDE 'D0$SPYTHIA$INC:DIFFEQ.INC'
      INCLUDE 'D0$SPYTHIA$INC:WMASSES.INC'
      INCLUDE 'D0$SPYTHIA$INC:SOFT.INC'
      INCLUDE 'D0$SPYTHIA$INC:GUTSCALE.INC'

      INTEGER I,ISOLN,ISGNMU,J,K
      REAL*8 TOPMASS,TANB,MZERO,MONEHALF,AZERO,SMU
      REAL*8 SGN
      REAL*8 ALPHA3TOP,MTOPT,GTOPZ, GTOP
      REAL*8 GBOT,GTAUZ,GBOTZ
      REAL*8 PASS(94)
      EXTERNAL SGN,GTOPZ
      INTEGER MASSEVE
      GTOP_TOO_BIG = .FALSE.
      MASSEVE = 0.
      CALL INITIALIZE
      ALPHAX=1./25.
      JERROR=1
      NPRINT=0

      MQ(5)=TOPMASS
      A0=AZERO
      MUSIGN=SMU
      MHALF=MONEHALF
      M0=MZERO
      TBETA=TANB

      MMU=MUSIGN*MHALF
      B0=(A0-1.)*M0/200.
      A0=A0*M0/200.       !ensure proper definitions of A0 and B0
      SW2=0.2324 - 1.03E-7*(MQ(5)**2-138.**2)
      ALPHAW=1./127.9
      ALPHA(1)=5./3.*ALPHAW/(1.-SW2)
      ALPHA(2)=ALPHAW/SW2
      ALPHA(3)=0.122

C..JTL..        write(*,*) 'Finding mb(mz)...'
      CALL MBTAU(1,GBOTZ,GTAUZ)
C..JTL..        write(*,*) 'After mbtau call: gbotz,gtauz=',gbotz,gtauz

      ALPHA3TOP=ALPHA(3)/(1.+7.*ALPHA(3)/(2.*PI)*LOG(MQ(5)/MZ))
      MTOPT=MQ(5)/(1.+4./3.*ALPHA3TOP/PI+11.*(ALPHA3TOP/PI)**2)
C..JTL..        write(*,*) 'Finding gtop at Mz'
      GTOP=GTOPZ(MTOPT)
Cc      gtop=sqrt(2.)*mtopt/(vev*sin(atan(tbeta)))
C..JTL..        write(*,*) '      gtop=',gtop

      NDECOUPLE=0
      NPRINT=0

      ATOPZ=0.
      ABOTZ=0.
      ATAUZ=0.
      BZ=0.
      MH1SQ=(10./200.)**2
      MH2SQ=(10./200.)**2
      MUZ=50.
C...RJG... Next two loops from new_main.f (18-JULY-94)
      DO I=1,12
        WSQ(I)=50./200.
        WSL(I)=50./200.
      ENDDO
      DO I=1,3
        WGF(I)=50./200.
      ENDDO
      CALL UNIFY(GTOP,GBOTZ,GTAUZ,MZ)
      IF (GTOP_TOO_BIG) THEN
        PRINT*, 'GTOP TOO BIG 1'
        RETURN
      ENDIF
      IF (JERROR.NE.1) THEN
        MX=1.0D16
        ALPHAX=0.04
        GTOPX=0.5
        GBOTX=0.05
        GTAUX=0.05
        JERROR=1
      ENDIF

      DO I=1,5
C..JTL..            write(*,*) 'Now run down in Massevolve...'
        CALL MASSEVOLVE(GBOT)
        IF (GTOP_TOO_BIG) THEN
          PRINT*, 'GTOP TOO BIG 2'
          RETURN
        ENDIF
        IF (JERROR.NE.1) THEN
          MASSEVE=JERROR
          RETURN
        ENDIF

C..JTL..            write(*,*) 'Reunify...'
        CALL UNIFY(GTOP,GBOTZ,GTAUZ,MZ)
        IF (GTOP_TOO_BIG) THEN
          PRINT*, 'GTOP TOO BIG 3'
          RETURN
        ENDIF

        IF (JERROR.NE.1) THEN
          MASSEVE=JERROR
          RETURN
        ENDIF
      ENDDO

  100 CONTINUE

      CALL MASSEVOLVE(GBOT)
      IF (GTOP_TOO_BIG)  THEN
        PRINT*, 'GTOP TOO BIG 4'
        RETURN
      ENDIF
      IF ((SGN(MUSIGN).NE.SGN(MUZ)).OR.(MH(3).LT.0.)) THEN
        MUZ=0.
        MMU=0.
      ENDIF

C       write(*,*) 'Alpha3w=',sngl(alpha(3))
C       write(*,*) 'Alpha1w=',sngl(alpha1w)
C       write(*,*) 'Alpha2w=',sngl(alpha2w)
C       write(*,*) 'B0=',B0
C       write(*,*) 'Mu0=',Mmu

      CALL HIGGS_SECTOR()
      CALL MASS_MATRIX()
      CALL MBTAU(2,GBOT,GTAUZ)

      A0=A0*200./M0
      ATOPZ=ATOPZ*200./M0
      ABOTZ=ABOTZ*200./M0
      ATAUZ=ATAUZ*200./M0
      B0=B0*200./M0
      BZ=BZ*200./M0

      ISGNMU=NINT(0.5*(MUSIGN+1.))
      ISOLN= ISGNMU + 10.*NINT(M0/10.) + 1000.*NINT(MHALF/10.)
     &          + 100000.*NINT(10.*(5-A0))
     &          + 10000000.*NINT(MQ(5)-100.)

      PASS(1)=DBLE(ISOLN)
      PASS(2)=M0
      PASS(3)=MHALF
      PASS(4)=MMU
      PASS(5)=A0
      PASS(6)=B0
      PASS(7)=MX
      PASS(8)=ALPHAX
      PASS(9)=MQ(5)
      PASS(10)=MBOT
      PASS(11)=TBETA
      PASS(12)=ALPHA(3)
      PASS(13)=MUZ
      PASS(14)=BZ
      PASS(15)=GTOPX
      IF (GTOPX.GT.GTOP_MAX) WRITE (*,*) 'New gtopx_max = ', GTOPX
      GTOP_MAX = MAX(GTOPX, GTOP_MAX)
      PASS(16)=GBOTX
      PASS(17)=GTAUX
      PASS(18)=ATOPZ
      PASS(19)=ABOTZ
      PASS(20)=ATAUZ
      DO I=1,12
        PASS(20+I)=MSQ(I)
        PASS(32+I)=MSL(I)
      ENDDO
      DO I=1,4
        PASS(44+I)=MH(I)
      ENDDO
      PASS(49)=MH0
      PASS(50)=HALPHA
      PASS(51)=MGF(1)
      PASS(52)=MGF(2)
      PASS(53)=MGF(3)
      PASS(54)=FINETUNE
      DO I=1,4
        PASS(54+I)=MXN(I)*EPS(I)
      ENDDO
      PASS(59)=MXC(1)*EPS(5)
      PASS(60)=MXC(2)*EPS(6)
      PASS(61)=MSTOP(1)
      PASS(62)=MSTOP(2)
      PASS(63)=MSBOT(1)
      PASS(64)=MSBOT(2)
      PASS(65)=MSTAU(1)
      PASS(66)=MSTAU(2)
      I=67
      DO J=1,4
        DO K=1,4
          PASS(I)=Z(J,K)
          I=I+1
        ENDDO
      ENDDO
      DO J=1,2
        DO K=1,2
          PASS(I)=U(J,K)
          I=I+1
        ENDDO
      ENDDO
      DO J=1,2
        DO K=1,2
          PASS(I)=V(J,K)
          I=I+1
        ENDDO
      ENDDO
      DO J=1,2
        DO K=1,2
          PASS(I)=T(J,K)
          I=I+1
        ENDDO
      ENDDO

   40 CONTINUE

  999 MASSEVE=JERROR

      RETURN
      END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION GTOPZ(MTOPT)

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:DIFFEQ.INC'
      INTEGER I
      REAL*8 GTOPZ,EPS,H0,GTOPT,MTOPT
      REAL*8 SCALE0,SCALE1,VARS(NUMEQS),WORK(6*NUMEQS)
      EXTERNAL DIFFEQS

      VEV=2.*MW/SQRT(4.*PI*ALPHA(2))

C       Initialize vars(*) with initial conditions at weak scale
C
      GTOPZ = 0. ! intitalize, returned value is below
      VARS(1)=ALPHA(1)
      VARS(2)=ALPHA(2)
      VARS(3)=ALPHA(3)
      GTOPT=SQRT(2.)*MTOPT/(VEV*SIN(ATAN(TBETA)))
      VARS(4)=GTOPT
      VARS(5)=0.
      VARS(6)=0.
      DO I=7,26
        VARS(I)=0.
      ENDDO

C       Run Mt down to Mz
C
      H0=0.1
      EPS=1.E-4
      NEQS=6
      MSUSY=MQ(5)+100.
      SCALE0=LOG(MQ(5))
      SCALE1=LOG(MZ)
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN

      GTOPZ=VARS(4)

      RETURN
      END


Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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

Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MASSEVOLVE (GBOT)

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:DIFFEQ.INC'
      INCLUDE 'D0$SPYTHIA$INC:SOFT.INC'
      INCLUDE 'D0$SPYTHIA$INC:WMASSES.INC'
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'      !Common blocks for global variables.
      INCLUDE 'D0$SPYTHIA$INC:TUNING.INC'
      INCLUDE 'D0$SPYTHIA$INC:GUTSCALE.INC'

      INTEGER I
      REAL*8 GBOT,SQQRT
      REAL*8 BMU,M1SQ,M2SQ,MU2
      REAL*8 VARS(NUMEQS),EPS,SCALE0,SCALE1,H0,WORK(6*NUMEQS)
      EXTERNAL DIFFEQS,SQQRT


C       Initialize the VARS with initial conditions at GUT scale.
C
      IF (GTOP_TOO_BIG) RETURN

      DO I=1,3
        VARS(I)=ALPHAX
      ENDDO

      VARS(4)=GTOPX
      VARS(5)=GBOTX                      !! mb not equal mtau
      VARS(6)=GTAUX

      VARS(7)=A0
      VARS(8)=A0
      VARS(9)=A0

      VARS(10)=B0
C       write(*,*) 'B0=',B0
      VARS(11)=(M0/200.)**2
      VARS(12)=(M0/200.)**2
      VARS(13)=MMU/200.
      DO I=14,23
        VARS(I)=(M0/200.)**2
      ENDDO
      DO I=24,26
        VARS(I)=MHALF/200.
      ENDDO

C       Call subroutine to run from GUT scale to Z scale.
C
      SCALE0=LOG(MX)
      SCALE1=LOG(MZ)
      EPS=1.E-4
      H0=1.0
      NEQS=26
C       write(*,*) 'Running down in MASSEVE...'
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN

C       Assign traditional labels to VARS
C
C       write(*,*) 'At Mz: alpha1=',sngl(vars(1))
C       write(*,*) '       alpha2=',sngl(vars(2))
C       write(*,*) '       alpha3=',sngl(vars(3))
      ALPHA(3)=VARS(3)*(1.-VARS(3)*3./(12.*PI))       !DR-bar to MS-bar
      GBOT=VARS(5)
      ATOPZ=VARS(7)
      ABOTZ=VARS(8)
      ATAUZ=VARS(9)
      BZ=VARS(10)
      MH1SQ=VARS(11)
      MH2SQ=VARS(12)
      MUZ=VARS(13)
      WSQ(5)=SQQRT(VARS(14))
      WSQ(6)=WSQ(5)
      WSQ(11)=SQQRT(VARS(15))
      WSQ(12)=SQQRT(VARS(16))
      WSL(6)=SQQRT(VARS(17))
      WSL(5)=WSL(6)
      WSL(12)=SQQRT(VARS(18))
      WSL(2)=SQQRT(VARS(19))
      WSL(1)=WSL(2)
      WSL(3)=WSL(2)
      WSL(4)=WSL(2)
      WSL(8)=SQQRT(VARS(20))
      WSL(10)=WSL(8)
      WSQ(1)=SQQRT(VARS(21))
      WSQ(2)=WSQ(1)
      WSQ(3)=WSQ(1)
      WSQ(4)=WSQ(1)
      WSQ(7)=SQQRT(VARS(22))
      WSQ(9)=WSQ(7)
      WSQ(8)=SQQRT(VARS(23))
      WSQ(10)=WSQ(8)
      WGF(1)=VARS(24)
      WGF(2)=VARS(25)
      WGF(3)=VARS(26)

C..JTL..        write(*,*) 'RGE soln:'
C..JTL..        write(*,*) '  Mu(Mz)=',sngl(muz*200.)
C..JTL..        write(*,*) '  B(Mz)=',sngl(Bz)

C       Calculate EW breaking at Z-scale
C
      MU2=-0.5*MZ**2/200.**2+(MH1SQ-MH2SQ*TBETA**2)/(TBETA**2-1.)

C       if (mu2.lt.0.) then
C          muz=0.
C          Bz=1.
C       else
C          muz=musign*sqrt(mu2)*200.
C          Bmu=-0.5*sin(2.*atan(tbeta))*(mH1sq+mH2sq+2.*mu2)
C          Bz=Bmu/(muz/200.)
C       endif

      MUZ=MUSIGN*SQQRT(MU2)*200.
      BMU=-0.5*SIN(2.*ATAN(TBETA))*(MH1SQ+MH2SQ+2.*MU2)
      BZ=BMU/(MUZ/200.)

      M1SQ=200.**2*MH1SQ+MUZ**2
      M2SQ=200.**2*MH2SQ+MUZ**2
      MH(3)=SQQRT(M1SQ+M2SQ)
      FINETUNE=M1SQ/MZ**2

C..JTL..        write(*,*) 'Solved soln (tree level):'
C..JTL..        write(*,*) '  Mu(Mz)=',sngl(muz)
C..JTL..        write(*,*) '  B(Mz)=',sngl(Bz)
C..JTL..        write(*,*) '  m(A0)=',sngl(mh(3))

C       Now calculate B and mu with the full effective potential
C
C...RJG... added bail here from RTSEC -> Veff
      CALL VEFF()
      IF (GTOP_TOO_BIG) RETURN
      MH(3)=SQQRT(-2.*BZ*200.*MUZ/SIN(2.*ATAN(TBETA)))

C..JTL..        write(*,*) 'Solved soln (1-loop):'
C..JTL..        write(*,*) '  Mu(Mz)=',sngl(muz)
C..JTL..        write(*,*) '  B(Mz)=',sngl(Bz)
C..JTL..        write(*,*) '  m(A0)=',sngl(mh(3))

C       Calculate physical masses include D-terms
C
C..JTL..        write(*,*) 'Before calling D-terms: wsl*200=',wsl(1)*200.,
C..JTL..     &    wsl(2)*200
      CALL D_TERMS

C       Now run back up to get B0 and Mmu, first to 10 TeV, then rest of way.
C
C       write(*,*) 'Now find B0,Mmu...'
      VARS(10)=BZ
      VARS(13)=MUZ/200.
      EPS=1.E-4
      H0=1.0
      NEQS=26
      SCALE0=LOG(MZ)
      SCALE1=LOG(10.E3)

      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN

      H0=1.0
      SCALE0=SCALE1
      SCALE1=LOG(MX)

      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS,H0,EPS,DIFFEQS,WORK)
      IF (GTOP_TOO_BIG) RETURN

      MMU=VARS(13)*200.
      B0=VARS(10)
C..JTL..        write(*,*) 'At GUT scale: B0=',sngl(B0)
C..JTL..        write(*,*) '              Mmu=',sngl(Mmu)
C..JTL..        write(*,*) '              Atop=',sngl(vars(7))
C..JTL..        write(*,*) '              Atau=',sngl(vars(9))
C..JTL..        write(*,*) '              alpha1=',sngl(vars(1))
C..JTL..        write(*,*) '              alpha2=',sngl(vars(2))
C..JTL..        write(*,*) '              alpha3=',sngl(vars(3))

      RETURN
      END


Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      SUBROUTINE D_TERMS

C       This subroutine calculates the masses of the squarks and leptons
C       from their soft mass terms, their D-terms, and (where large enough)
C       their F-terms. It also diagonalizes the t,b,tau sfermion systems.
C
C       General form of D-term: -c2beta*mz**2*(-T_3 + Q*sw2)
C       Or equivalently: -c2beta*mw**2*(-T_3 + Y/2*tw2)
C
C       Note: There exists possible overall sign problems in the D-terms!!
C             My own calculation yields signs opposite of those below!

      IMPLICIT NONE
      INTEGER I,NROT
      REAL*8 C2BETA,MLR,MSQU,TW2
      REAL*8 SQQRT,MST(2,2)

      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'
      INCLUDE 'D0$SPYTHIA$INC:WMASSES.INC'       !these are the args of the routine

      EXTERNAL SQQRT

      C2BETA=COS(2.*ATAN(ABS(TBETA)))
      TW2=SW2/(1.-SW2)
      MSQ(1)=SQQRT(200.**2*WSQ(1)**2-MZ**2*C2BETA*(2.*SW2/3.-0.5))
      MSQ(2)=SQQRT(200.**2*WSQ(2)**2-MZ**2*C2BETA*(-SW2/3.+0.5))
      MSQ(3)=MSQ(1)
      MSQ(4)=MSQ(2)
      MSQ(5)=SQQRT(200.**2*WSQ(5)**2-MZ**2*C2BETA*(2.*SW2/3.-0.5)+MQ(5)
     +  **2)
      MSQ(6)=SQQRT(200.**2*WSQ(6)**2-MZ**2*C2BETA*(-SW2/3.+0.5)+MQ(6)
     +  **2)
      MSQ(7)=SQQRT(200.**2*WSQ(7)**2+MZ**2*C2BETA*2.*SW2/3.)
      MSQ(8)=SQQRT(200.**2*WSQ(8)**2-MZ**2*C2BETA*SW2/3.)
      MSQ(9)=MSQ(7)
      MSQ(10)=MSQ(8)
      MSQ(11)=SQQRT(200.**2*WSQ(11)**2+MZ**2*C2BETA*2.*SW2/3.+MQ(5)**2)
      MSQ(12)=SQQRT(200.**2*WSQ(12)**2-MZ**2*C2BETA*SW2/3.+MQ(6)**2)
      MSL(1)=SQQRT(200.**2*WSL(1)**2+MZ**2*C2BETA/2.)
      MSL(2)=SQQRT(200.**2*WSL(2)**2-MZ**2*C2BETA*(0.5-SW2))
      MSL(3)=MSL(1)
      MSL(4)=MSL(2)
      MSL(5)=MSL(1)
      MSL(6)=SQQRT(200.**2*WSL(6)**2-MZ**2*C2BETA*(0.5-SW2)+ML(6)**2)
      MSL(8)=SQQRT(200.**2*WSL(8)**2-MZ**2*C2BETA*SW2)
      MSL(10)=MSL(8)
      MSL(12)=SQQRT(200.**2*WSL(12)**2-MZ**2*C2BETA*SW2+ML(6)**2)

C       Diagonalize top squark mass matrix
C
C       write(*,*) 'In D-terms: mtop=',sngl(mq(5)),'  Muz=',sngl(muz)
C       write(*,*) '     Atopz=',sngl(atopz),'   tbeta=',sngl(tbeta)
      MST(1,1)=MSQ(5)**2
      MST(2,2)=MSQ(11)**2
      MST(1,2)=MQ(5)*(200.*ATOPZ+MUZ/ABS(TBETA))
      MST(2,1)=MST(1,2)
      CALL DJACOBI(MST,2,MSTOP,T,NROT)
      CALL DEIGSRT(MSTOP,T,2)
      CALL DTRPOSE(T,2)
      MSTOP(1)=SQQRT(MSTOP(1))
      MSTOP(2)=SQQRT(MSTOP(2))

C       msqu=0.5*(msq(5)**2+msq(11)**2-sqqrt((msq(5)**2
C     &   -msq(11)**2)**2+4.*mlr**2))
C       mt1=sqqrt(msqu)
C       msqu=0.5*(msq(5)**2+msq(11)**2+sqqrt((msq(5)**2
C     &   -msq(11)**2)**2+4.*mlr**2))
C       mt2=sqqrt(msqu)

C       Diagonalize bottom squark mass matrix
C
      MLR=MQ(6)*(200.*ABOTZ+MUZ*ABS(TBETA))
      MSQU=0.5*(MSQ(6)**2+MSQ(12)**2-SQQRT((MSQ(6)**2
     &    -MSQ(12)**2)**2+4.*MLR**2))
      MSBOT(1)=SQQRT(MSQU)
      MSQU=0.5*(MSQ(6)**2+MSQ(12)**2+SQQRT((MSQ(6)**2
     &    -MSQ(12)**2)**2+4.*MLR**2))
      MSBOT(2)=SQQRT(MSQU)

C       Diagonalize tau slepton mass matrix
C
      MLR=ML(6)*(200.*ATAUZ+MUZ*ABS(TBETA))
      MSQU=0.5*(MSL(6)**2+MSL(12)**2-SQQRT((MSL(6)**2
     &    -MSL(12)**2)**2+4.*MLR**2))
      MSTAU(1)=SQQRT(MSQU)
      MSQU=0.5*(MSL(6)**2+MSL(12)**2+SQQRT((MSL(6)**2
     &    -MSL(12)**2)**2+4.*MLR**2))
      MSTAU(2)=SQQRT(MSQU)

      DO I=1,3
        MGF(I)=200.*WGF(I)
      ENDDO


      RETURN
      END


