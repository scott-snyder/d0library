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
