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
