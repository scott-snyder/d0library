      SUBROUTINE ONE_LOOP_CORRS(MU2,SIG1,SIG2)

C       This subroutine calculates the effect of the 1-loop correction to
C       the higgs potential. It determines 2 values, sig1 and sig2, which
C       represent the shifts to m1sq and m2sq due to the corrections.
C       Formulas follow Arnowitt and Nath, Phys Rev D46, 3981 (1992),
C       hopefully with all the typos in that article corrected.
C
C       Make sure D-TERMS is called before referencing this routine.

      IMPLICIT NONE
      REAL*8 SIG1,SIG2,S2BETA,C2BETA,MU,MU2
      REAL*8 TEMP,MPH,MZI,MPZ,E,SBETA,NCOL,SQQRT
      INTEGER I

      INCLUDE 'D0$SPYTHIA$INC:WMASSES.INC'
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'
      INCLUDE 'D0$SPYTHIA$INC:SOFT.INC'

      SIG1=0.
      SIG2=0.
      E=EXP(1.0)
      NCOL=3.
      SBETA=SIN(ATAN(ABS(TBETA)))
      S2BETA=SIN(2.*ATAN(ABS(TBETA)))
      C2BETA=SQRT(1.-S2BETA**2)
      MU=MUSIGN*SQQRT(MU2)
      MUZ=MU

      CALL D_TERMS(WSQ,WSL,WGF)
      CALL MASS_MATRIX()
      CALL HIGGS_SECTOR_NOCORRS()

      DO I=1,5,2

        IF (I.EQ.5) GOTO 101    !skip top squark

                                    !LH up and charm squarks
        TEMP=(SW2/6. - (1.-SW2)/2.)/(8.*PI*(1.-SW2))*MSQ(I)**2
     &        *LOG(MSQ(I)**2/(E*MZ**2))*ALPHA(2)*NCOL
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP
                                    !RH up and charm squarks
        TEMP=(-SW2*2./3.)/(8.*PI*(1.-SW2))*MSQ(I+6)**2
     &        *LOG(MSQ(I+6)**2/(E*MZ**2))*ALPHA(2)*NCOL
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP

  101   CONTINUE
                                    !LH d,s,b squarks
        TEMP=(SW2/6. + (1.-SW2)/2.)/(8.*PI*(1.-SW2))*MSQ(I+1)**2
     &        *LOG(MSQ(I+1)**2/(E*MZ**2))*ALPHA(2)*NCOL
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP
                                    !RH d,s,b squarks
        TEMP=(SW2/3.)/(8.*PI*(1.-SW2))*MSQ(I+7)**2
     &        *LOG(MSQ(I+7)**2/(E*MZ**2))*ALPHA(2)*NCOL
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP
                                    !LH nu_(e,mu,tau) sneutrinos
        TEMP=(-SW2/2. - (1.-SW2)/2.)/(8.*PI*(1.-SW2))*MSL(I)**2
     &        *LOG(MSL(I)**2/(E*MZ**2))*ALPHA(2)
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP
                                    !LH e,mu,tau sleptons
        TEMP=(-SW2/2. + (1.-SW2)/2.)/(8.*PI*(1.-SW2))*MSL(I+1)**2
     &        *LOG(MSL(I+1)**2/(E*MZ**2))*ALPHA(2)
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP
                                    !RH e,mu,tau sleptons
        TEMP=SW2/(8.*PI*(1.-SW2))*MSL(I+7)**2
     &        *LOG(MSL(I+7)**2/(E*MZ**2))*ALPHA(2)
        SIG1=SIG1 - TEMP
        SIG2=SIG2 + TEMP

      ENDDO
                                    !t squarks
      TEMP=ALPHA(2)/(8.*PI*(1.-SW2))*MSTOP(1)**2
     &    *(0.25-(0.5*(MSQ(5)**2-MSQ(11)**2)
     &    *(1./2.-4./3.*SW2) + MQ(5)**2*MU/(MZ**2*SBETA**2)
     &    *(ATOPZ*200.*TBETA+MU))
     &    /(MSTOP(2)**2-MSTOP(1)**2))*LOG(MSTOP(1)**2/(E*MZ**2))*NCOL
      SIG1=SIG1+TEMP

      TEMP=ALPHA(2)/(8.*PI*(1.-SW2))*MSTOP(2)**2
     &    *(0.25+(0.5*(MSQ(5)**2-MSQ(11)**2)
     &    *(1./2.-4./3.*SW2) + MQ(5)**2*MU/(MZ**2*SBETA**2)
     &    *(ATOPZ*200.*TBETA+MU))
     &    /(MSTOP(2)**2-MSTOP(1)**2))*LOG(MSTOP(2)**2/(E*MZ**2))*NCOL
      SIG1=SIG1+TEMP

      TEMP=ALPHA(2)/(8.*PI*(1.-SW2))*MSTOP(1)**2
     &    *((MQ(5)**2/(MZ**2*SBETA**2)
     &    -0.25) - (0.5*(MSQ(5)**2-MSQ(11)**2)*(-1./2.+4./3.*SW2)
     &    +MQ(5)**2*ATOPZ*200./(MZ**2*SBETA**2)*(ATOPZ*200.+MU/TBETA))
     &    /(MSTOP(2)**2-MSTOP(1)**2))
     &    *LOG(MSTOP(1)**2/(E*MZ**2))*NCOL
      SIG2=SIG2+TEMP

      TEMP=ALPHA(2)/(8.*PI*(1.-SW2))*MSTOP(2)**2
     &    *((MQ(5)**2/(MZ**2*SBETA**2)
     &    -0.25) + (0.5*(MSQ(5)**2-MSQ(11)**2)*(-1./2.+4./3.*SW2)
     &    + MQ(5)**2*ATOPZ*200./(MZ**2*SBETA**2)*(ATOPZ*200.+MU/TBETA))
     &    /(MSTOP(2)**2-MSTOP(1)**2))
     &    *LOG(MSTOP(2)**2/(E*MZ**2))*NCOL
      SIG2=SIG2+TEMP
                                    !top quark
      TEMP=-ALPHA(2)/(4.*PI*(1.-SW2))*MQ(5)**4/(MZ**2*SBETA**2)
     &    *LOG(MQ(5)**2/(E*MZ**2))*NCOL
      SIG2=SIG2+TEMP
                                    !W and Z bosons
      TEMP=3.*ALPHA(2)*MZ**2/(16.*PI)*((2.*(1.-SW2)+1./(1.-SW2))
     &    *LOG(MZ**2/(E*MZ**2)) + 2.*(1.-SW2)*LOG(1.-SW2))
      SIG1=SIG1+TEMP
      SIG2=SIG2+TEMP
                                    !h Higgs boson
      TEMP=ALPHA(2)*MH(1)**2/(32.*PI*(1.-SW2))*(1. - (MZ**2+MH(3)**2
     &    *(1.+4.*C2BETA+2.*C2BETA**2))
     &    /(MH(2)**2-MH(1)**2))*LOG(MH(1)**2/(E*MZ**2))
      SIG1=SIG1+TEMP

      TEMP=ALPHA(2)*MH(1)**2/(32.*PI*(1.-SW2))*(1. - (MZ**2+MH(3)**2
     &    *(1.-4.*C2BETA+2.*C2BETA**2))
     &    /(MH(2)**2-MH(1)**2))*LOG(MH(1)**2/(E*MZ**2))
      SIG2=SIG2+TEMP
                                    !H Higgs boson
      TEMP=ALPHA(2)*MH(2)**2/(32.*PI*(1.-SW2))*(1. + (MZ**2+MH(3)**2
     &    *(1.+4.*C2BETA+2.*C2BETA**2))
     &    /(MH(2)**2-MH(1)**2))*LOG(MH(2)**2/(E*MZ**2))
      SIG1=SIG1+TEMP

      TEMP=ALPHA(2)*MH(2)**2/(32.*PI*(1.-SW2))*(1. + (MZ**2+MH(3)**2
     &    *(1.-4.*C2BETA+2.*C2BETA**2))
     &    /(MH(2)**2-MH(1)**2))*LOG(MH(2)**2/(E*MZ**2))
      SIG2=SIG2+TEMP
                                    !H+ Higgs boson
      TEMP=ALPHA(2)/(8.*PI)*(MH(4)**2)*LOG(MH(4)**2/(E*MZ**2))
      SIG1=SIG1+TEMP
      SIG2=SIG2+TEMP
                                    !Charginos
      TEMP=-ALPHA(2)/(4.*PI)*MXC(1)**2*(1.-(2.*MW**2*C2BETA
     &    +MGF(2)**2+MUZ**2+2.*MGF(2)*MUZ*TBETA)/(MXC(2)**2
     &    -MXC(1)**2))*LOG(MXC(1)**2/(E*MZ**2))
      SIG1=SIG1+TEMP

      TEMP=-ALPHA(2)/(4.*PI)*MXC(1)**2*(1.-(-2.*MW**2*C2BETA
     &    +MGF(2)**2+MUZ**2+2.*MGF(2)*MUZ/TBETA)/(MXC(2)**2
     &    -MXC(1)**2))*LOG(MXC(1)**2/(E*MZ**2))
      SIG2=SIG2+TEMP

      TEMP=-ALPHA(2)/(4.*PI)*MXC(2)**2*(1.+(2.*MW**2*C2BETA
     &    +MGF(2)**2+MUZ**2+2.*MGF(2)*MUZ*TBETA)/(MXC(2)**2
     &    -MXC(1)**2))*LOG(MXC(2)**2/(E*MZ**2))
      SIG1=SIG1+TEMP

      TEMP=-ALPHA(2)/(4.*PI)*MXC(2)**2*(1.+(-2.*MW**2*C2BETA
     &    +MGF(2)**2+MUZ**2+2.*MGF(2)*MUZ/TBETA)/(MXC(2)**2
     &    -MXC(1)**2))*LOG(MXC(2)**2/(E*MZ**2))
      SIG2=SIG2+TEMP
                                    !Neutralinos
      MPH=MGF(2)*SW2 + MGF(1)*(1.-SW2)
      MZI=MGF(2)*(1.-SW2) + MGF(1)*SW2
      MPZ=SQRT(SW2)*SQRT(1.-SW2)*(MGF(1)-MGF(2))
      DO I=1,4
        IF (MXN(I).LT.1.) GOTO 102  !Avoid artificial blowup at M=0.

        TEMP=-ALPHA(2)/(4.*PI*(1.-SW2))*(MXN(I)**5*EPS(I)
     &        + MU*MXN(I)**4
     &        - MXN(I)**3*EPS(I)*MU*MPH)*TBETA/(4.*MXN(I)**3*EPS(I)
     &        - 3.*(MPH+MZI)*MXN(I)**2
     &        - 2.*(MZ**2+MU**2+MPZ**2-MPH*MZI)
     &        *MXN(I)*EPS(I) + (MPH-MU*S2BETA)*MZ**2
     &        +(MPH+MZI)*MU**2) * LOG(MXN(I)**2/(E*MZ**2))
        SIG1=SIG1+TEMP

        TEMP=-ALPHA(2)/(4.*PI*(1.-SW2))*(MXN(I)**5*EPS(I)
     &        + MU*MXN(I)**4
     &        - MXN(I)**3*EPS(I)*MU*MPH)/TBETA/(4.*MXN(I)**3*EPS(I)
     &        - 3.*(MPH+MZI)*MXN(I)**2
     &        - 2.*(MZ**2+MU**2+MPZ**2-MPH*MZI)
     &        *MXN(I)*EPS(I) + (MPH-MU*S2BETA)*MZ**2
     &        +(MPH+MZI)*MU**2) * LOG(MXN(I)**2/(E*MZ**2))
        SIG2=SIG2+TEMP

  102   CONTINUE
      ENDDO

C       write(*,*) 'In 1-loop-cors; sig1,sig2,mu2=',sig1,sig2,mu2
      RETURN
      END
