      SUBROUTINE VEFF

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:SOFT.INC'
      REAL*8 MU2,SIG1,SIG2,RTSEC
      REAL*8 MU_SOLVER,LIM1,LIM2,XACC,SQQRT
      EXTERNAL MU_SOLVER

C..JTL..        write(*,*) 'In VEFF: Bz,muz=',bz,muz
      IF (MUZ.EQ.0.) THEN
        LIM1=0.5*MHALF**2
        LIM2=2.0*MHALF**2
      ELSE
        LIM1=0.8*MUZ**2
        LIM2=1.2*MUZ**2
      ENDIF
      XACC=1.0
      MU2=RTSEC(MU_SOLVER,LIM1,LIM2,XACC)
      IF (GTOP_TOO_BIG) RETURN
C       if (mu2.le.0.) then
C          muz=0.
C          Bz=0.
C          return
C       endif

C       muz=musign*sqrt(mu2)
      MUZ=MUSIGN*SQQRT(MU2)
      CALL ONE_LOOP_CORRS(MU2,SIG1,SIG2)

      BZ=-SIN(2.*ATAN(TBETA))*(MH1SQ*200.**2+MH2SQ*200.**2+2.*MU2
     &    +SIG1+SIG2)/(2.*MUZ)/200.

      RETURN
      END
