

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
