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
