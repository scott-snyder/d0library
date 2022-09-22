      SUBROUTINE VEE2D(PHI1,XG1,YG1,ON1,N1,PHI2,XG2,YG2,ON2,N2,
     1                  XV,YV,OK) 
C------------------------------------------------------------------
C 
C  Check if a pair of central tracks makes a vee in r-phi plane. 
C
C  Input:    PHI1,XG1,YG1,PHI2,XG2,YG2 - phi and center of gravity
C            for the two tracks
C            ON1,ON2    - wires on track 1, 2
C            N1,N2      1: VTX  2: CDC  3: FDC
C                             
C  Output:   XV,YV - vertex coordinates
C            OK    - true if good vee (the distance between the 
C                    primary vertex and the vee > RVMIN and the
C                    vee points to the primary vertex (not away)
C 
C  Daria Zieminska 9-JUL-1990
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
      INTEGER LVRFT,GZVRFT,GZDGEH,LDRFT,GZDRFT,LDALS,GZDALS
      INTEGER LAYER,WIRE,LAYMIN,WIRMIN
      INTEGER ICALL,IER,IB(32),NB,LOC,ON1,ON2,N1,N2
      REAL RWIRE_VTX(0:7,0:2),RWIRE_CDC(0:6,0:3)
      REAL R1MIN,R2MIN,RVMIN,RVMIN1,RVMIN2,XG1,YG1,XG2,YG2,XV,YV
      REAL RV,PHI1,PHI2,TAN1,TAN2
      LOGICAL OK,READSTP
      SAVE ICALL,RVMIN,RWIRE_VTX
      DATA ICALL/0/
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VEES_RCP')
        CALL EZGET('RVMIN1',RVMIN1,IER)
        CALL EZGET('RVMIN2',RVMIN2,IER)
        CALL EZGET_l('READSTP',READSTP,IER)
        IF (READSTP.EQV..FALSE.) GO TO 1001
C
C  Get wire positions from STP
C
C VTX: 
        IF ( LVGEH .LE. 0 ) GO TO 1000 
        LVRFT=GZVRFT()
        DO 100 LAYER=0, 2
          DO 200 WIRE=0, 7
            RWIRE_VTX(WIRE,LAYER)=C(LVRFT+7+7*LAYER)+C(LVRFT+23+WIRE)
  200     CONTINUE 
  100   CONTINUE 
C CDC: 
        LDGEH = GZDGEH()
        IF ( LDGEH .LE. 0 ) GO TO 1000 
        LDRFT=GZDRFT()
        DO 300 LAYER=0,3
          LDALS = GZDALS(LAYER,0)
          DO 400 WIRE=0,6
            LOC=LDALS+6+WIRE*IC(LDALS+6)
            RWIRE_CDC(WIRE,LAYER)=SQRT(C(LOC+1)**2+C(LOC+2)**2)
  400     CONTINUE 
  300   CONTINUE 
 1001   CONTINUE
        ICALL=1
      END IF
      OK=.FALSE.
      TAN1=TAN(PHI1)
      TAN2=TAN(PHI2)
      XV=(YG2-YG1+XG1*TAN1-XG2*TAN2)/(TAN1-TAN2)
      YV=YG1+(XV-XG1)*TAN1
      RV=SQRT(XV**2+YV**2)
      IF (RV.LT.RVMIN1) GO TO 1000
      IF (N1.GE.2.AND.N2.GE.2) THEN
        IF (RV.LT.RVMIN2) GO TO 1000
      END IF
      R1MIN=60.
      R2MIN=60.
      IF (RV.GT.R2MIN) GO TO 1000
      IF (READSTP.EQV..FALSE.) THEN
        OK=.TRUE.
        GO TO 1000
      END IF
      IF (N1.EQ.1) THEN  ! VTX track
        CALL UBITS(ON1,32,IB,NB)
        WIRMIN=IB(1)-1
        LAYMIN=WIRMIN/8
        WIRMIN=MOD(WIRMIN,8)
        R1MIN=RWIRE_VTX(WIRMIN,LAYMIN)  
      ELSE IF (N1.EQ.2) THEN ! CDC track
        CALL UBITS(ON1,32,IB,NB)
        WIRMIN=IB(1)-1
        LAYMIN=WIRMIN/7
        WIRMIN=MOD(WIRMIN,7)
        R1MIN=RWIRE_CDC(WIRMIN,LAYMIN) 
      END IF
      IF (RV.LT.R1MIN+0.5) THEN
        IF (N2.EQ.1) THEN  ! VTX track
          CALL UBITS(ON2,32,IB,NB)
          WIRMIN=IB(1)-1
          LAYMIN=WIRMIN/8
          WIRMIN=MOD(WIRMIN,8)
          R2MIN=RWIRE_VTX(WIRMIN,LAYMIN) 
        ELSE IF (N2.EQ.2) THEN ! CDC track
          CALL UBITS(ON2,32,IB,NB)
          WIRMIN=IB(1)-1
          LAYMIN=WIRMIN/7
          WIRMIN=MOD(WIRMIN,7)
          R2MIN=RWIRE_CDC(WIRMIN,LAYMIN) 
        END IF
        IF (RV.LT.R2MIN+0.5) THEN
          OK=.TRUE.
        END IF
      END IF
 1000 CONTINUE 
      RETURN
      END
