      SUBROUTINE L0_ADC_EXPECT(BUNCH,CHARGE,TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calaulate N,TSUM,T2SUM,TMAX,TMIN,TCOR,
C-                         TAVE,VTX,SIGMA,MI,INTER,GOODZ using CHARGE,
C-                         TIME and the calibration constants
C-
C-   Inputs  : Bunch : Bunch ID
C              Charge: L0 Raw Charge
C-             Time  : L0 Raw Time
C-
C-   Quant.  : N     : Number of Counters being hit
C-             Tsum  : Sum of corrected time for those counters being hit
C-             T2sum : Sum of square of corrceted time for those counters
C-                     being hit
C-             Tmax  : Max corrceted time
C-             Tmin  : Min corrected time
C-             Tcor  : Corrected Time
C-             Tave  : Average Time
C-             Vtx   : Vertex position
C-             Sigma : Sigma
C-             MI    : Multiple Interaction Flag
C-             Inter : Interaction Flag (0: No Collision,1: Collision)
C-             GoodZ : Good Vertex Flag (0: Bad Vertex,1: Good Vertex)
C-             Qsum  : Charge Sum
C-             NUMHITS: Number of hits used in statistics calculation
C-             EXCLUSIVE: 1=without max and min times in stat calculation
C-                        0= with max and min times in stat calculation
C-             SHORTY: 0= with long counters, 1= only short counters
C-   Controls:
C-
C-   Created  20-AUG-1992   Haowei Xu
C-   Updated   8-DEC-1992   Freedy Nang    Tailored for D0GEANT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
      INTEGER BUNCH,ibunch
      INTEGER CHARGE(80),TIME(80)
      INTEGER CHAN,I,J,K,ERR
      INTEGER CUT(4),MC(10)
      INTEGER TDIF,NSHORT
      REAL CUTI(4)
      REAL PED_ARRAY(5,72),QPED(72)
      REAL CALIB_ARRAY(5,72),LIMIT_ARRAY(4,2)
      REAL T0(72),SLOPE(72),QUAD(72),SLEW(72)
      REAL Q_MIN(72),Q_MAX(72)
      REAL T_MIN(72),T_MAX(72)
      REAL Q,T
      INCLUDE 'D0$INC:LV0PARAM.INC'
      LOGICAL EZERROR
      LOGICAL FIRST
      LOGICAL VALID(2)
      EXTERNAL EZERROR
      SAVE FIRST,Q_MIN,Q_MAX,T_MIN,T_MAX
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL VZERO(PED_ARRAY,360)
        CALL VZERO(CALIB_ARRAY,360)
        CALL EZPICK('MCLEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('MCLEVEL0-no-rcp','L0_ADC_EXPECT',
     &                              'MCLEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('PED_ARRAY',PED_ARRAY,ERR)
          CALL EZGET('CALIB_ARRAY',CALIB_ARRAY,ERR)
          CALL EZGET('LIMIT_ARRAY',LIMIT_ARRAY,ERR)
          CALL EZGET('CUT_1',CUTI(1),ERR)
          CALL EZGET('CUT_2',CUTI(2),ERR)
          CALL EZGET('CUT_3',CUTI(3),ERR)
          CALL EZGET('CUT_4',CUTI(4),ERR)
          CALL EZGET('MC_1',MC(1),ERR)
          CALL EZGET('MC_2',MC(2),ERR)
          CALL EZGET('MC_3',MC(3),ERR)
          CALL EZGET('MC_4',MC(4),ERR)
          CALL EZGET('MC_5',MC(5),ERR)
          CALL EZGET('MC_6',MC(6),ERR)
          CALL EZGET('MC_7',MC(7),ERR)
          IBUNCH=BUNCH
        ENDIF
        CALL EZRSET
C
        DO I=1,4
          CUT(I)=INT(CUTI(I))
        ENDDO
C
        DO CHAN=1,20
          Q_MIN(CHAN) = LIMIT_ARRAY(1,1)
          Q_MAX(CHAN) = LIMIT_ARRAY(2,1)
          T_MIN(CHAN) = LIMIT_ARRAY(3,1)
          T_MAX(CHAN) = LIMIT_ARRAY(4,1)
          Q_MIN(CHAN+36) = LIMIT_ARRAY(1,1)
          Q_MAX(CHAN+36) = LIMIT_ARRAY(2,1)
          T_MIN(CHAN+36) = LIMIT_ARRAY(3,1)
          T_MAX(CHAN+36) = LIMIT_ARRAY(4,1)
        ENDDO
C
        DO CHAN=21,36
          Q_MIN(CHAN) = LIMIT_ARRAY(1,2)
          Q_MAX(CHAN) = LIMIT_ARRAY(2,2)
          T_MIN(CHAN) = LIMIT_ARRAY(3,2)
          T_MAX(CHAN) = LIMIT_ARRAY(4,2)
          Q_MIN(CHAN+36) = LIMIT_ARRAY(1,2)
          Q_MAX(CHAN+36) = LIMIT_ARRAY(2,2)
          T_MIN(CHAN+36) = LIMIT_ARRAY(3,2)
          T_MAX(CHAN+36) = LIMIT_ARRAY(4,2)
        ENDDO
C
        DO I=1,72
          CHAN = INT(PED_ARRAY(1,I))
          IF ( CHAN.GT.0 .AND. CHAN.LE.72 ) THEN
            T_MAX(CHAN) = PED_ARRAY(2,I)-T_MAX(CHAN)
            QPED(CHAN)=PED_ARRAY(4,I)
          ENDIF
        ENDDO
        CALL VZERO(T0,72)
        CALL VZERO(SLOPE,72)
        CALL VZERO(QUAD,72)
        CALL VZERO(SLEW,72)
        DO I=1,72
          CHAN = INT(CALIB_ARRAY(1,I))
          IF ( CHAN.GT.0 .AND. CHAN.LE.72 ) THEN
            T0(CHAN)    = CALIB_ARRAY(2,CHAN)
            SLOPE(CHAN) = CALIB_ARRAY(3,CHAN)
            QUAD(CHAN)  = CALIB_ARRAY(4,CHAN)
            SLEW(CHAN)  = CALIB_ARRAY(5,CHAN)
          ENDIF
        ENDDO
        FIRST = .FALSE.
      ENDIF
C
      DO I=1,2
        DO J=1,2
          N(I,J)=0
          TSUM(I,J)=0
          T2SUM(I,J)=0
          TMAX(I,J)=0
          TMIN(I,J)=255
        ENDDO
      ENDDO
c
      QSUM=0.0
      DO I=1,72
        Q=FLOAT(CHARGE(I))-QPED(I)
        IF (Q.GT.Q_MIN(I)) QSUM=QSUM+0.01*Q
      ENDDO
C
C     Calculate corrected time,N,TSUM,T2SUM,TMAX and TMIN for short counters
C
      IF ( SLV0(3).EQ.2.0 ) THEN
        NSHORT=36
      ELSE
        NSHORT=20
      ENDIF
      DO J=1,2
        DO I=1,NSHORT
          IF (J.EQ.1) THEN
            K=I
          ELSE
            K=I+36
          ENDIF
C
C     If Both charge and time are in the required windows,calculate
C     the corrected time
C
          Q=FLOAT(CHARGE(K))-QPED(K)
          T=FLOAT(TIME(K))
          IF (Q.GT.Q_MIN(K).AND.CHARGE(K).LT.Q_MAX(K).AND.
     &        T.GT.T_MIN(K).AND.T.LT.T_MAX(K)) THEN
            TCOR(K)=TIME(K)*SLV0(2)
C
C     If no calibration constans available set the corrected time at 0
C
            IF (SLOPE(K).EQ.0.0.AND.QUAD(K).EQ.0.0.
     &        AND.SLEW(K).EQ.0.0) TCOR(K)=0
C
C     If the corrected time is inside the time window, mask TCOR to
C     get 8 bits corrected time and accumulate Nsum,Tsum and T2sum
C     for inner 20 short counters,Otherwise set TCOR at 0
C
            IF (I.LE.20) THEN
              IF (TCOR(K).GT.0.AND.TCOR(K).LE.200) THEN
                N(1,J)=N(1,J)+1
                TSUM(1,J)=TSUM(1,J)+TCOR(K)
                T2SUM(1,J)=T2SUM(1,J)+TCOR(K)**2
                IF (TCOR(K).GT.TMAX(1,J)) TMAX(1,J)=TCOR(K)
                IF (TCOR(K).LT.TMIN(1,J)) TMIN(1,J)=TCOR(K)
              ELSE
                TCOR(K)=0
              ENDIF
            ELSE
              IF (TCOR(K).GT.0.AND.TCOR(K).LE.200) THEN
                N(2,J)=N(2,J)+1
                TSUM(2,J)=TSUM(2,J)+TCOR(K)
                T2SUM(2,J)=T2SUM(2,J)+TCOR(K)**2
                IF (TCOR(K).GT.TMAX(1,J)) TMAX(2,J)=TCOR(K)
                IF (TCOR(K).LT.TMIN(1,J)) TMIN(2,J)=TCOR(K)
              ELSE
                TCOR(K)=0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C     Calculate corrected time,NSUM,TSUM,T2SUM,TMAX and TMIN for long counters
C
      IF ( SLV0(3).EQ.1.0 ) THEN
        DO J=1,2
          DO I=21,28
            IF (J.EQ.1) THEN
              IF (I.LE.24) THEN
                K=I
              ELSE
                K=I+4
              ENDIF
            ELSE
              IF (I.LE.24) THEN
                K=I+36
              ELSE
                K=I+40
              ENDIF
            ENDIF
C
C     If Both charge and time are in the required windows,calculate
C     the corrected time for one end of the long counter
C
            Q=FLOAT(CHARGE(K))-QPED(K)
            T=FLOAT(TIME(K))
            IF (Q.GT.Q_MIN(K).AND.CHARGE(K).LT.Q_MAX(K).
     &        AND.T.GT.T_MIN(K).AND.T.LT.T_MAX(K))  THEN
              TCOR(K)=TIME(K)*SLV0(2)
              VALID(1)=.TRUE.
            ELSE
              VALID(1)=.FALSE.
            ENDIF
C
C     If Both charge and time are in the required windows,calculate
C     the corrected time for another end of the long counter
C
            Q=FLOAT(CHARGE(K+4))-QPED(K+4)
            T=FLOAT(TIME(K+4))
            IF (Q.GT.Q_MIN(K+4).AND.CHARGE(K+4).LT.
     &        Q_MAX(K+4).AND.T.GT.T_MIN(K+4).AND.T.LT.T_MAX(K+4))  THEN
              TCOR(K+4)=TIME(K+4)*SLV0(2)
              VALID(2)=.TRUE.
            ELSE
              VALID(2)=.FALSE.
            ENDIF
C
C     If both ends have valid data, calculate the average time for the
C     long counter
C
            IF (VALID(1).AND.VALID(2)) THEN
              TCOR(K)=(TCOR(K)+TCOR(K+4)+1)/2
              TCOR(K+4)=0
            ELSE
              TCOR(K)=0
              TCOR(K+4)=0
            ENDIF
C
C     If no calibration constans available set the corrected time at 0
C
            IF ((SLOPE(K).EQ.0.0.AND.QUAD(K).EQ.0.0.AND.
     &        SLEW(K).EQ.0.0).OR.(SLOPE(K+4).EQ.0.0.AND.
     &        QUAD(K+4).EQ.0.0.AND.SLEW(K+4).EQ.0.0)) TCOR(K)=0
C
C     If the average corrected time is inside the time window, mask TCOR
C     to get 8 bits corrected time and accumulate Nsum,Tsum and T2sum
C     for long counters. Otherwise set TCOR at 0
C
            IF (TCOR(K).GT.0.AND.TCOR(K).LE.200) THEN
              N(2,J)=N(2,J)+1
              TSUM(2,J)=TSUM(2,J)+TCOR(K)
              T2SUM(2,J)=T2SUM(2,J)+TCOR(K)**2
              IF (TCOR(K).GT.TMAX(1,J)) TMAX(2,J)=TCOR(K)
              IF (TCOR(K).LT.TMIN(1,J)) TMIN(2,J)=TCOR(K)
            ELSE
              TCOR(K)=0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C     Calculate Vertex,Sigma and Multiple vertexs Flag
C
      DO I=1,2
C
C     Calculate TMAX,TMIN,TSUM,T2SUM, Including long counters information
C     if # of short counters is smaller than CUT(1),otherwise excluding
C     long counters
C
        IF (N(1,I).LT.CUT(1)) THEN
          NUMHITS=N(1,I)+N(2,I)
          SHORTY=0
          TMAX(3,I)=MAX(TMAX(1,I),TMAX(2,I))
          TMIN(3,I)=MIN(TMIN(1,I),TMIN(2,I))
          N(3,I)=N(1,I)+N(2,I)
          TSUM(3,I)=TSUM(1,I)+TSUM(2,I)
          T2SUM(3,I)=T2SUM(1,I)+T2SUM(2,I)
          TDIF=TMAX(3,I)-TMIN(3,I)
          IF (N(3,I).GE.CUT(3).AND.TDIF.GE.CUT(2)) THEN
            EXCLUSIVE=1
            N(3,I)=N(3,I)-2
            TSUM(3,I)=TSUM(3,I)-TMAX(3,I)-TMIN(3,I)
            T2SUM(3,I)=T2SUM(3,I)-TMAX(3,I)**2-TMIN(3,I)**2
            IF (N(3,I).NE.0) THEN
              T2SUM(3,I)=T2SUM(3,I)-NINT(1.0*TSUM(3,I)**2/N(3,I))
            ENDIF
          ELSE
            IF (N(3,I).NE.0) THEN
              T2SUM(3,I)=T2SUM(3,I)-NINT(1.0*TSUM(3,I)**2/N(3,I))
              EXCLUSIVE=0
            ENDIF
          ENDIF
        ELSE
          NUMHITS=N(1,I)
          SHORTY=1
          TMAX(3,I)=TMAX(1,I)
          TMIN(3,I)=TMIN(1,I)
          N(3,I)=N(1,I)
          TSUM(3,I)=TSUM(1,I)
          T2SUM(3,I)=T2SUM(1,I)
          TDIF=TMAX(3,I)-TMIN(3,I)
          IF (N(3,I).GE.CUT(3).AND.TDIF.GE.CUT(2)) THEN
            N(3,I)=N(3,I)-2
            TSUM(3,I)=TSUM(3,I)-TMAX(3,I)-TMIN(3,I)
            T2SUM(3,I)=T2SUM(3,I)-TMAX(3,I)**2-TMIN(3,I)**2
            IF (N(3,I).NE.0) THEN
              EXCLUSIVE=1
              T2SUM(3,I)=T2SUM(3,I)-NINT(1.0*TSUM(3,I)**2/N(3,I))
            ENDIF
          ELSE
            IF (N(3,I).NE.0) THEN
              T2SUM(3,I)=T2SUM(3,I)-NINT(1.0*TSUM(3,I)**2/N(3,I))
              EXCLUSIVE=0
            ENDIF
          ENDIF
        ENDIF
C
C     Calculate Tave
C
        IF (N(3,I).NE.0) THEN
          TAVE(I)=NINT(1.0*TSUM(3,I)/N(3,I))
        ELSE
          TAVE(I)=0
        ENDIF
C
C     Calculate Sigma
C
        IF (N(3,I).LE.1) THEN
          SIGMA(I)=0.0
        ELSEIF (N(3,I).EQ.2) THEN
          T2SUM(3,I)=IAND(T2SUM(3,I),'1FFFFC'X)
          SIGMA(I)=FLOAT(NINT(SQRT(1.0*T2SUM(3,I))))
        ELSEIF (N(3,I).GE.3.AND.N(3,I).LE.4) THEN
          T2SUM(3,I)=IAND(T2SUM(3,I)/2,'1FFFFC'X)
          SIGMA(I)=FLOAT(NINT(SQRT(2.0*T2SUM(3,I)/(N(3,I)-1))))
        ELSEIF (N(3,I).GE.5.AND.N(3,I).LE.8) THEN
          T2SUM(3,I)=IAND(T2SUM(3,I)/4,'1FFFFC'X)
          SIGMA(I)=FLOAT(NINT(SQRT(4.0*T2SUM(3,I)/(N(3,I)-1))))
        ELSEIF (N(3,I).GE.9.AND.N(3,I).LE.16) THEN
          T2SUM(3,I)=IAND(T2SUM(3,I)/8,'1FFFFC'X)
          SIGMA(I)=FLOAT(NINT(SQRT(8.0*T2SUM(3,I)/(N(3,I)-1))))
        ELSEIF (N(3,I).GE.17.AND.N(3,I).LE.31) THEN
          T2SUM(3,I)=IAND(T2SUM(3,I)/16,'1FFFFC'X)
          SIGMA(I)=FLOAT(NINT(SQRT(16.0*T2SUM(3,I)/(N(3,I)-1))))
        ENDIF
      ENDDO
C
C     Determine Interaction Flag and Good Z Flag
C
      INTER=0
      GOODZ=0
C
      IF (N(3,1).NE.0.AND.N(3,2).NE.0) THEN
        INTER=1
        SIGMA(3)=SQRT(SIGMA(1)**2+SIGMA(2)**2)
      ELSE
        SIGMA(3)=0.0
      ENDIF
C
      VTX=0.75*FLOAT(TAVE(1)-TAVE(2))
      IF (ABS(VTX).LT.CUT(4)) GOODZ=1
C
C     Determine multiple vertex flag
C
      IF (SIGMA(3).GE.MC(1).AND.SIGMA(3).LT.MC(2)) THEN  ! Most likely S.I.
        MI=1
        MV=1
      ENDIF
C
      IF (SIGMA(3).GE.MC(3).AND.SIGMA(3).LT.MC(4)) THEN  ! More Likely S.I.
        MI=2
        MV=2
      ENDIF
C
      IF (SIGMA(3).GE.MC(5).AND.SIGMA(3).LT.MC(6)) THEN  ! More Likely M.I.
        MI=3
        MV=4
      ENDIF
C
      IF (SIGMA(3).GE.MC(7)) THEN                        ! Most likely M.I.
        MI=4
        MV=8
      ENDIF
C
  999 RETURN
      END
