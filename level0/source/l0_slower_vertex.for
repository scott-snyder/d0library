      SUBROUTINE L0_SLOWER_VERTEX(SLOWER_Z,MI_FLAG,MI_QUALITY,
     &                INTERACTION,GOODZ,FULL_Z,CHAN_EFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Software version of the hardware VERTEX
C-                         board that includes extra processing.
C-
C-   Inputs  : none
C-   Outputs : SLOWER_Z  = Slow Z vertex position result
C-             MI_FLAG = Multiple Interaction flag :
C-                          0 = most likely single interaction
C-                          1 = likely single interaction
C-                          2 = likely multiple interaction
C-                          3 = most likely multiple interaction
C-             MI_QUALITY   = sigma value for MI_FLAG determination
C-             INTERACTION  = 0=interaction, 1=halo
C-             GOODZ        = TRUE if valid SLOWER_Z could be constructed
C-             FULL_Z       = time summation term
C-             CHAN_EFF(72) = channel correct time - average time at end
C-   Controls: none
C-
C-   Created  11-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER RAW_TIME(80)
      INTEGER BUNCH_ID(80)
      INTEGER RAW_CHARGE(80)
      INTEGER CORRECT_TIME(80)
      INTEGER NDATA
      INTEGER ZBIN
      INTEGER CBUNCH
      INTEGER END
      INTEGER N_SC(2), N_LC(2), N_CH(2)
      INTEGER CHAN, NUM_CHAN
      INTEGER MIS_FLAG, MI_FLAG
      INTEGER OFFSET, OFFSETL, ILONG
      INTEGER INTERACTION
      INTEGER CUT_A, CUT_B, CUT_C, CUT_D
      INTEGER I,J,ERR
      INTEGER IBUNCH,NCHAN,NWORD
      INTEGER IPLV0_DATA(20),VERTEX_DATA(52)
      INTEGER LINK,GZPLV0,GZL0VX_BUNCH
      EXTERNAL GZPLV0,GZL0VX_BUNCH
      INTEGER AVG_T_N,AVG_T_S,NHITS_N,NHITS_S
      INTEGER INTERACTION_BITN, INTERACTION_BITS
      INTEGER NVER, IVER
      INTEGER N_LOW(2), N_HIGH(2)
      INTEGER LOW_CHAN(36),HIGH_CHAN(36)
      INTEGER N_TIMES(36),S_TIMES(36)
      INTEGER N_OTIMES(36),S_OTIMES(36)
      INTEGER N_TIMAP(36),S_TIMAP(36)
      INTEGER N_TOT,S_TOT
      INTEGER N_NGRP,S_NGRP,N_NUMGRP(10),S_NUMGRP(10)
      INTEGER N_LAST(0:10),S_LAST(0:10)
      INTEGER MID_TIME, BELOW_MID
      INTEGER IGRP,JGRP
      INTEGER DIFFIGRP,DIFFJGRP
C
      REAL TSUM(2), T2SUM(2), TAVG(2)
      REAL MI_QUAL(2),MI_QUALITY, MIS_QUALITY
      REAL SLOW_Z
      REAL SLOWER_Z(10,10)
      REAL SLOWER_OFFZ
      REAL FULL_SZ, FULL_Z
      REAL SIGMA_N, SIGMA_S, SIGMA_TOTAL
      REAL SUM_LOW(2),SUM2_LOW(2),SUM_HIGH(2),SUM2_HIGH(2)
      REAL MEAN_LOW(2),SIGMA_LOW(2),MEAN_HIGH(2),SIGMA_HIGH(2)
      REAL L0Z(2)
      REAL T0_Z(2)
      REAL CHAN_EFF(72)
      REAL PLV0_DATA(20)
      EQUIVALENCE ( PLV0_DATA, IPLV0_DATA )
      REAL NHITS_SCN,NHITS_SCS,NHITS_LCN,NHITS_LCS,LCN_USED,LCS_USED
      REAL ZVER(5),DZVER(5)
      REAL N_MEAN(10),S_MEAN(10),N_SIGMA(10),S_SIGMA(10)
      REAL SUM,SUM2
      REAL DIFFZ,CUT_Z1,CUT_Z2
C
      LOGICAL GOODSZ, GOODZ
      LOGICAL FIRST, PRODFL
      LOGICAL DO_NORTH, DO_SOUTH
      LOGICAL FULL_PLV0
      LOGICAL USE_CHAN(72)
      LOGICAL EZERROR
      EXTERNAL EZERROR
C
      SAVE FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0_SLOWER_VERTEX',
     &                              'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('L0Z',L0Z,ERR)
          CALL EZGET('SLOWER_OFFZ',SLOWER_OFFZ,ERR)
          CALL EZGET('CUT_A',CUT_A,ERR)
          CALL EZGET('CUT_B',CUT_B,ERR)
          CALL EZGET('CUT_C',CUT_C,ERR)
          CALL EZGET('CUT_D',CUT_D,ERR)
          CALL EZGET('CUT_Z1',CUT_Z1,ERR)
          CALL EZGET('CUT_Z2',CUT_Z2,ERR)
          CALL EZRSET
        ENDIF
C
        FIRST = .FALSE.
      ENDIF
C
      CALL VZERO(SLOWER_Z,100)
      MI_FLAG=0
      MIS_FLAG=0
      MI_QUALITY=0.0
      INTERACTION=0
      GOODZ=.FALSE.
      FULL_Z=0.0
      CALL VZERO(CHAN_EFF,72)
      CALL VZERO(N_TIMES,36)
      CALL VZERO(S_TIMES,36)
      CALL VZERO(N_OTIMES,36)
      CALL VZERO(S_OTIMES,36)
      CALL VZERO(N_TIMAP,36)
      CALL VZERO(S_TIMAP,36)
      N_TOT=0
      S_TOT=0
      N_NGRP=0
      S_NGRP=0
      CALL VZERO(N_NUMGRP,10)
      CALL VZERO(S_NUMGRP,10)
      CALL VZERO(N_LAST,10)
      CALL VZERO(S_LAST,10)
      CALL VZERO(N_MEAN,10)
      CALL VZERO(S_MEAN,10)
      CALL VZERO(N_SIGMA,10)
      CALL VZERO(S_SIGMA,10)
C
C  fetch location in TRGR bank of L0 crate, crate 01
C
      CALL L0_COR_BUNCH(CBUNCH)
C
C
C  Check if data in PLV0 bank.
C
      FULL_PLV0 = .FALSE.
      LINK=GZPLV0()
      IF ( LINK.GT.0 ) THEN
        IF ( IBITS(IQ(LINK+1),7,3).GT.0 ) THEN
          FULL_PLV0 = .TRUE.
          CALL GTPLV0(IPLV0_DATA)
          SLOWER_Z(1,1)=PLV0_DATA(5)
          IF ( BTEST(IPLV0_DATA(1),1) ) THEN
            MIS_FLAG=1
          ELSEIF ( BTEST(IPLV0_DATA(1),2) ) THEN
            MIS_FLAG=2
          ELSEIF ( BTEST(IPLV0_DATA(1),4) ) THEN
            MIS_FLAG=4
          ELSEIF ( BTEST(IPLV0_DATA(1),3) ) THEN
            MIS_FLAG=3
          ENDIF
          MIS_QUALITY=PLV0_DATA(6)
          LINK=GZL0VX_BUNCH(CBUNCH)
          IF ( LINK.GT.0 ) THEN
            CALL GTL0VX(CBUNCH,VERTEX_DATA)
            IF ( BTEST(VERTEX_DATA(26),0) ) INTERACTION=1
            TAVG(1) = VERTEX_DATA(15)
            TAVG(2) = VERTEX_DATA(41)
C            FULL_SZ = (-2./30.0)*(SLOWER_Z(1,1)+SLOWER_Z(1,1)-SLOWER_OFFZ)
          ENDIF
          IF ( BTEST(IPLV0_DATA(1),6) ) THEN
            GOODSZ = .TRUE.
            GOTO 100
          ENDIF
        ENDIF
        IF ( IBITS(IQ(LINK+1),7,3).GT.0 ) THEN
          FULL_PLV0 = .TRUE.
          CALL GTPLV0(IPLV0_DATA)
          SLOW_Z=PLV0_DATA(3)
          IF ( BTEST(IPLV0_DATA(1),12) ) INTERACTION = 1
          IF ( BTEST(IPLV0_DATA(1),1) ) THEN
            MIS_FLAG=1
          ELSEIF ( BTEST(IPLV0_DATA(1),2) ) THEN
            MIS_FLAG=2
          ELSEIF ( BTEST(IPLV0_DATA(1),4) ) THEN
            MIS_FLAG=4
          ELSEIF ( BTEST(IPLV0_DATA(1),3) ) THEN
            MIS_FLAG=3
          ELSE
            IF ( INTERACTION.EQ.1 ) THEN
              IF ( .NOT.PRODFL ) CALL ERRMSG('LEVEL0-NO-MIFLAG-LSV',
     &                'L0_SLOWER_VERTEX','NO MIFLAG FD','W')
            ENDIF
          ENDIF
          MI_QUALITY=PLV0_DATA(4)
          IF ( BTEST(IPLV0_DATA(1),5) ) GOODZ = .TRUE.
        ENDIF
      ENDIF
C
C  Assemble vertex board information.
C
  100 CONTINUE
      CALL GTL0VX(CBUNCH,VERTEX_DATA)
      IF ( VERTEX_DATA(3).EQ.1 ) THEN
        INTERACTION_BITN = VERTEX_DATA(21)
        INTERACTION_BITS = VERTEX_DATA(47)
        SIGMA_N = VERTEX_DATA(14)
        SIGMA_S = VERTEX_DATA(40)
        AVG_T_N = VERTEX_DATA(15)
        AVG_T_S = VERTEX_DATA(41)
        NHITS_N = VERTEX_DATA(16)
        NHITS_S = VERTEX_DATA(42)
        NHITS_SCN = VERTEX_DATA(5)
        NHITS_SCS = VERTEX_DATA(31)
        NHITS_LCN = VERTEX_DATA(10)
        NHITS_LCS = VERTEX_DATA(36)
        LCN_USED = 1-VERTEX_DATA(18)
        LCS_USED = 1-VERTEX_DATA(44)
      ELSE
        INTERACTION_BITS = VERTEX_DATA(21)
        INTERACTION_BITN = VERTEX_DATA(47)
        SIGMA_S = VERTEX_DATA(14)
        SIGMA_N = VERTEX_DATA(40)
        AVG_T_S = VERTEX_DATA(15)
        AVG_T_N = VERTEX_DATA(41)
        NHITS_S = VERTEX_DATA(16)
        NHITS_N = VERTEX_DATA(42)
        NHITS_SCN = VERTEX_DATA(31)
        NHITS_SCS = VERTEX_DATA(5)
        NHITS_LCN = VERTEX_DATA(36)
        NHITS_LCS = VERTEX_DATA(10)
        LCN_USED = 1-VERTEX_DATA(44)
        LCS_USED = 1-VERTEX_DATA(18)
      ENDIF
      SIGMA_TOTAL = SQRT( SIGMA_N**2. + SIGMA_S**2. )
      DO_NORTH = .FALSE.
      DO_SOUTH = .FALSE.
C
      IF ( MIS_FLAG.EQ.2 ) THEN
        IF ( SIGMA_S .GT. 0.0 ) THEN
          IF ( SIGMA_N / SIGMA_S .GT. 2.0 ) DO_NORTH = .TRUE.
        ENDIF
        IF ( SIGMA_N .GT. 0.0 ) THEN
          IF ( SIGMA_S / SIGMA_N .GT. 2.0 ) DO_SOUTH = .TRUE.
        ENDIF
      ELSEIF ( MIS_FLAG.EQ.3 ) THEN
        IF ( SIGMA_S .GT. 0.0 ) THEN
          IF ( SIGMA_N / SIGMA_S .GT. 2.0 ) DO_NORTH = .TRUE.
        ENDIF
        IF ( SIGMA_N .GT. 0.0 ) THEN
          IF ( SIGMA_S / SIGMA_N .GT. 2.0 ) DO_SOUTH = .TRUE.
        ENDIF
      ELSEIF ( MIS_FLAG.EQ.4 ) THEN
        IF ( SIGMA_S .GT. 0.0 ) THEN
          IF ( SIGMA_N / SIGMA_S .GT. 3.0 ) DO_NORTH = .TRUE.
        ENDIF
        IF ( SIGMA_N .GT. 0.0 ) THEN
          IF ( SIGMA_S / SIGMA_N .GT. 3.0 ) DO_SOUTH = .TRUE.
        ENDIF
      ENDIF
C
C  Assemble local corrected times values.
C
      CALL VZERO(USE_CHAN,72)
      CALL VZERO(N_SC,2)
      CALL VZERO(N_LC,2)
      CALL VZERO(N_CH,2)
      CALL VZERO(TSUM,2)
      CALL VZERO(T2SUM,2)
      CALL VZERO(TAVG,2)
C
      CALL GTL0AD(CBUNCH,IBUNCH,NCHAN,NWORD,RAW_TIME,BUNCH_ID,
     &                                        RAW_CHARGE,CORRECT_TIME)
C
      DO END=1,2                      ! N and S ends
        OFFSET = (END-1)*36
        N_SC(END)=0
        DO CHAN=1+OFFSET,20+OFFSET
          IF ( CORRECT_TIME(CHAN).GT.0.0 ) THEN
            N_SC(END) = N_SC(END) + 1
            IF(END.EQ.1) N_TIMES(N_SC(END))=CORRECT_TIME(CHAN)
            IF(END.EQ.2) S_TIMES(N_SC(END))=CORRECT_TIME(CHAN)
            USE_CHAN(CHAN) = .TRUE.
          ENDIF
        ENDDO
C
        IF ( N_SC(END).LT.CUT_A ) THEN
          DO ILONG=1,2
            OFFSETL = OFFSET + (ILONG-1)*8
C
            DO CHAN=21+OFFSETL,24+OFFSETL
              IF ( CORRECT_TIME(CHAN).GT.0.0 ) THEN
                N_LC(END) = N_LC(END) + 1
                IF(END.EQ.1) N_TIMES(N_LC(END)+N_SC(END)) =
     &            CORRECT_TIME(CHAN)
                IF(END.EQ.2) S_TIMES(N_LC(END)+N_SC(END)) =
     &            CORRECT_TIME(CHAN)
                USE_CHAN(CHAN) = .TRUE.
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
        IF (END.EQ.1) N_TOT=N_SC(END)+N_LC(END)
        IF (END.EQ.2) S_TOT=N_SC(END)+N_LC(END)
        IF ( N_SC(END)+N_LC(END) .LE. 0 ) THEN
          GOTO 990
        ENDIF
C
      ENDDO
C
C ****
C
      DO I=1,N_TOT
        N_TIMAP(I)=I
        N_OTIMES(I)=N_TIMES(I)
      ENDDO
      DO I=1,S_TOT
        S_TIMAP(I)=I
        S_OTIMES(I)=S_TIMES(I)
      ENDDO
      CALL SRTINT(N_TIMES,N_TOT,N_TIMAP)
      CALL SRTINT(S_TIMES,S_TOT,S_TIMAP)
C
C ****
C
  401 CONTINUE
      N_NGRP=0
c      IF ( (DO_NORTH) ) THEN
      IF ( N_TOT.LT.CUT_B ) GOTO 402
      N_LAST(0)=0
      DO I=1,N_TOT-1
        IF ( N_TIMES(I+1)-N_TIMES(I).GT.CUT_C ) THEN
          IF ( N_TIMES(I)-N_TIMES(N_LAST(N_NGRP)+1).GT.CUT_D ) THEN
            MID_TIME = ( N_TIMES(I)+N_TIMES(N_LAST(N_NGRP)+1) ) / 2.0
            DO J=N_LAST(N_NGRP)+1,I
              IF ( N_TIMES(J).LE.MID_TIME ) BELOW_MID = J
            ENDDO
            N_NGRP=N_NGRP+1
            N_LAST(N_NGRP)=BELOW_MID
            N_NGRP=N_NGRP+1
            N_LAST(N_NGRP)=I
          ELSE
            N_NGRP=N_NGRP+1
            N_LAST(N_NGRP)=I
          ENDIF
        ENDIF
      ENDDO
      IF ( N_TIMES(N_TOT)-N_TIMES(N_LAST(N_NGRP)+1).GT.CUT_D ) THEN
        MID_TIME = ( N_TIMES(N_TOT)+N_TIMES(N_LAST(N_NGRP)+1) ) / 2.0
        DO J=N_LAST(N_NGRP)+1,N_TOT
          IF ( N_TIMES(J).LE.MID_TIME ) BELOW_MID = J
        ENDDO
        N_NGRP=N_NGRP+1
        N_LAST(N_NGRP)=BELOW_MID
        N_NGRP=N_NGRP+1
        N_LAST(N_NGRP)=N_TOT
      ELSE
        N_NGRP=N_NGRP+1
        N_LAST(N_NGRP)=N_TOT
      ENDIF
C
      DO IGRP=1,N_NGRP
        N_NUMGRP(IGRP) = 0
        SUM = 0
        SUM2 = 0
        DO I=N_LAST(IGRP-1)+1,N_LAST(IGRP)
          SUM  = SUM + N_TIMES(I)
          SUM2 = SUM2 + (N_TIMES(I)**2.)
          N_NUMGRP(IGRP) = N_NUMGRP(IGRP) + 1
        ENDDO
        IF ( n_numgrp(igrp).GT.0 ) THEN
          N_MEAN(IGRP)   = SUM / N_NUMGRP(IGRP)
          N_SIGMA(IGRP)  = SQRT(SUM2/N_NUMGRP(IGRP) -
     &                                        (N_MEAN(IGRP)**2.))
        ENDIF
      ENDDO
c      ENDIF
C
  402 CONTINUE
      S_NGRP=0
c      IF ( (DO_SOUTH) ) THEN
      IF ( S_TOT.LT.CUT_B ) GOTO 403
      S_LAST(0)=0
      DO I=1,S_TOT-1
        IF ( S_TIMES(I+1)-S_TIMES(I).GT.CUT_C ) THEN
          IF ( S_TIMES(I)-S_TIMES(S_LAST(S_NGRP)+1).GT.CUT_D ) THEN
            MID_TIME = ( S_TIMES(I)+S_TIMES(S_LAST(S_NGRP)+1) ) / 2.0
            DO J=S_LAST(S_NGRP)+1,I
              IF ( S_TIMES(J).LE.MID_TIME ) BELOW_MID = J
            ENDDO
            S_NGRP=S_NGRP+1
            S_LAST(S_NGRP)=BELOW_MID
            S_NGRP=S_NGRP+1
            S_LAST(S_NGRP)=I
          ELSE
            S_NGRP=S_NGRP+1
            S_LAST(S_NGRP)=I
          ENDIF
        ENDIF
      ENDDO
      IF ( S_TIMES(S_TOT)-S_TIMES(S_LAST(S_NGRP)+1).GT.CUT_D ) THEN
        MID_TIME = ( S_TIMES(S_TOT)+S_TIMES(S_LAST(S_NGRP)+1) ) / 2.0
        DO J=S_LAST(S_NGRP)+1,S_TOT
          IF ( S_TIMES(J).LE.MID_TIME ) BELOW_MID = J
        ENDDO
        S_NGRP=S_NGRP+1
        S_LAST(S_NGRP)=BELOW_MID
        S_NGRP=S_NGRP+1
        S_LAST(S_NGRP)=S_TOT
      ELSE
        S_NGRP=S_NGRP+1
        S_LAST(S_NGRP)=S_TOT
      ENDIF
C
      DO IGRP=1,S_NGRP
        S_NUMGRP(IGRP) = 0
        SUM = 0
        SUM2 = 0
        DO I=S_LAST(IGRP-1)+1,S_LAST(IGRP)
          SUM  = SUM + S_TIMES(I)
          SUM2 = SUM2 + (S_TIMES(I)**2.)
          S_NUMGRP(IGRP) = S_NUMGRP(IGRP) + 1
        ENDDO
        IF ( s_numgrp(igrp).GT.0 ) THEN
          S_MEAN(IGRP)   = SUM / S_NUMGRP(IGRP)
          S_SIGMA(IGRP)  = SQRT(SUM2/S_NUMGRP(IGRP) -
     &                                        (S_MEAN(IGRP)**2.))
        ENDIF
      ENDDO
c      ENDIF
C
  403 CONTINUE
C
  500 CONTINUE
C
C   Fold all conversion factors into single constant.
C   0.45 = 3.0E+10 cm/sec * 1.0E-09 sec/nsec
C
      IF ( N_NGRP.GT.7 ) N_NGRP=7
      IF ( S_NGRP.GT.7 ) S_NGRP=7
      IF ( N_NGRP.LT.10 ) THEN
        N_NGRP=N_NGRP+1
        N_MEAN(N_NGRP)=AVG_T_N
      ENDIF
      IF ( S_NGRP.LT.10 ) THEN
        S_NGRP=S_NGRP+1
        S_MEAN(S_NGRP)=AVG_T_S
      ENDIF
      DO IGRP=1,N_NGRP-1
        DO JGRP=1,S_NGRP-1
          SLOWER_Z(IGRP,JGRP) = 0.75 * ( N_MEAN(IGRP)-S_MEAN(JGRP) )
        ENDDO
      ENDDO
      IGRP=N_NGRP
      DO JGRP=1,S_NGRP-1
        SLOWER_Z(IGRP,JGRP) = 0.75 * ( N_MEAN(IGRP)-S_MEAN(JGRP) )
      ENDDO
      JGRP=S_NGRP
      DO IGRP=1,N_NGRP-1
        SLOWER_Z(IGRP,JGRP) = 0.75 * ( N_MEAN(IGRP)-S_MEAN(JGRP) )
      ENDDO
      IGRP=N_NGRP
      JGRP=S_NGRP
      SLOWER_Z(IGRP,JGRP) = 0.75 * ( N_MEAN(IGRP)-S_MEAN(JGRP) )
C
      NVER=0
      CALL VZERO(ZVER,5)
      CALL ZVERTE(NVER,ZVER,DZVER)
C
      DO IVER=1,NVER
C
        DIFFZ = 100.0
        DIFFIGRP=0
        DIFFJGRP=0
        DO IGRP=1,N_NGRP-1
          DO JGRP=1,S_NGRP-1
            IF ( ABS(SLOWER_Z(IGRP,JGRP)-ZVER(IVER)) .LT. DIFFZ ) THEN
              DIFFZ=ABS(SLOWER_Z(IGRP,JGRP)-ZVER(IVER))
              DIFFIGRP=IGRP
              DIFFJGRP=JGRP
            ENDIF
          ENDDO
        ENDDO
        IF ( DIFFZ.GT.CUT_Z1 ) THEN
          IGRP=N_NGRP
          DO JGRP=1,S_NGRP-1
            IF ( ABS(SLOWER_Z(IGRP,JGRP)-ZVER(IVER)) .LT. DIFFZ ) THEN
              DIFFZ=ABS(SLOWER_Z(IGRP,JGRP)-ZVER(IVER))
              DIFFIGRP=IGRP
              DIFFJGRP=JGRP
            ENDIF
          ENDDO
          JGRP=S_NGRP
          DO IGRP=1,N_NGRP-1
            IF ( ABS(SLOWER_Z(IGRP,JGRP)-ZVER(IVER)) .LT. DIFFZ ) THEN
              DIFFZ=ABS(SLOWER_Z(IGRP,JGRP)-ZVER(IVER))
              DIFFIGRP=IGRP
              DIFFJGRP=JGRP
            ENDIF
          ENDDO
        ENDIF
        IF ( DIFFZ.LT.CUT_Z2 ) THEN
          SLOWER_Z(IVER,10)  = SLOWER_Z(DIFFIGRP,DIFFJGRP)
          SLOWER_Z(IVER,9)   = SLOWER_Z(DIFFIGRP,DIFFJGRP)-ZVER(IVER)
          SLOWER_Z(IVER+3,10)= FLOAT(DIFFIGRP)
          SLOWER_Z(IVER+3,9) = FLOAT(DIFFJGRP)
          SLOWER_Z(10,IVER)  = FLOAT(N_NUMGRP(DIFFIGRP))
          SLOWER_Z(9,IVER)   = FLOAT(S_NUMGRP(DIFFJGRP))
          SLOWER_Z(10,IVER+3)= 0.0
          SLOWER_Z(9,IVER+3) = 0.0
        ENDIF
C
      ENDDO
C
      GOODZ = .TRUE.
C
C  Fill array with channel differences.
C
C  980 CONTINUE
C      CALL VZERO(CHAN_EFF,72)
C      DO CHAN=1,20
C        IF (USE_CHAN(CHAN).EQ.1 ) CHAN_EFF(CHAN) =
C     &                                  CORRECT_TIME(CHAN)-TAVG(1)
C      ENDDO
C      DO CHAN=37,56
C        IF (USE_CHAN(CHAN).EQ.1 ) CHAN_EFF(CHAN) =
C     &                                  CORRECT_TIME(CHAN)-TAVG(2)
C      ENDDO
C
C  Done.
C
      GOTO 999
C
C  Bad Z vertex from slow vertex finding.
C
  990 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
