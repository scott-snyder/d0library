      SUBROUTINE L0_SLOW_CHECK(HWORDS,SWORDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Software version of the hardware VERTEX
C-                         board.
C-
C-   Inputs  : none
C-   Outputs : HWORDS(1-26) = output from the North Slow Vertex board
C-             SWORDS(1-26) = output from software Slow Z simulation,
C-                                   the North end only
C-   Controls: none
C-
C-   Created  13-AUG-1992   Jeffrey Bantly
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
      INTEGER CHAN
      INTEGER USE_CHAN(72)
      INTEGER MIN_TIME, MAX_TIME
      INTEGER MIN_CHAN, MAX_CHAN
      INTEGER MI_FLAG
      INTEGER OFFSET, OFFSETL, ILONG
      INTEGER INTERACTION
      INTEGER CUT_1, CUT_2,CUT_3
      INTEGER I,ERR
      INTEGER IBUNCH,NCHAN,NWORD
      INTEGER HWORDS(52),SWORDS(26)
      INTEGER PRUNIT,IVERS,K,FASTZ_ID,VERTEX_BOARD
C
      REAL TSUM(2), T2SUM(2), TAVG(2)
      REAL MI_QUAL(2),MI_QUALITY
      REAL CUT_4
      REAL SLOW_Z
      REAL SLOW_OFFZ
      REAL FULL_Z
      REAL PEDSIG_TMUL,PEDSIG_QMUL
      REAL PED_ARRAY(5,72)
      REAL CALIB_ARRAY(5,72)
      REAL CHG_MINCUT(72), CHG_MAXCUT(72)
      REAL TIM_MINCUT(72), TIM_MAXCUT(72)
      REAL CHGPED(72)
      REAL COR_TIME(72)
      REAL CLB_T0(72)
      REAL CLB_SLOPE(72)
      REAL CLB_QUAD(72)
      REAL CLB_SLEW(72)
      REAL RTDC,RADC
      REAL L0Z(2)
      REAL T0_Z(2)
      REAL CHAN_EFF(72)
C
      LOGICAL GOODFZ, GOODSZ, FIRST
      LOGICAL DBG_SLOWZ
      LOGICAL EZERROR
      EXTERNAL EZERROR
C
      SAVE FIRST,CHG_MINCUT,CHG_MAXCUT,TIM_MINCUT,TIM_MAXCUT,DBG_SLOWZ
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','LV0HIS',
     &                              'LEVEL0_RCP not found.','W')
        ELSE
          CALL VZERO(PED_ARRAY,360)
          CALL VZERO(CALIB_ARRAY,360)
          CALL EZGET('PEDSIG_TMUL',PEDSIG_TMUL,ERR)
          CALL EZGET('PEDSIG_QMUL',PEDSIG_QMUL,ERR)
          CALL EZGET('PED_ARRAY(1)',PED_ARRAY(1,1),ERR)
          CALL EZGET('CALIB_ARRAY(1)',CALIB_ARRAY(1,1),ERR)
          CALL EZGET('L0Z',L0Z,ERR)
          CALL EZGET('SLOW_OFFZ',SLOW_OFFZ,ERR)
          CALL EZGET('DBG_SLOWZ',DBG_SLOWZ,ERR)
          CALL EZGET('CUT_1',CUT_1,ERR)
          CALL EZGET('CUT_2',CUT_2,ERR)
          CALL EZGET('CUT_3',CUT_3,ERR)
          CALL EZGET('CUT_4',CUT_4,ERR)
          CALL EZRSET
        ENDIF
        CALL GTUNIT(684,PRUNIT,ERR)
C
        DO CHAN=1,72
          CHG_MINCUT(CHAN) =  60
          CHG_MAXCUT(CHAN) = 950
          TIM_MINCUT(CHAN) = 300
          TIM_MAXCUT(CHAN) = 950
        ENDDO
        DO I=1,72
          CHAN = INT(PED_ARRAY(1,I))
          IF ( CHAN.GT.0 .AND. CHAN.LE.72 ) THEN
            TIM_MAXCUT(CHAN) = PED_ARRAY(2,I)-PEDSIG_TMUL*PED_ARRAY(3,I)
            CHG_MINCUT(CHAN) = PED_ARRAY(4,I)+PEDSIG_QMUL*PED_ARRAY(5,I)
            IF ( CHG_MINCUT(CHAN).LE.0 ) CHG_MINCUT(CHAN)=5
            CHGPED(CHAN) = PED_ARRAY(4,I)
          ENDIF
        ENDDO
        CALL VZERO(CLB_T0,72)
        CALL VFILL(CLB_SLOPE,72,1.)
        CALL VZERO(CLB_QUAD,72)
        CALL VZERO(CLB_SLEW,72)
        DO I=1,72
          CHAN = INT(CALIB_ARRAY(1,I))
          IF ( CHAN.GT.0 .AND. CHAN.LE.72 ) THEN
            CLB_T0(CHAN)    = CALIB_ARRAY(2,I)
            CLB_SLOPE(CHAN) = CALIB_ARRAY(3,I)
            CLB_QUAD(CHAN)  = CALIB_ARRAY(4,I)
            CLB_SLEW(CHAN)  = CALIB_ARRAY(5,I)
          ENDIF
        ENDDO
C
        FIRST = .FALSE.
      ENDIF
C
      CALL VZERO(HWORDS,26)
      CALL VZERO(SWORDS,26)
C
C  Fetch the FASTZ info for software answer
C
      CALL L2_L0_VERTEX(ZBIN,GOODFZ)
C
C  Fetch the correct bunch number
C
      CALL L0_COR_BUNCH(CBUNCH)
C
      CALL GTL0AD(CBUNCH,IBUNCH,NCHAN,NWORD,RAW_TIME,BUNCH_ID,
     &                                        RAW_CHARGE,CORRECT_TIME)
C
      CALL VZERO(USE_CHAN,72)
      CALL VZERO(COR_TIME,72)
      CALL VZERO(N_SC,2)
      CALL VZERO(N_LC,2)
      CALL VZERO(N_CH,2)
      CALL VZERO(TSUM,2)
      CALL VZERO(T2SUM,2)
      CALL VZERO(TAVG,2)
C
      DO END=1,2                      ! N and S ends
        OFFSET = (END-1)*36
        N_SC(END)=0
        DO CHAN=1+OFFSET,20+OFFSET
          IF ( RAW_CHARGE(CHAN).GE.CHG_MINCUT(CHAN) ) THEN
            IF ( RAW_CHARGE(CHAN).LE.CHG_MAXCUT(CHAN) ) THEN
              IF ( RAW_TIME(CHAN).GE.TIM_MINCUT(CHAN) ) THEN
                IF ( RAW_TIME(CHAN).LE.TIM_MAXCUT(CHAN) ) THEN
                  RTDC = FLOAT(RAW_TIME(CHAN))
                  RADC = FLOAT(RAW_CHARGE(CHAN))-CHGPED(CHAN)
                  IF ( RADC.LE.0 ) RADC=1.
                  COR_TIME(CHAN) = CLB_T0(CHAN) +
     &              (CLB_SLOPE(CHAN)*RTDC) +
     &              (CLB_QUAD(CHAN)*RTDC*RTDC) +
     &              (CLB_SLEW(CHAN)/SQRT(RADC))
                  N_SC(END) = N_SC(END) + 1
                  USE_CHAN(CHAN) = 1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF ( END.EQ.0 ) THEN
          SWORDS(5)=N_SC(END)
          SWORDS(17)=0
          SWORDS(18)=1
        ENDIF
C
        IF ( N_SC(END).LT. CUT_1) THEN
C
          DO ILONG=1,2
            OFFSETL = OFFSET + (ILONG-1)*8
C
            DO CHAN=21+OFFSETL,24+OFFSETL
              IF ( RAW_CHARGE(CHAN).GE.CHG_MINCUT(CHAN) ) THEN
                IF ( RAW_CHARGE(CHAN).LE.CHG_MAXCUT(CHAN) ) THEN
                  IF ( RAW_TIME(CHAN).GE.TIM_MINCUT(CHAN) ) THEN
                    IF ( RAW_TIME(CHAN).LE.TIM_MAXCUT(CHAN) )
     &                THEN
                      IF ( RAW_TIME(CHAN+4).GE.TIM_MINCUT(CHAN+4)
     &                  ) THEN
                        IF ( RAW_TIME(CHAN+4).LE.
     &                    TIM_MAXCUT(CHAN+4)  ) THEN
                          RTDC = FLOAT(RAW_TIME(CHAN))
                          RADC = FLOAT(RAW_CHARGE(CHAN))
     &                      -CHGPED(CHAN)
                          IF ( RADC.LE.0 ) RADC=1.
                          COR_TIME(CHAN) = CLB_T0(CHAN) +
     &                      (CLB_SLOPE(CHAN)*RTDC) +
     &                      (CLB_QUAD(CHAN)*RTDC*RTDC) +
     &                      (CLB_SLEW(CHAN)/SQRT(RADC))
                          RTDC = RAW_TIME(CHAN+4)
                          RADC = FLOAT(RAW_CHARGE(CHAN+4))
     &                      -CHGPED(CHAN+4)
                          IF ( RADC.LE.0 ) RADC=1.
                          COR_TIME(CHAN+4) = CLB_T0(CHAN+4) +
     &                      (CLB_SLOPE(CHAN+4)*RTDC) +
     &                      (CLB_QUAD(CHAN+4)*RTDC*RTDC) +
     &                      (CLB_SLEW(CHAN+4)/SQRT(RADC))
                          COR_TIME(CHAN) = (COR_TIME(CHAN) +
     &                      COR_TIME(CHAN+4))/2.
                          COR_TIME(CHAN+4) = COR_TIME(CHAN)
                          N_LC(END) = N_LC(END) + 1
                          USE_CHAN(CHAN) = 1
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
C
          IF ( N_SC(END)+N_LC(END) .LE. 0 ) THEN
            GOTO 990
          ENDIF
          IF ( END.EQ.0 ) THEN
            SWORDS(10)=N_LC(END)
            SWORDS(17)=0
            SWORDS(18)=0
          ENDIF
C
        ELSEIF ( N_SC(END).GE. CUT_2 ) THEN
          MIN_CHAN = 0
          MAX_CHAN = 0
          MIN_TIME = 9999999.
          MAX_TIME = -999999.
          DO CHAN=1+OFFSET,20+OFFSET
            IF (USE_CHAN(CHAN).EQ.1) THEN
              IF ( COR_TIME(CHAN).LE.MIN_TIME ) THEN
                MIN_TIME = COR_TIME(CHAN)
                MIN_CHAN = CHAN
              ENDIF
              IF ( COR_TIME(CHAN).GE.MAX_TIME ) THEN
                MAX_TIME = COR_TIME(CHAN)
                MAX_CHAN = CHAN
              ENDIF
            ENDIF
          ENDDO
          IF ( MIN_CHAN.GT.0 ) THEN
            USE_CHAN(MIN_CHAN) = 0
            N_SC(END) = N_SC(END) - 1
          ENDIF
          IF ( MAX_CHAN.GT.0 ) THEN
            USE_CHAN(MAX_CHAN) = 0
            N_SC(END) = N_SC(END) - 1
          ENDIF
          IF ( END.EQ.0 ) THEN
C??            SWORDS(5)=N_SC(END)
            SWORDS(6)=MIN_TIME
            SWORDS(7)=MAX_TIME
            SWORDS(17)=1
            SWORDS(18)=1
          ENDIF
        ENDIF
C
        N_CH(END) = 0
        DO CHAN=1+OFFSET,20+OFFSET
          IF ( USE_CHAN(CHAN).EQ.1 ) THEN
            TSUM(END) = TSUM(END) + COR_TIME(CHAN)
            T2SUM(END) = T2SUM(END) + (COR_TIME(CHAN)**2.)
            N_CH(END) = N_CH(END) + 1
          ENDIF
        ENDDO
        IF ( END.EQ.0 ) THEN
          SWORDS(4)=T2SUM(END)
          SWORDS(8)=TSUM(END)
        ENDIF
        DO CHAN=21+OFFSET,36+OFFSET
          IF ( USE_CHAN(CHAN).EQ.1 ) THEN
            TSUM(END) = TSUM(END) + COR_TIME(CHAN)
            T2SUM(END) = T2SUM(END) + (COR_TIME(CHAN)**2.)
            N_CH(END) = N_CH(END) + 1
          ENDIF
        ENDDO
        IF ( END.EQ.0 ) THEN
          SWORDS(9)=T2SUM(END)-SWORDS(4)
          SWORDS(13)=TSUM(END)-SWORDS(8)
        ENDIF
C
        IF ( N_CH(END).LT.CUT_3 ) THEN
          GOTO 990
        ELSE
          TAVG(END) = TSUM(END) / N_CH(END)
          IF ( END.EQ.0 ) SWORDS(15)=TAVG(END)
        ENDIF
C
      ENDDO
C
C   Fold all conversion factors into single constant.
C   0.45 = 3.0E+10 cm/sec * 1.0E-09 sec/nsec
C
      SLOW_Z = 30.0 * (TAVG(1)-TAVG(2)) / 2.
      SLOW_Z = SLOW_Z + SLOW_OFFZ
      T0_Z(1) = ABS(L0Z(1)-SLOW_Z)/30.0 - TAVG(1)
      T0_Z(2) = ABS(L0Z(2)-SLOW_Z)/30.0 - TAVG(2)
      FULL_Z = T0_Z(1)-T0_Z(2)
C
      IF ( ABS(SLOW_Z) .GT. CUT_4 ) GOTO 990
C
      DO END=1,2
        MI_QUAL(END) = ( 1. / (float(N_CH(END))-1.) ) *
     &    (  T2SUM(END) - ( (1./float(N_CH(END)))*(TSUM(END)**2.) )  )
      ENDDO
      IF ( END.EQ.0 ) SWORDS(14)=MI_QUAL(END)
      MI_QUALITY = (MI_QUAL(1)**2. + MI_QUAL(2)**2.)**0.5
      MI_QUALITY = MI_QUALITY * 30.0
      IF ( MI_QUALITY .LT. 150.0 ) THEN
        MI_FLAG = 0
      ELSEIF ( MI_QUALITY .LT. 250.0 ) THEN
        MI_FLAG = 1
      ELSEIF ( MI_QUALITY .LT. 400.0 ) THEN
        MI_FLAG = 2
      ELSE
        MI_FLAG = 3
      ENDIF
      SWORDS(19) = SLOW_Z
      IF ( GOODFZ ) SWORDS(20) = ZBIN
      SWORDS(26-MI_FLAG)=1
      SWORDS(22)=1
C
C  Fill array with channel differences.
C
      CALL VZERO(CHAN_EFF,72)
      DO CHAN=1,20
        IF (USE_CHAN(CHAN).EQ.1 ) CHAN_EFF(CHAN)=COR_TIME(CHAN)-TAVG(1)
      ENDDO
      DO CHAN=37,56
        IF (USE_CHAN(CHAN).EQ.1 ) CHAN_EFF(CHAN)=COR_TIME(CHAN)-TAVG(2)
      ENDDO
C
C  Done making calculations from raw data.
C
      GOODSZ = .TRUE.
C
C  Fetch hardware solution to slow vertex and compare.
C
      CALL GTL0VX(CBUNCH,HWORDS)
C
      IF ( DBG_SLOWZ ) THEN
      IBUNCH=HWORDS(1)
      FASTZ_ID=HWORDS(2)
      VERTEX_BOARD=HWORDS(3)
      WRITE(PRUNIT,101) IVERS, IBUNCH, FASTZ_ID, VERTEX_BOARD
      WRITE(PRUNIT,199)
      WRITE(PRUNIT,200)
      WRITE(PRUNIT,201) HWORDS(4),HWORDS(9)
      WRITE(PRUNIT,201) SWORDS(4),SWORDS(9)
      WRITE(PRUNIT,202) HWORDS(8),HWORDS(13)
      WRITE(PRUNIT,202) SWORDS(8),SWORDS(13)
      WRITE(PRUNIT,203) HWORDS(6),HWORDS(11)
      WRITE(PRUNIT,203) SWORDS(6),SWORDS(11)
      WRITE(PRUNIT,204) HWORDS(7),HWORDS(12)
      WRITE(PRUNIT,204) SWORDS(7),SWORDS(12)
      WRITE(PRUNIT,205) HWORDS(5)
      WRITE(PRUNIT,205) SWORDS(5)
      WRITE(PRUNIT,205) HWORDS(10)
      WRITE(PRUNIT,205) SWORDS(10)
      WRITE(PRUNIT,207) HWORDS(15), HWORDS(14)
      WRITE(PRUNIT,207) SWORDS(15), SWORDS(14)
      WRITE(PRUNIT,208) HWORDS(16)
      WRITE(PRUNIT,208) SWORDS(16)
      WRITE(PRUNIT,209) HWORDS(17), HWORDS(18)
      WRITE(PRUNIT,209) SWORDS(17), SWORDS(18)
      WRITE(PRUNIT,210) HWORDS(19), HWORDS(20)
      WRITE(PRUNIT,210) SWORDS(19), SWORDS(20)
      WRITE(PRUNIT,211) HWORDS(21), HWORDS(22)
      WRITE(PRUNIT,211) SWORDS(21), SWORDS(22)
      WRITE(PRUNIT,212)
      WRITE(PRUNIT,213) (HWORDS(K),K=23,26)
      WRITE(PRUNIT,213) (SWORDS(K),K=23,26)
      WRITE(PRUNIT,*) ' '
      WRITE(PRUNIT,*) ' '
      ENDIF
C
  101 FORMAT(/' VERTEX banks for LV0 detector - Version ',I3,
     &  ' Current bunch is ',I3/,
     &  ' The Fast Z id is ',I4,
     &  ' and the Vertex board number is ',I4)
  199 FORMAT(/' Comparison of Hardware Slow Z vs Software Slow Z')
  200 FORMAT(/,40X,' SHORT COUNTERS ',2X,' LONG COUNTERS ')
  201 FORMAT(/,' The Sum of (Corrected Time)**2 is ',I15,2X,I15)
  202 FORMAT(/,' The Sum of Corrected Time is      ',I15,2X,I15)
  203 FORMAT(/,' The Minimum Corrected Time is     ',I15,2X,I15)
  204 FORMAT(/,' The Maximum Corrected Time is     ',I15,2X,I15)
  205 FORMAT(/,' The Number of Hits for Short Counters is      ',I8)
  206 FORMAT(/,' The Number of Valid Hits for Long Counters is ',I8)
  207 FORMAT(/,' The average time is ',I4,
     &  ' and the the standard deviation of time is ',I4)
  208 FORMAT(/,' The Number of Hits used in statistics calculation is ',
     &  I4)
  209 FORMAT(/,' The two statistics calculation values are ',I4,' and ',
     &  I4)
  210 FORMAT(/,' The Vertex Position is ',I4,' and the data from FASTZ',
     &           ' module is ',I4)
  211 FORMAT(/,' The Interaction Flag is ',I4,' and the Good Vertex',
     &           ' Flag is ',I4)
  212 FORMAT(/,8X,' MI Flag 3 ',2X,' MI Flag 2 ',2X,' MI Flag 1 ',2X,
     &           ' MI Flag 0 ')
  213 FORMAT(/,5X,I10,3X,I10,3X,I10,3X,I10)
      GOTO 999
C
C  Bad Z vertex from slow vertex finding.
C
  990 CONTINUE
      SLOW_Z = 0.0
      GOODSZ = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
