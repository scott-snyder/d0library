      SUBROUTINE L0_SLOW_VERTEX(SLOW_Z,MI_FLAG,MI_QUALITY,
     &                INTERACTION,GOODZ,FULL_Z,CHAN_EFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Software version of the hardware VERTEX
C-                         board.
C-
C-   Inputs  : none
C-   Outputs : SLOW_Z  = Slow Z vertex position result
C-             MI_FLAG = Multiple Interaction flag :
C-                          1 = most likely single interaction
C-                          2 = likely single interaction
C-                          3 = likely multiple interaction
C-                          4 = most likely multiple interaction
C-             MI_QUALITY   = sigma value for MI_FLAG determination
C-             INTERACTION  = 1=interaction, 0=halo
C-             GOODZ        = TRUE if valid SLOW_Z could be constructed
C-             FULL_Z       = time summation term
C-             CHAN_EFF(72) = channel (correct time - average time at end)
C-   Controls: none
C-
C-   Created  11-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER VERTEX_DATA(52)
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
      INTEGER IPLV0_DATA(20)
      INTEGER VBOARD, SVTX, SIGN, IAND
      INTEGER LINK,GZPLV0,GZL0VX_BUNCH
      EXTERNAL GZPLV0,GZL0VX_BUNCH
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
      REAL SLOW_Z_MULTIPLIER
      REAL PLV0_DATA(20)
      EQUIVALENCE ( PLV0_DATA, IPLV0_DATA )
C
      LOGICAL GOODZ, FIRST
      LOGICAL FULL_PLV0, FULL_L0VX
      LOGICAL SHORT_ONLY, NO_MINMAX
      LOGICAL EZERROR
      EXTERNAL EZERROR
      LOGICAL PRODUC, PRODFL
      EXTERNAL PRODUC
C
      SAVE FIRST,CHG_MINCUT,CHG_MAXCUT,TIM_MINCUT,TIM_MAXCUT
      SAVE SLOW_Z_MULTIPLIER, SLOW_OFFZ, PRODFL
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0_SLOW_VERTEX',
     &                              'LEVEL0_RCP not found.','W')
          SLOW_Z_MULTIPLIER = 0.75
          SLOW_OFFZ = 7.007
        ELSE
          CALL EZGET('SLOW_Z_MULTIPLIER',SLOW_Z_MULTIPLIER,ERR)
          IF ( ERR.NE.0 ) SLOW_Z_MULTIPLIER = 0.75
          CALL EZGET('SLOW_OFFZ',SLOW_OFFZ,ERR)
          IF ( ERR.NE.0 ) SLOW_OFFZ = 7.007
          CALL EZRSET
        ENDIF
        PRODFL=PRODUC()
C
        FIRST = .FALSE.
      ENDIF
C
      SLOW_Z = 0.0
      GOODZ  = .FALSE.
      INTERACTION = 0
C
C  Fetch the correct bunch number
C
      CALL L0_COR_BUNCH(CBUNCH)
      IF ( CBUNCH.LT.1 .OR. CBUNCH.GT.6 ) GOTO 990
C
C  Check if data in PLV0 bank.
C
      FULL_PLV0 = .FALSE.
      LINK=GZPLV0()
      IF ( LINK.GT.0 ) THEN
        IF ( IBITS(IQ(LINK+1),7,3).GT.0 ) THEN
          CALL GTPLV0(IPLV0_DATA)
          SLOW_Z=PLV0_DATA(3)
          IF ( BTEST(IPLV0_DATA(1),12) ) INTERACTION = 1
          IF ( BTEST(IPLV0_DATA(1),1) ) THEN
            MI_FLAG=1
          ELSEIF ( BTEST(IPLV0_DATA(1),2) ) THEN
            MI_FLAG=2
          ELSEIF ( BTEST(IPLV0_DATA(1),4) ) THEN
            MI_FLAG=4
          ELSEIF ( BTEST(IPLV0_DATA(1),3) ) THEN
            MI_FLAG=3
          ELSE
            IF ( INTERACTION.EQ.1 ) THEN
              IF ( .NOT.PRODFL ) CALL ERRMSG('LEVEL0-NO-MIFLAG-LSV',
     &                'L0_SLOW_VERTEX','NO MIFLAG FD','W')
            ENDIF
          ENDIF
          MI_QUALITY=PLV0_DATA(4)
          IF ( BTEST(IPLV0_DATA(1),5) ) GOODZ = .TRUE.
          FULL_PLV0 = .TRUE.
        ENDIF
      ENDIF
C
C  Obtain remaining data from correct bunch L0VX bank.
C
      LINK=GZL0VX_BUNCH(CBUNCH)
      IF ( LINK.GT.0 ) THEN
        CALL GTL0VX(CBUNCH,VERTEX_DATA)
        VBOARD=VERTEX_DATA(3)
        IF ( VBOARD.EQ.1 ) THEN
          OFFSET=0
        ELSEIF (VBOARD.EQ.2 ) THEN
          OFFSET=26
        ELSE
          GOTO 980
        ENDIF
        IF ( .NOT.FULL_PLV0 ) THEN
          IF ( VERTEX_DATA(OFFSET+21).EQ.1 ) INTERACTION=1
          SVTX = VERTEX_DATA(OFFSET+19)
          SIGN = IAND(SVTX,'100'X)/'100'X
          IF ( SIGN.EQ.0 ) THEN
            SLOW_Z = SLOW_Z_MULTIPLIER*IAND(SVTX,'FF'X)
          ELSE
            SLOW_Z = -SLOW_Z_MULTIPLIER*IAND('100'X-SVTX,'FF'X)
          ENDIF
          IF ( VERTEX_DATA(OFFSET+26).EQ.1 ) then
            MI_FLAG=1
          ELSEIF ( VERTEX_DATA(OFFSET+25).EQ.1 ) then
            MI_FLAG=2
          ELSEIF ( VERTEX_DATA(OFFSET+23).EQ.1 ) then
            MI_FLAG=4
          ELSEIF ( VERTEX_DATA(OFFSET+24).EQ.1 ) then
            MI_FLAG=3
          ELSE
            IF ( INTERACTION.EQ.1 ) THEN
              IF ( .NOT.PRODFL ) CALL ERRMSG('LEVEL0-NO-MIFLAG-LSV2',
     &                'L0_SLOW_VERTEX','NO MIFLAG FD','W')
            ENDIF
          ENDIF
          IF ( VERTEX_DATA(OFFSET+22).EQ.1 ) GOODZ=.TRUE.
          FULL_L0VX=.TRUE.
        ENDIF
        TAVG(VERTEX_DATA(3)) = VERTEX_DATA(15)
        TAVG(VERTEX_DATA(29)) = VERTEX_DATA(41)
        FULL_Z = (-2./30.0)*(SLOW_Z+SLOW_Z-SLOW_OFFZ)
C
        CALL VZERO(USE_CHAN,72)
        CALL VZERO(N_SC,2)
        CALL VZERO(N_LC,2)
        CALL VZERO(N_CH,2)
        CALL VZERO(TSUM,2)
        CALL VZERO(T2SUM,2)
        CALL VZERO(TAVG,2)
C
C  Fill in remaining values based on L0VX bank data.
C
        DO OFFSET=0,26,26
C
          N_SC(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(5+OFFSET)
          N_LC(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(10+OFFSET)
C
          SHORT_ONLY = .FALSE.
          IF ( VERTEX_DATA(18+OFFSET).EQ.1 ) SHORT_ONLY = .TRUE.
          NO_MINMAX = .FALSE.
          IF ( VERTEX_DATA(17+OFFSET).EQ.1 ) NO_MINMAX = .TRUE.
C
          IF ( SHORT_ONLY ) THEN
            IF ( NO_MINMAX ) THEN
              TSUM(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(8+OFFSET) -
     &            VERTEX_DATA(6+OFFSET) - VERTEX_DATA(7+OFFSET)
              T2SUM(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(4+OFFSET) -
     &          VERTEX_DATA(6+OFFSET)**2. - VERTEX_DATA(7+OFFSET)**2.
              N_CH(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(5+OFFSET) - 2
            ELSE
              TSUM(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(8+OFFSET)
              T2SUM(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(4+OFFSET)
              N_CH(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(5+OFFSET)
            ENDIF
          ELSE
            TSUM(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(8+OFFSET) +
     &                     VERTEX_DATA(13+OFFSET)
            TSUM(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(4+OFFSET) +
     &                     VERTEX_DATA(9+OFFSET)
            N_CH(VERTEX_DATA(3+OFFSET)) = VERTEX_DATA(5+OFFSET) +
     &                     VERTEX_DATA(10+OFFSET)
          ENDIF
C
        ENDDO
C
C  Multiple Interaction quality comes in 50psec bins, convert to nsec.
C
        MI_QUALITY = SQRT(VERTEX_DATA(14)**2.+VERTEX_DATA(14+26)**2.)
        MI_QUALITY = MI_QUALITY * 0.05
C
C  Fill array with channel differences using correct bunch L0AD bank.
C
  980   CONTINUE
        CALL GTL0AD(CBUNCH,IBUNCH,NCHAN,NWORD,RAW_TIME,BUNCH_ID,
     &                                        RAW_CHARGE,CORRECT_TIME)
        CALL VZERO(CHAN_EFF,72)
        DO CHAN=1,20
          IF (USE_CHAN(CHAN).EQ.1 )
     &            CHAN_EFF(CHAN)=CORRECT_TIME(CHAN)-TAVG(1)
        ENDDO
        DO CHAN=37,56
          IF (USE_CHAN(CHAN).EQ.1 )
     &            CHAN_EFF(CHAN)=CORRECT_TIME(CHAN)-TAVG(2)
        ENDDO
C
C  Done.
C
      ENDIF
      IF ( FULL_PLV0 ) GOTO 999
      IF ( FULL_L0VX ) GOTO 999
C
C  Bad slow Z vertex from slow vertex board.
C
  990 CONTINUE
      SLOW_Z = 0.0
      GOODZ = .FALSE.
      INTERACTION = 0
C----------------------------------------------------------------------
  999 RETURN
      END
