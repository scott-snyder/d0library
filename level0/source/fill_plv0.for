      SUBROUTINE FILL_PLV0()
C----------------------------------------------------------------------
C
C    Purpose and Methods : Fills the Level 0 PLV0 processed results bank
C
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-JUL-1992   Freedy Nang
C-   Updated  30-NOV-1992   Jeffrey Bantly  set interaction bit
C-   Updated  25-JAN-1993   Jeffrey Bantly  interaction bit redefined
C-   Updated   1-JUL-1993   Jeffrey Bantly  add TRGR 71 FADC info
C-   Updated  26-JUL-1995   Jeffrey Bantly  fix bug in TRGR 71 FADC info 
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER CBUNCH
      INTEGER IBUNCH,ICHAN
      INTEGER INTERACTION,MI_FLAG
      INTEGER SINTERACTION,MIS_FLAG
      INTEGER CHAN_HIT(80)
      INTEGER SHORT_N,SHORT_S
      INTEGER LONG_N,LONG_S
      INTEGER ERR
      INTEGER LKPLV0,GZPLV0
      EXTERNAL GZPLV0
      INTEGER N_NHIT,S_NHIT
      INTEGER N_BEST_HIT,S_BEST_HIT
      INTEGER VWORDS(52)
      INTEGER LTRGR,LCRATE,GZFIND_CRATE
      EXTERNAL GZFIND_CRATE
C
      REAL FASTZ,SLOW_Z,FULL_Z,SLOWER_Z(10,10),FULL_SZ
      REAL MI_QUALITY,MIS_QUALITY
      REAL CHAN_EFF(72)
      REAL SCHAN_EFF(72)
      REAL N_TIME(10),S_TIME(10)
      REAL N_PH(10),S_PH(10)
C
      LOGICAL FIRST,LV0,GOODFZ,GOODSZ,GOODSSZ
      LOGICAL EZERROR
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C      IF (FIRST) THEN
C        CALL EZPICK('LEVEL0_RCP')
C        IF ( EZERROR(ERR) ) THEN
C          CALL ERRMSG('LEVEL0-no-rcp','FILL_PLV0',
C     &                                 'LEVEL0_RCP not found.','W')
C        ELSE
C          CALL EZRSET
C        ENDIF
CC
C        FIRST = .FALSE.
C      END IF
C
C  Book bank PLV0 if necessary.
C
      LKPLV0 = GZPLV0()
      IF ( LKPLV0.LE.0 ) THEN
        CALL BKPLV0(LKPLV0)
        IF (LKPLV0 .EQ. 0) THEN
          CALL ERRMSG('LEVEL0-error-book-PLV0','FILL_PLV0',
     &                       'PLV0 bank not booked','W')
          GOTO 999
        ENDIF
      ELSE
        LTRGR = LQ(LHEAD-IZTRGR)
        IF (LTRGR .EQ. 0) GOTO 999
        LCRATE = GZFIND_CRATE('TRGR',LTRGR,1)
        IF (LCRATE .LE. 0) GOTO 999
      ENDIF
C
C  Fetch correct bunch number.
C
      CALL L0_COR_BUNCH(CBUNCH)
C
C  Fill FASTZ words.
C
      FASTZ=0.0
      GOODFZ=.FALSE.
      CALL L0_FASTZ_VERTEX(FASTZ,GOODFZ)
      IF ( GOODFZ ) THEN
        IQ(LKPLV0+1) = IBSET(IQ(LKPLV0+1),0)
        Q(LKPLV0+2)  = FASTZ
      ENDIF
C
C  Fill Slow Z words.
C
      SLOW_Z=0.0
      GOODSZ=.FALSE.
      CALL L0_SLOW_VERTEX(SLOW_Z,MI_FLAG,MI_QUALITY,
     &               INTERACTION,GOODSZ,FULL_Z,CHAN_EFF)
      IF ( INTERACTION.EQ.1 ) THEN
        IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),12)
      ENDIF
      IF ( GOODSZ ) THEN
        IF ( MI_FLAG.GE.1 .AND. MI_FLAG.LE.4 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),MI_FLAG)
        ENDIF
        IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),5)
        Q(LKPLV0+3) =SLOW_Z
        Q(LKPLV0+4) =MI_QUALITY
        Q(LKPLV0+14)=FULL_Z
      ENDIF
C
C  Fill software Slow Z words.
C
      CALL VZERO(SLOWER_Z,100)
      GOODSSZ=.FALSE.
      SINTERACTION=0
      CALL L0_SLOWER_VERTEX(SLOWER_Z,MIS_FLAG,MIS_QUALITY,
     &                        SINTERACTION,GOODSSZ,FULL_SZ,SCHAN_EFF)
      IF ( SINTERACTION.EQ.1 ) THEN
        IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),13)
      ENDIF
      IF ( GOODSSZ ) THEN
        IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),6)
        Q(LKPLV0+5) =SLOWER_Z(10,10)
        Q(LKPLV0+6) =MIS_QUALITY
        Q(LKPLV0+15)=FULL_SZ
      ENDIF
C
C  Fill used channels bits and used channels totals.
C
      CALL L0_CHAN_HIT(CHAN_HIT)
C
      SHORT_N = 0
      DO ICHAN=1,20
        IF ( CHAN_HIT(ICHAN).GT.0.0 ) THEN
          SHORT_N = SHORT_N + 1
          IQ(LKPLV0+11)=IBSET(IQ(LKPLV0+11),(ICHAN-1))
        ENDIF
      ENDDO
      IQ(LKPLV0+7) = SHORT_N
C
      LONG_N = 0
      DO ICHAN=21,36
        IF ( CHAN_HIT(ICHAN).GT.0.0 ) THEN
          LONG_N = LONG_N + 1
          IQ(LKPLV0+13)=IBSET(IQ(LKPLV0+13),(ICHAN-21))
        ENDIF
      ENDDO
      IQ(LKPLV0+8) = LONG_N
C
      SHORT_S = 0
      DO ICHAN=37,56
        IF ( CHAN_HIT(ICHAN).GT.0.0 ) THEN
          SHORT_S = SHORT_S + 1
          IQ(LKPLV0+12)=IBSET(IQ(LKPLV0+12),(ICHAN-37))
        ENDIF
      ENDDO
      IQ(LKPLV0+9) = SHORT_S
C
      LONG_S = 0
      DO ICHAN=57,72
        IF ( CHAN_HIT(ICHAN).GT.0.0 ) THEN
          LONG_S = LONG_S + 1
          IQ(LKPLV0+13)=IBSET(IQ(LKPLV0+13),(ICHAN-41))
        ENDIF
      ENDDO
      IQ(LKPLV0+10) = LONG_S
C
C  Fill in words with the times of the two FADC pulses from the TRGR crate 71
C  FADC pulses.
C
      CALL L0_FADC_HITS(N_TIME,N_PH,N_NHIT,N_BEST_HIT,
     &  S_TIME,S_PH,S_NHIT,S_BEST_HIT)
      IF (N_BEST_HIT.GT.0.AND.N_BEST_HIT.LE.10) 
     &  Q(LKPLV0+16)=N_TIME(N_BEST_HIT)
      IF (S_BEST_HIT.GT.0.AND.S_BEST_HIT.LE.10) 
     &  Q(LKPLV0+17)=S_TIME(S_BEST_HIT)
C
C  Fill in bits noting if min,max hits and/or if long counter
C  hits were used on N,S
C
      CALL GTL0VX(CBUNCH,VWORDS)
      IF ( VWORDS(3).EQ.1 ) THEN
        IF ( VWORDS(17).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),14)
        ENDIF
        IF ( VWORDS(18).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),15)
        ENDIF
        IF ( VWORDS(43).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),16)
        ENDIF
        IF ( VWORDS(44).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),17)
        ENDIF
      ELSE
        IF ( VWORDS(17).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),16)
        ENDIF
        IF ( VWORDS(18).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),17)
        ENDIF
        IF ( VWORDS(43).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),14)
        ENDIF
        IF ( VWORDS(44).EQ.1 ) THEN
          IQ(LKPLV0+1)=IBSET(IQ(LKPLV0+1),15)
        ENDIF
      ENDIF
C
C  Fill in correct bunch last.
C  A filled in correct bunch marks the bank as having been filled
C  as far as the FASTZ, slow z, and slower z routines are concerned.
C
      IF ( CBUNCH.LT.1 .OR. CBUNCH.GT.6 ) GOTO 990
      CALL MVBITS(CBUNCH,0,3,IQ(LKPLV0+1),7)
C
C  Done.
C
  990 CONTINUE
C
C-------------------------------------------------------------------------
  999 RETURN
      END
