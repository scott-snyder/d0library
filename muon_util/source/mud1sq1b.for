C&IF VAXVMS
      OPTIONS /CHECK=NOOVERFLOW
C&ENDIF
      SUBROUTINE MUD1SQ1B(NMOD_KEEP,MOD_KEEP_LIST,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Squeeze the MUD1 bank.  
C-
C-   Inputs  :  NMOD_KEEP      Number of modules to keep in MUD1.
C-              MOD_KEEP_LIST  List of modules to keep.
C-
C-   Outputs :  IER       0 = Success
C-                        1 = MUD1 doesn't exist.
C-                        2 = New size larger than original.
C-   Controls: 
C-
C-   Created   4-DEC-1993   Cary Y. Yoshikawa
C-   Modified  2-JAN-1994   D. Wood, fix trailer bug, add buffer
C-                          overflow protection
C-   Modified  9-mar-1994   D. Wood, set squeeze bit in crate header
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IBMAX
      PARAMETER(IBMAX=2000)             ! buffer size
      INTEGER NMOD_KEEP,MOD_KEEP_LIST(200),IER,I
      INTEGER LMUD1,GZMUD1,LCRATE_OFF,IBUF_CRATE_OFF
      INTEGER BUF(IBMAX)
      INTEGER LTRAIL_OFF,ICONT,NHEAD,ITRIG,NDATA,IVERS
      INTEGER LMOD_OFF,NGOOD_MOD_IN_CRATE,NMOD_IN_CRATE,IMOD_BEF
      INTEGER LMOD_OFF_NEXT,MOD_BEF,MADC_BEF,NHIT_BEF,IFLG_BEF,IUS4_BEF
      INTEGER MLAT_BEF(4),I_MOD_KEEP,IB_COUNT
      INTEGER MOD_GOOD(200),MADC_GOOD(200),I_MOD_HD
      REAL    NADC_R
      INTEGER IBUF_ADCA_OFF,LADCA_OFF,NADCA_BEF,NADCA_GOOD
      INTEGER IBUF_ADCB_OFF,LADCB_OFF,NADCB_BEF,NADCB_GOOD
      INTEGER ADC_MASK,IMADC,IGOOD_MOD_IN_CRATE,IADC_BEF
      INTEGER IBUF_CRATE_TRAIL_OFF,L_CRATE_TRAIL_OFF,NSCRATE
      INTEGER LCRATE_NEXT_OFF,TCRID,TCOUNT,TTRIG
      INTEGER IUS3,IUS4,IUS5,IUS6,IUS7,IUS8
      INTEGER NEW_SIZE,OLD_SIZE,DIFF_SIZE,N_MAX_NSCRATE
      INTEGER I_MOD_TEST,MOD_TEST,LMOD_OFF_TEST,LMOD_OFF_NEXT_TEST
      INTEGER MADC_TEST,NHIT_TEST,IFLG_TEST,IUS4_TEST,MLAT_TEST(4)
C----------------------------------------------------------------------
      DATA  IMADC/'801C0000'X/
C----------------------------------------------------------------------
      CALL VZERO(BUF,IBMAX)
      IER = 0
      LMUD1=GZMUD1(0)
      IF (LMUD1.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      LCRATE_OFF = 0
      IBUF_CRATE_OFF = 0
 1000 CONTINUE
      CALL GTMUD1(1,LCRATE_OFF,LTRAIL_OFF,ICONT,NHEAD,ITRIG,NDATA,IVERS
     &            ,IUS5,IUS6,IUS7,IUS8)
      NMOD_IN_CRATE = NHEAD/8 - 1
      LMOD_OFF_TEST = LCRATE_OFF + 8
      DO I_MOD_TEST = 1 , NMOD_IN_CRATE
        CALL GTMUD1(12,LMOD_OFF_TEST,LMOD_OFF_NEXT_TEST,MOD_TEST,
     &         MADC_TEST,NHIT_TEST,IFLG_TEST,IUS4_TEST,
     &         MLAT_TEST(1),MLAT_TEST(2),MLAT_TEST(3),MLAT_TEST(4))
        DO I_MOD_KEEP = 1 , NMOD_KEEP
          IF (MOD_TEST.EQ.MOD_KEEP_LIST(I_MOD_KEEP)) GOTO 300
        ENDDO
        LMOD_OFF_TEST = LMOD_OFF_NEXT_TEST
      ENDDO
C  CRATE HAS NO MODULES OF INTEREST.  GO TO NEXT CRATE.
      CALL GTMUD1(5,LTRAIL_OFF,LCRATE_NEXT_OFF,TCRID,TCOUNT,TTRIG,
     &            IUS3,IUS4,IUS5,IUS6,IUS7,IUS8)
      LCRATE_OFF = LCRATE_NEXT_OFF
      IF (LCRATE_OFF.NE.0) THEN
        GOTO 1000
      ELSE
        GOTO 700  ! LAST CRATE HAS NO GOOD MODS.  ATTACH MUD1 TRAILER.
      ENDIF
  300 CONTINUE  ! THIS CRATE HAS AL LEAST 1 MODULE OF INTEREST
C guard against buffer overflow (though unlikely)
      IF(IBMAX-IBUF_CRATE_OFF.LT.112) THEN
        IER = 3
        GOTO 700
      ENDIF
      DO I = 2 , 8
        BUF(IBUF_CRATE_OFF+I) = IQ(LMUD1+LCRATE_OFF+I)
      ENDDO
C set Squeezed flag in crate header
      BUF(IBUF_CRATE_OFF+4) = IBSET(BUF(IBUF_CRATE_OFF+4),21)
      LMOD_OFF = LCRATE_OFF + 8
      NGOOD_MOD_IN_CRATE = 0
      DO IMOD_BEF = 1 , NMOD_IN_CRATE
        CALL GTMUD1(12,LMOD_OFF,LMOD_OFF_NEXT,MOD_BEF,
     &         MADC_BEF,NHIT_BEF,IFLG_BEF,IUS4_BEF,
     &         MLAT_BEF(1),MLAT_BEF(2),MLAT_BEF(3),MLAT_BEF(4))
        DO I_MOD_KEEP = 1 , NMOD_KEEP
          IF (MOD_BEF.EQ.MOD_KEEP_LIST(I_MOD_KEEP)) THEN
            NGOOD_MOD_IN_CRATE = NGOOD_MOD_IN_CRATE + 1
            MOD_GOOD(NGOOD_MOD_IN_CRATE) = MOD_BEF
            MADC_GOOD(NGOOD_MOD_IN_CRATE) = MADC_BEF
            DO I_MOD_HD = 1 , 8  ! FILL BUF WITH MODULE HEADER
              BUF(IBUF_CRATE_OFF+8+(NGOOD_MOD_IN_CRATE-1)*8+I_MOD_HD) 
     &          = IQ(LMUD1+LCRATE_OFF+8+(IMOD_BEF-1)*8+I_MOD_HD)
            ENDDO
C set Squeezed flag in crate header
            BUF(IBUF_CRATE_OFF+8+(NGOOD_MOD_IN_CRATE-1)*8+4) =
     &       IBSET(BUF(IBUF_CRATE_OFF+8+(NGOOD_MOD_IN_CRATE-1)*8+4),21)
            GOTO 500
          ENDIF
        ENDDO
  500   CONTINUE
        LMOD_OFF = LMOD_OFF_NEXT
      ENDDO
      BUF(IBUF_CRATE_OFF+1) = 8 + 8*NGOOD_MOD_IN_CRATE - 1
C=========================
C  DO ADC
C---------
C
C  DATA BLOCK A
C
      IBUF_ADCA_OFF = IBUF_CRATE_OFF + 8 + 8*NGOOD_MOD_IN_CRATE
      LADCA_OFF = LCRATE_OFF + 8 + 8*NMOD_IN_CRATE
      NADC_R = FLOAT(IQ(LMUD1+LADCA_OFF+1))
      NADCA_BEF = NINT(NADC_R/4)
      NADCA_GOOD = 0
      DO IADC_BEF = 1 , NADCA_BEF
        ADC_MASK = IAND(IQ(LMUD1+LADCA_OFF+4*(IADC_BEF-1)+2),IMADC)
        DO IGOOD_MOD_IN_CRATE = 1 , NGOOD_MOD_IN_CRATE
          IF (ADC_MASK.EQ.MADC_GOOD(IGOOD_MOD_IN_CRATE)) THEN
C skip futher hits if buffer is full
            IB_COUNT = IBUF_ADCA_OFF+4*(NADCA_GOOD+1)
            IF(IB_COUNT+20.LE.IBMAX) THEN
              NADCA_GOOD = NADCA_GOOD + 1
              DO I = 1 , 4
                BUF(IBUF_ADCA_OFF+4*(NADCA_GOOD-1)+I+1) = 
     &            IQ(LMUD1+LADCA_OFF+4*(IADC_BEF-1)+I+1)
              ENDDO
              GOTO 600
            ELSE
              IER = 3
            ENDIF
          ENDIF
        ENDDO
  600   CONTINUE
      ENDDO
      BUF(IBUF_ADCA_OFF+1) = 4*NADCA_GOOD   ! FILL IN DATA BLOCK A HEADER
C
C  DATA BLOCK B
C
      IBUF_ADCB_OFF = IBUF_CRATE_OFF + 8 + 8*NGOOD_MOD_IN_CRATE + 
     &            (4*NADCA_GOOD+1)
      LADCB_OFF = LCRATE_OFF + 8 + 8*NMOD_IN_CRATE + 
     &            (4*NADCA_BEF+1)
      NADC_R = FLOAT(IQ(LMUD1+LADCB_OFF+1))
      NADCB_BEF = NINT(NADC_R/4)
      NADCB_GOOD = 0
      DO IADC_BEF = 1 , NADCB_BEF
        ADC_MASK = IAND(IQ(LMUD1+LADCB_OFF+4*(IADC_BEF-1)+2),IMADC)
        DO IGOOD_MOD_IN_CRATE = 1 , NGOOD_MOD_IN_CRATE
          IF (ADC_MASK.EQ.MADC_GOOD(IGOOD_MOD_IN_CRATE)) THEN
C skip futher hits if buffer is full
            IB_COUNT = IBUF_ADCB_OFF+4*(NADCB_GOOD+1)
            IF(IB_COUNT+20.LE.IBMAX) THEN
              NADCB_GOOD = NADCB_GOOD + 1
              DO I = 1 , 4
                BUF(IBUF_ADCB_OFF+4*(NADCB_GOOD-1)+I+1) = 
     &          IQ(LMUD1+LADCB_OFF+4*(IADC_BEF-1)+I+1)
              ENDDO
              GOTO 650
            ELSE
              IER = 3
            ENDIF
          ENDIF
        ENDDO
  650   CONTINUE
      ENDDO
      BUF(IBUF_ADCB_OFF+1) = 4*NADCB_GOOD   ! FILL IN DATA BLOCK B HEADER
C=====================================================================
C  DO CRATE TRAILER
C------------------
      IBUF_CRATE_TRAIL_OFF = IBUF_ADCB_OFF + 4*NADCB_GOOD + 1
      L_CRATE_TRAIL_OFF = LADCB_OFF + 4*NADCB_BEF + 1
      BUF(IBUF_CRATE_TRAIL_OFF+1) = 8 + 8*NGOOD_MOD_IN_CRATE + 
     &    4*NADCA_GOOD+1 + 4*NADCB_GOOD+1
      BUF(IBUF_CRATE_TRAIL_OFF+2) = IQ(LMUD1+L_CRATE_TRAIL_OFF+2)
      BUF(IBUF_CRATE_TRAIL_OFF+3) = 0
      NSCRATE = 0
      N_MAX_NSCRATE = IBUF_CRATE_TRAIL_OFF+3
      DO I = IBUF_CRATE_OFF+1 , N_MAX_NSCRATE
        NSCRATE = NSCRATE + BUF(I)
      ENDDO
      BUF(IBUF_CRATE_TRAIL_OFF+4) = NOT(NSCRATE)
C==================================================================
      CALL GTMUD1(5,LTRAIL_OFF,LCRATE_NEXT_OFF,TCRID,TCOUNT,TTRIG,
     &            IUS3,IUS4,IUS5,IUS6,IUS7,IUS8)
      LCRATE_OFF = LCRATE_NEXT_OFF
      IBUF_CRATE_OFF = IBUF_CRATE_TRAIL_OFF+4
      IF (LCRATE_OFF.NE.0) GOTO 1000
C============================
C DO MUD1 TRAILER
C----------------
  700 CONTINUE
      DO I = 1 , 16
        BUF(IBUF_CRATE_TRAIL_OFF+4+I) = IQ(LMUD1+LTRAIL_OFF+4+I)
      ENDDO
C============================
C COPY BUF TO MUD1 AND PUSH
C--------------------------
      NEW_SIZE = IBUF_CRATE_TRAIL_OFF+20
      OLD_SIZE = IQ(LMUD1-1)
      IF (NEW_SIZE.GT.OLD_SIZE) THEN
        IER = 2
        GOTO 999
      ENDIF
      DO I = 1 , NEW_SIZE
        IQ(LMUD1+I) = BUF(I)
      ENDDO
      DIFF_SIZE = OLD_SIZE - NEW_SIZE
      CALL MZPUSH(IXCOM,LMUD1,0,-DIFF_SIZE,' ')
C
  999 RETURN
      END
