      FUNCTION MRVETO(VETO_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find out if a given MR setting was set
C-
C-   Inputs  : VETO_NAME of the MRVETO setting
C-   Outputs : TRUE if the corresponding MRVETO condition was SET,
C-             FALSE otherwise
C-   Controls: none
C-
C-   Created  17-OCT-1994   Meenakshi Narain
C-   Updated  20-OCT-1994   Jeffrey Bantly  add early veto conditions
C-                                          add documentation of outputs
C-   Updated   1-MAR-1995   Jeffrey Bantly  correct 1a mrbs_loss bit move 
C-
C-----------------------------------------------------------------------
C-
C-   Possible Main Ring veto quantities:
C-
C-   MRBS_LOSS, MICRO_BLANK, CAL_RECOVERY, MU_HV_RECOVERY, MR_VETO_HIGH,
C-   MR_VETO_LOW
C-
C-   Possible Active Veto conditions:
C-
C-   MAX_LIVE, GOOD_CAL, GOOD_BEAM
C-
C-   for runs before 79984 there was NO active veto scheme so the defaults
C-      installed are:
C-
C-      MAX_LIVE = MRBS_LOSS
C-      GOOD_CAL = MRBS_LOSS or MICROBLANK
C-      GOOD_BEAM = MRBS_LOSS or MICROBLANK
C-
C-   for runs taken with the initial active veto scheme ( 79984 - 81577 ):
C-
C-      MAX_LIVE = MR_VETO_HIGH
C-      GOOD_CAL = MR_VETO_HIGH or MR_VETO_LOW
C-      GOOD_BEAM = MRBS_LOSS or MICROBLANK
C-
C-   for runs taken with the second active veto scheme ( 81578 - present ):
C-
C-      MAX_LIVE = MRBS_LOSS and MICROBLANK
C-      GOOD_CAL = (MRBS_LOSS and MICROBLANK) or MR_VETO_LOW
C-      GOOD_BEAM = MRBS_LOSS or MICROBLANK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NUM_VETO,NUM_L1TERMS
      PARAMETER( NUM_L1TERMS = 6 )
      PARAMETER( NUM_VETO = 3 )
      INTEGER ANDOR_TERM_BITMAP(NUM_L1TERMS)
      CHARACTER*(*) VETO_NAME
      CHARACTER*32 ANDOR_TERM_NAME(NUM_L1TERMS)
      CHARACTER*32 NAME,MRVETO_NAME(NUM_VETO)
      LOGICAL MRVETO_SET(NUM_VETO),ANDOR_TERM_SET(NUM_L1TERMS)
      INTEGER LKGLOB,GZGLOB,LENG,BITNUM,I,TRULEN,RUNNUM,RUNNO
      EXTERNAL GZGLOB,RUNNO
      LOGICAL MRVETO
      LOGICAL MAX_LIVE,GOOD_CAL,GOOD_BEAM
      DATA ANDOR_TERM_BITMAP/ 3, 7, 5, 8, 13, 12 /
      DATA ANDOR_TERM_NAME/'MR_VETO_HIGH','MR_VETO_LOW',
     &  'MU_HV_RECOVERY','CAL_RECOVERY','MICRO_BLANK','MRBS_LOSS'/
      DATA MRVETO_NAME/'MAX_LIVE','GOOD_CAL','GOOD_BEAM'/
C----------------------------------------------------------------------
C
      MRVETO = .FALSE.
      RUNNUM=RUNNO()
C
      CALL UPCASE(VETO_NAME,NAME)
      LENG = MIN(TRULEN(NAME),32)
C
      LKGLOB=GZGLOB()
      IF (LKGLOB.NE.0) THEN
        DO I=1, NUM_L1TERMS
          BITNUM = ANDOR_TERM_BITMAP(I)
          ANDOR_TERM_SET(I) = .FALSE.
          IF (BTEST(IQ(LKGLOB+18),BITNUM)) ANDOR_TERM_SET(I) =.TRUE.
C  mrbs_loss was at different location for 1A runs before 63400
          IF (I.EQ.6.AND.RUNNUM.LE.63400) THEN
            BITNUM = 9
            ANDOR_TERM_SET(I) = .FALSE.
            IF (BTEST(IQ(LKGLOB+18),BITNUM)) ANDOR_TERM_SET(I) =.TRUE.
          ENDIF
C
        ENDDO
      ELSE
        GOTO 300
      ENDIF
C
C ****  MAX_LIVE = (MRBS_LOSS)                 pre-Active Veto
C ****  MAX_LIVE = (MR_VETO_HIGH)              first Active Veto scheme
C ****  MAX_LIVE = (MRBS_LOSS AND MICRO_BLANK) current Active Veto scheme
C
      IF (RUNNUM.LT.79984) THEN
        MAX_LIVE = ANDOR_TERM_SET(6)
      ELSEIF (RUNNUM.GE.79984 .AND. RUNNUM.LE.81577) THEN
        MAX_LIVE = ANDOR_TERM_SET(1)
      ELSE
        MAX_LIVE = ANDOR_TERM_SET(6) .AND.
     &             ANDOR_TERM_SET(5)
      ENDIF
      MRVETO_SET(1) = MAX_LIVE
C
C ****  GOOD_CAL = (MRBS_LOSS or MICRO_BLANK)   pre-Active Veto
C ****  GOOD_CAL = (MR_VETO_HIGH or MR_VETO_LOW)  first Active Veto scheme
C ****  GOOD_CAL = (MRBS_LOSS AND MICRO_BLANK or MR_VETO_LOW)  current AV
C
      IF (RUNNUM.LT.79984) THEN
        GOOD_CAL =  ANDOR_TERM_SET(6) .OR.
     &              ANDOR_TERM_SET(5)
      ELSEIF (RUNNUM.GE.79984 .AND. RUNNUM.LE.81577) THEN
        GOOD_CAL =  ANDOR_TERM_SET(1) .OR.
     &              ANDOR_TERM_SET(2)
      ELSE
        GOOD_CAL = (ANDOR_TERM_SET(6) .AND.
     &              ANDOR_TERM_SET(5)) .OR.
     &              ANDOR_TERM_SET(2)
      ENDIF
      MRVETO_SET(2) = GOOD_CAL
C
C ****  GOOD_BEAM = (MRBS_LOSS or MICRO_BLANK)
C
      GOOD_BEAM = ANDOR_TERM_SET(6) .OR.
     &            ANDOR_TERM_SET(5)
      MRVETO_SET(3) = GOOD_BEAM
C
C ****  Determine out if the requested veto was set?
C
C
      DO I=1, NUM_L1TERMS
        IF (ANDOR_TERM_NAME(I)(1:LENG).EQ.NAME(1:LENG)) THEN
          MRVETO = ANDOR_TERM_SET(I)
          GO TO 300
        ENDIF
      ENDDO
      DO I=1, NUM_VETO
        IF (MRVETO_NAME(I)(1:LENG).EQ.NAME(1:LENG)) THEN
          MRVETO = MRVETO_SET(I)
          GO TO 300
        ENDIF
      ENDDO
  300 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END

