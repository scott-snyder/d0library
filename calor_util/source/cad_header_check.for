      SUBROUTINE CAD_HEADER_CHECK(RCP_BANK,NCAD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECKS CAD BANK HEADERS FOR ERRORS
C-                         CONTROLED VIA RCP_BANK
C-                         
C-                         CHECK ALL CONTROL WORDS ARE SAME
C-                 
C-
C-   Inputs  : RCP_BANK [C]   CHARACTER STRING GIVING RCP_BANK TO CONTROL
C-             NCAD     [I]   CAD NUMBER 1 or 2 when WHOLE_THING=.TRUE. 
C-                            For ENTRY CAD_HEADER_CHECK: NCH
C-                              Number of CAD CHannels 
C-                            For ENTRY CAD_ADDRESS_CHECK: ADDR
C-                              CAD ADDR WORD on INPUT - 
C-                              ADDR PACKED WORD CONTAINING IN LOWEST 14 BITS
C-                              THE UPPER 14 BITS OF CAD BANK DATA WORD
C-   Outputs : IER    [I]...  0 = OK
C-                            1 = HEADER INFO OK; SKIP CHECKING ADDRESSES 
C-                           -1 = ERROR but CONTINUE with DATA
C-                        <= -2 = ERROR - SKIP THIS EVENT 
C-                           -3 = NON-ZERO-SUPPRESSED NUMBER OF CHANNELS BAD
C-                           -4 = NCAD INPUT BANK DOESN'T EXIST
C-                           -5 = CAD STATUS BAD 
C-                           -6 = CAD CONTROL WORD BAD 
C-                           -7 = NUMBER OF CHANNELS TOO BIG
C-                           -8 = CAD CONTROL WORD BLS MODE CONTROL BAD
C-                           -9 = CAD VERSION CHANGE DURING RUN 
C-                          -10 = CAD HEADER_LEN TOO BIG ( > 5 )
C-   Controls: RCP_BANK
C-
C-   Created   7-JAN-1991   Chip Stewart
C-   Updated  29-JUN-1992   Chip Stewart  ADDED RESET ENTRY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INTEGER NCAD,IER,NCHAN,NCH,LCAD,POINT,NCARD,J,IW2,DATYPE
      INTEGER NEVENT, DEPTH,PRESCALE,STATUS_MASK
      INTEGER VERSION,ICAD,HEADER_LEN,SYNCH,CONTROL_WORD
      INTEGER STATUS_VERTEX,PULSER,PREV_CONTROL_MODE,PREV_VERSION
      INTEGER GZCAD1,GZCAD2,IBCLR,NWORDS
      LOGICAL FIRST,ZSUPRS,BTEST
      CHARACTER*(*) RCP_BANK
      CHARACTER*40 MSG
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CONTROL_WORD,CONTROL_BYTE)
      INTEGER*2 CONTROL_MODE(2)
      EQUIVALENCE(CONTROL_WORD,CONTROL_MODE)
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        NEVENT = 0
        PREV_VERSION           = 0
        CALL EZPICK(RCP_BANK)
        CALL EZERR(IER)
        IF ( IER.EQ.0) THEN
          CALL EZGET('CAD_ADDR_CHECK_PRESCALE',PRESCALE,IER)
          IF (IER.NE.0 .OR. PRESCALE.EQ.0) THEN
            PRESCALE = 1
            CALL ERRMSG('NO CAD_ADDRESS_CHECK_PRESCALE in RCP ',
     &        'CAD_BANK_CHECK',' USE NOMINAL 1 ','W')
          END IF
          CALL EZRSET
        ELSE
          PRESCALE = 10
        END IF
      END IF
      IER = 0
      NEVENT = NEVENT + 1
C
C ****  PRESCALE 
C
      IF ( MOD(NEVENT,PRESCALE).NE.0)   IER = 1
      NCH = 0
      LCAD = 0
      IF ( NCAD.EQ.1 ) LCAD = GZCAD1()
      IF ( NCAD.EQ.2 ) LCAD = GZCAD2()
      IF ( LCAD .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
      POINT = 0
      PREV_CONTROL_MODE = 0
      NWORDS            = IQ(LCAD-1)
  112 HEADER_LEN        = IQ(LCAD+1+POINT)
      SYNCH             = IQ(LCAD+2+POINT)
      CONTROL_WORD      = IQ(LCAD+3+POINT)
      VERSION           = IQ(LCAD+4+POINT)
      IF (BTEST(VERSION,29)) THEN              ! MC CAD bank
        VERSION           = IBCLR(VERSION,20)  ! don't look at NOISY bits
        VERSION           = IBCLR(VERSION,21)
      END IF
      STATUS_VERTEX     = IQ(LCAD+5+POINT)
      ZSUPRS            = IAND(CONTROL_WORD,4).NE.0
      IF ( HEADER_LEN .GT. 5)  THEN
        WRITE(MSG,1006) HEADER_LEN 
        CALL ERRMSG('CAD-HEADER-BAD','CAD_HEADER_CHECK',MSG,'W')
        IER = -10
        GOTO 999
      ENDIF
      IF ( STATUS_VERTEX .LT. 0)  THEN
        WRITE(MSG,1000) STATUS_VERTEX
        CALL ERRMSG('CAD-STATUS-BAD','CAD_HEADER_CHECK',MSG,'W')
        IER = -5
        GOTO 999
      ENDIF
      IF ( (PREV_CONTROL_MODE .NE. 0)
     &  .AND. (CONTROL_MODE(WORD1).NE. PREV_CONTROL_MODE) ) THEN
        WRITE(MSG,1004) CONTROL_WORD, PREV_CONTROL_MODE
        CALL ERRMSG('CAD-CONTROL-BAD','CAD_HEADER_CHECK',MSG,'W')
        IER = -6
        PREV_CONTROL_MODE      = CONTROL_MODE(WORD1)
        GOTO 999
      ENDIF
      IF ( (PREV_VERSION .NE. 0)
     &  .AND. (VERSION.NE.PREV_VERSION) ) THEN
        WRITE(MSG,1005) VERSION, PREV_VERSION
        CALL ERRMSG('CAD-VERSION-CHANGE','CAD_HEADER_CHECK',MSG,'W')
        IER = -9
        PREV_VERSION           = VERSION
        GOTO 999
      ENDIF
      PREV_VERSION           = VERSION
      PREV_CONTROL_MODE      = CONTROL_MODE(WORD1)
      IW2    =  CONTROL_BYTE(BYTE2)
      NCARD  =  CONTROL_BYTE(BYTE3)
      DATYPE=MOD(IW2,8)
      IF(DATYPE.LE.-1) THEN
        WRITE(MSG,1004) CONTROL_WORD
        CALL ERRMSG('CAD-CONTROL-BAD','CAD_HEADER_CHECK',MSG,'W')
        IER = - 8
        GOTO 999
      ENDIF
      POINT = POINT + HEADER_LEN + 2
      DO 13 J=0,NCARD
        NCHAN = IQ(LCAD+POINT)
        IF(.NOT.ZSUPRS .AND. (MOD(NCHAN,384).NE.0)) THEN
          WRITE(MSG,1002) CONTROL_WORD,NCHAN
          CALL ERRMSG('CAD NWORD WRONG','CAD_HEADER_CHECK',MSG,'W')
          IER = -3
          GOTO 999
        ENDIF
        IF( NCHAN.GT.768) THEN
          CALL ERRMSG('CAD CHANNELS WRONG','CAD_HEADER_CHECK',
     &      'NUMBER OF CHANNELS IN CARD TOO BIG','W')
          IER = -7
          GOTO 999
        END IF
        POINT = POINT + NCHAN + 1
        NCH = NCH + NCHAN
   13 CONTINUE
C
C ****  Check for another crate
C
      POINT = POINT + 3
      IF( (POINT.LT.(NWORDS-NTRAILER)).AND.
     &  (IQ(LCAD+POINT+1).EQ.HEADER_LEN) ) GOTO 112
      
  999 RETURN
 1000 FORMAT(' STATUS =',Z9.8)
 1002 FORMAT(' CONTROL=',Z9.8,' NCHAN= ',I5)
 1004 FORMAT(' CONTROL=',2Z10.8)
 1005 FORMAT(' VERSION=',2Z10.8)
 1006 FORMAT(' HEADER_LEN=',I10)
C------------------------------------------------------------------------------
      ENTRY CAD_HEADER_CHECK_RESET
      PREV_CONTROL_MODE = 0
      PREV_VERSION = 0
      END
