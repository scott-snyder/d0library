      LOGICAL FUNCTION VERIFY_ZEBRA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Verify Zebcom store using DZVERI.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  VERIFY_ZEBRA_INI - Initialization.
C-                  VERIFY_ZEBRA_END - Statistical summary.
C-
C-   Created  10-Dec-1993   Herbert Greenlee
C-   Modified 16-Nov-1994   Herbert Greenlee 
C-     Check TSUM bnak.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VERIFY_ZEBRA_INI, VERIFY_ZEBRA_END
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C-
C- Variables from VERIFY_ZEBRA_RCP.
C-
      LOGICAL DO_VERIFY_ZEBRA
      LOGICAL ZEBRA_ERRORS_FATAL
      LOGICAL CHECK_BANKS, CHECK_LINKS, CHECK_STORE, CHECK_UP_LINKS
      LOGICAL CHECK_TSUM
C-
C- TSUM stuff.
C-
      LOGICAL TSUM_ERROR
      INTEGER LTSUM, GZTSUM
      INTEGER NTRIG, NFILT, NFIX, NR, TRIG, FILT, I, P
C-
C- Statistics
C-
      INTEGER NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
      INTEGER NUM_ZEBRA_ERR, NUM_TSUM_ERR
C-
C- Other variables and functions.
C-
      INTEGER IER
      CHARACTER*8 CHOPT
      INTEGER LUN, SSUNIT
      LOGICAL FIRST
C-
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      VERIFY_ZEBRA = .TRUE.
      NUM_EVENT_INPUT = NUM_EVENT_INPUT + 1
C-
C- Verify Zebra
C-
      IF(DO_VERIFY_ZEBRA)THEN
        CALL DZVERI(' ', IXMAIN, CHOPT)
        IF(IQUEST(1).NE.0)THEN
          CALL ERRMSG('Zebra error', 'VERIFY_ZEBRA',
     &      'Error detected by DZVERI in Zebcom store', 'W')
          NUM_ZEBRA_ERR = NUM_ZEBRA_ERR + 1
          VERIFY_ZEBRA = .FALSE.
        ENDIF
C-
C- Check TSUM
C-
        IF(CHECK_TSUM)THEN
          TSUM_ERROR = .FALSE.
          LTSUM = GZTSUM()
          IF(LTSUM.GT.0)THEN
            NFIX = IQ(LTSUM+2)
            NR = IQ(LTSUM+3)
            NTRIG = IQ(LTSUM+4)
            NFILT = IQ(LTSUM+5)
            DO I = 1,NTRIG
              P = NFIX + 1 + (I-1)*NR
              TRIG = IQ(LTSUM + P)
              IF(TRIG.LT.0.OR.TRIG.GT.31)THEN
                CALL ERRMSG('TSUM error', 'VERIFY_ZEBRA',
     &            'Bad trigger number in TSUM bank', 'W')
                TSUM_ERROR = .TRUE.
              ENDIF
            ENDDO
            DO I = 1,NFILT
              P = NFIX + 1 + (NTRIG+I-1)*NR
              FILT = IQ(LTSUM + P)
              IF(FILT.LT.0.OR.FILT.GT.127)THEN
                CALL ERRMSG('TSUM error', 'VERIFY_ZEBRA',
     &            'Bad filter number in TSUM bank', 'W')
                TSUM_ERROR = .TRUE.
              ENDIF
            ENDDO
          ENDIF
          IF(TSUM_ERROR)THEN
            NUM_TSUM_ERR = NUM_TSUM_ERR + 1
            VERIFY_ZEBRA = .FALSE.
          ENDIF
        ENDIF
      ENDIF
C-
C- Event statistics
C-
      IF(VERIFY_ZEBRA)NUM_EVENT_OUTPUT = NUM_EVENT_OUTPUT + 1
      GO TO 999
 
      ENTRY VERIFY_ZEBRA_INI()
C-
C- Initialization entry point
C-
      VERIFY_ZEBRA_INI = .TRUE.
      IF(FIRST) THEN
C-
C- Zero statistics
C-
        NUM_EVENT_INPUT = 0
        NUM_EVENT_OUTPUT = 0
        NUM_ZEBRA_ERR = 0
        NUM_TSUM_ERR = 0
C-
C- Read RCP parameters.  First read from VERIFY_ZEBRA_RCP.
C-
        CALL EZPICK_NOMSG('VERIFY_ZEBRA_RCP', IER)
        IF(IER.NE.0)THEN
          CALL INRCP('VERIFY_ZEBRA_RCP', IER)
          CALL EZPICK_NOMSG('VERIFY_ZEBRA_RCP', IER)
        ENDIF
        IF(IER.EQ.0)CALL EZGET('DO_VERIFY_ZEBRA', DO_VERIFY_ZEBRA, IER)
        IF(IER.EQ.0. .AND. DO_VERIFY_ZEBRA) THEN
          IF(IER.EQ.0)CALL EZGET('ZEBRA_ERRORS_FATAL',
     &      ZEBRA_ERRORS_FATAL, IER)
          IF(IER.EQ.0)CALL EZGET('CHECK_BANKS', CHECK_BANKS, IER)
          IF(IER.EQ.0)CALL EZGET('CHECK_LINKS', CHECK_LINKS, IER)
          IF(IER.EQ.0)CALL EZGET('CHECK_STORE', CHECK_STORE, IER)
          IF(IER.EQ.0)CALL EZGET('CHECK_UP_LINKS', CHECK_UP_LINKS, IER)
          IF(IER.EQ.0)CALL EZGET('CHECK_TSUM', CHECK_TSUM, IER)
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in VERIFY_ZEBRA_RCP',
     &    'VERIFY_ZEBRA',' ','F')
        CHOPT = ' '
        IF(ZEBRA_ERRORS_FATAL)CHOPT = 'F'//CHOPT
        IF(CHECK_BANKS)CHOPT = 'C'//CHOPT
        IF(CHECK_LINKS)CHOPT = 'L'//CHOPT
        IF(CHECK_STORE)CHOPT = 'S'//CHOPT
        IF(CHECK_UP_LINKS)CHOPT = 'U'//CHOPT
        FIRST=.FALSE.
      ENDIF
      GO TO 999
 
      ENTRY VERIFY_ZEBRA_END()
C-
C- Job summary entry point
C-
      LUN = SSUNIT()
      PRINT 500, NUM_EVENT_INPUT, NUM_EVENT_OUTPUT, 
     &  NUM_ZEBRA_ERR, NUM_TSUM_ERR
      WRITE(LUN,500)NUM_EVENT_INPUT, NUM_EVENT_OUTPUT, 
     &  NUM_ZEBRA_ERR, NUM_TSUM_ERR
 500  FORMAT(/' VERIFY_ZEBRA package statistics'/
     &  /1X,I8,' Events processed'
     &  /1X,I8,' Events selected'
     &  /1X,I8,' Events with ZEBRA errors'
     &  /1X,I8,' Events with TSUM errors'/)
 999  RETURN
      END
