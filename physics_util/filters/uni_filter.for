      FUNCTION UNI_FILTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Call the functions provided by each Physics
C-      group and make a decision to keep each event based on
C-      the OR of all the filters.
C-
C-   Entry points:
C-      UNI_FILTER_BOJ Beginning of job tasks
C-      UNI_FILTER_EOJ End of job tasks
C-      OMNI_FILTER_BOJ for use with STREAM FILTER package. This inialization
C-                      must be placed before STREAM_FILTER_INI
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-DEC-1992   Lee Lueking
C-   Modified 30-APR-1992   Lee Lueking Added control through RCP parameters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL UNI_FILTER,UNI_FILTER_BOJ,UNI_FILTER_EOJ,OMNI_FILTER_BOJ
C      EXTERNAL UNI_FILTER,UNI_FILTER_BOJ,UNI_FILTER_EOJ
      INTEGER NFILT
      PARAMETER (NFILT=20) !20 FILTER FUNCTIONS
      LOGICAL FILTER(NFILT)
      LOGICAL PRE_FIX_FLAG,PACKAGE_OFF,FILTER_FLAG(NFILT),VERIFY
      CHARACTER*40 MSG
      CHARACTER*20 FILTER_NAMES(NFILT),DISABLED_FILTERS(NFILT)
      INTEGER FILTER_BITS,NDISABLED
      REAL TIME_SUM_EVENT,TIME_SQUARED_SUM_EVENT,TIME_EVENT
      REAL AVERAGE_TIME_EVENT,SECNDS
      REAL TIME_SUM(NFILT),TIME_SQUARED_SUM(NFILT),NUM_CALLS,TIME
      REAL ELAPSED_TIME
      REAL AVERAGE_TIME,STD_DEV_TIME
      REAL CORRELATION(NFILT,NFILT)
      INTEGER I,J,IER
      INTEGER IND_RGE,IND_QCD
      INTEGER SSUNIT,LUN
      INTEGER EVONUM,RUNNO
      REAL NUM_RGE,NUM_QCD
      LOGICAL KEEP_RGE,KEEP_QCD
      LOGICAL FIRST/.TRUE./
      REAL SUMMARY(20)
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C TYPE THE FILTERS
C----------------------------------------------------------------------
      LOGICAL
     &                  QCDJETS,                !QCD            1 Disabled
     &                  QCD_GAMMA,              !QCD            2
     &                  EL2_B,                  !B              3
     &                  MU1_B,                  !B              4 Disabled
     &                  MU2_B,                  !B              5
     &                  MU3_B,                  !B              6
     &                  ELF_W,                  !WZ-TOP         7
     &                  ELF_Z,                  !WZ-TOP         8
     &                  MU1_WZT,                !WZ-TOP         9
     &                  MU2_WZT,                !WZ-TOP         10
     &                  TOP_JETS,               !TOP            11
     &                  NP_LQ_2EM,              !NEW PHE        12
     &                  NP_LQ_ENU,              !NEW PHE        13
     &                  NP_LSS_SELECT,          !NEW PHE        14
     &                  NP_SCALAR,              !NEW PHE        15
     &                  NP_SQGL,                !NEW PHE        16
     &                  TOP_EMU,                !TOP            17
     &                  TOP_MUJ,                !TOP            18
     &                  NP_MSP,                 !NEW PHE        19
     &                  NP_TAU                  !NEW PHE        20
C----------------------------------------------------------------------
C Data in the filter names
C----------------------------------------------------------------------
      DATA FILTER_NAMES/
     &                  'QCDJETS',              !QCD            1 Disabled
     &                  'QCD_GAMMA',            !QCD            2
     &                  'EL2_B',                !B              3
     &                  'MU1_B',                !B              4 Disabled
     &                  'MU2_B',                !B              5
     &                  'MU3_B',                !B              6
     &                  'ELF_W',                !WZ-TOP         7
     &                  'ELF_Z',                !WZ-TOP         8
     &                  'MU1_WZT',              !WZ-TOP         9
     &                  'MU2_WZT',              !WZ-TOP         10
     &                  'TOP_JETS',             !TOP            11
     &                  'NP_LQ_2EM',            !NEW PHE        12
     &                  'NP_LQ_ENU',            !NEW PHE        13
     &                  'NP_LSS_SELECT',        !NEW PHE        14
     &                  'NP_SCALAR',            !NEW PHE        15
     &                  'NP_SQGL',              !NEW PHE        16
     &                  'TOP_EMU',              !TOP            17
     &                  'TOP_MUJ',              !TOP            18
     &                  'NP_MSP',               !NEW PHE        19
     &                  'NP_TAU'                !NEW PHE        20
     &/
C----------------------------------------------------------------------
C Normal execution
C----------------------------------------------------------------------
      LUN = SSUNIT()
      UNI_FILTER=.TRUE.
      NUM_CALLS=NUM_CALLS+1
      TIME_EVENT=SECNDS(0.)
C----------------------------------------------------------------------
C Package control
C----------------------------------------------------------------------
      IF (PACKAGE_OFF)RETURN
C----------------------------------------------------------------------
C PRE FILTER FIXES
C----------------------------------------------------------------------
      IF (PRE_FIX_FLAG)THEN
C----------------------------------------------------------------------
C EM fix
C----------------------------------------------------------------------

        CALL EMFIX

      ENDIF
C----------------------------------------------------------------------
C Go through all of the filters, keep track of times
C----------------------------------------------------------------------
C
C  filter 1 (switchs to turn off any particular filter are in UNI_FILTER.RCP)
C
      IF(FILTER_FLAG(1))THEN
        TIME=SECNDS(0.)
C       FILTER(1)=QCDJETS()
        FILTER(1)=.false.
        TIME=SECNDS(TIME)
        TIME_SUM(1)=TIME_SUM(1)+TIME
        TIME_SQUARED_SUM(1)=TIME_SQUARED_SUM(1)+TIME**2
      ELSE
        FILTER(1)=.false.
      ENDIF
C
C  filter 2
C
      IF(FILTER_FLAG(2))THEN
        TIME=SECNDS(0.)
        FILTER(2)=QCD_GAMMA()
        TIME=SECNDS(TIME)
        TIME_SUM(2)=TIME_SUM(2)+TIME
        TIME_SQUARED_SUM(2)=TIME_SQUARED_SUM(2)+TIME**2
      ELSE
        FILTER(2)=.false.
      ENDIF
C
C  filter 3
C
      IF(FILTER_FLAG(3))THEN
        TIME=SECNDS(0.)
        FILTER(3)=EL2_B()
        TIME=SECNDS(TIME)
        TIME_SUM(3)=TIME_SUM(3)+TIME
        TIME_SQUARED_SUM(3)=TIME_SQUARED_SUM(3)+TIME**2
      ELSE
        FILTER(3)=.false.
      ENDIF
C
C  filter 4
C
      IF(FILTER_FLAG(4))THEN
        TIME=SECNDS(0.)
C       FILTER(4)=MU1_B() !disabled
        FILTER(4)=.false.
        TIME=SECNDS(TIME)
        TIME_SUM(4)=TIME_SUM(4)+TIME
        TIME_SQUARED_SUM(4)=TIME_SQUARED_SUM(4)+TIME**2
      ELSE
        FILTER(4)=.false.
      ENDIF
C
C  filter 5
C
      IF(FILTER_FLAG(5))THEN
        TIME=SECNDS(0.)
        FILTER(5)=MU2_B()
        TIME=SECNDS(TIME)
        TIME_SUM(5)=TIME_SUM(5)+TIME
        TIME_SQUARED_SUM(5)=TIME_SQUARED_SUM(5)+TIME**2
      ELSE
        FILTER(5)=.false.
      ENDIF
C
C  filter 6
C
      IF(FILTER_FLAG(6))THEN
        TIME=SECNDS(0.)
        FILTER(6)=MU3_B()
        TIME=SECNDS(TIME)
        TIME_SUM(6)=TIME_SUM(6)+TIME
        TIME_SQUARED_SUM(6)=TIME_SQUARED_SUM(6)+TIME**2
      ELSE
        FILTER(6)=.false.
      ENDIF
C
C  filter 7
C
      IF(FILTER_FLAG(7))THEN
        TIME=SECNDS(0.)
        FILTER(7)=ELF_W()
        TIME=SECNDS(TIME)
        TIME_SUM(7)=TIME_SUM(7)+TIME
        TIME_SQUARED_SUM(7)=TIME_SQUARED_SUM(7)+TIME**2
      ELSE
        FILTER(7)=.false.
      ENDIF
C
C  filter 8
C
      IF(FILTER_FLAG(8))THEN
        TIME=SECNDS(0.)
        FILTER(8)=ELF_Z()
        TIME=SECNDS(TIME)
        TIME_SUM(8)=TIME_SUM(8)+TIME
        TIME_SQUARED_SUM(8)=TIME_SQUARED_SUM(8)+TIME**2
      ELSE
        FILTER(8)=.false.
      ENDIF
C
C  filter 9
C
      IF(FILTER_FLAG(9))THEN
        TIME=SECNDS(0.)
        FILTER(9)= MU1_WZT()
        TIME=SECNDS(TIME)
        TIME_SUM(9)=TIME_SUM(9)+TIME
        TIME_SQUARED_SUM(9)=TIME_SQUARED_SUM(9)+TIME**2
      ELSE
        FILTER(9)=.false.
      ENDIF
C
C  filter 10
C
      IF(FILTER_FLAG(10))THEN
        TIME=SECNDS(0.)
        FILTER(10)=MU2_WZT()
        TIME=SECNDS(TIME)
        TIME_SUM(10)=TIME_SUM(10)+TIME
        TIME_SQUARED_SUM(10)=TIME_SQUARED_SUM(10)+TIME**2
      ELSE
        FILTER(10)=.false.
      ENDIF
C
C  filter 11
C
      IF(FILTER_FLAG(11))THEN
        TIME=SECNDS(0.)
        FILTER(11)=TOP_JETS()
        TIME=SECNDS(TIME)
        TIME_SUM(11)=TIME_SUM(11)+TIME
        TIME_SQUARED_SUM(11)=TIME_SQUARED_SUM(11)+TIME**2
      ELSE
        FILTER(11)=.false.
      ENDIF
C
C  filter 12
C
      IF(FILTER_FLAG(12))THEN
        TIME=SECNDS(0.)
        FILTER(12)=NP_LQ_2EM()
        TIME=SECNDS(TIME)
        TIME_SUM(12)=TIME_SUM(12)+TIME
        TIME_SQUARED_SUM(12)=TIME_SQUARED_SUM(12)+TIME**2
      ELSE
        FILTER(12)=.false.
      ENDIF
C
C  filter 13
C
      IF(FILTER_FLAG(13))THEN
        TIME=SECNDS(0.)
        FILTER(13)=NP_LQ_ENU()
        TIME=SECNDS(TIME)
        TIME_SUM(13)=TIME_SUM(13)+TIME
        TIME_SQUARED_SUM(13)=TIME_SQUARED_SUM(13)+TIME**2
      ELSE
        FILTER(13)=.false.
      ENDIF
C
C  filter 14
C
      IF(FILTER_FLAG(14))THEN
        TIME=SECNDS(0.)
        FILTER(14)=NP_LSS_SELECT()
        TIME=SECNDS(TIME)
        TIME_SUM(14)=TIME_SUM(14)+TIME
        TIME_SQUARED_SUM(14)=TIME_SQUARED_SUM(14)+TIME**2
      ELSE
        FILTER(14)=.false.
      ENDIF
C
C  filter 15
C
      IF(FILTER_FLAG(15))THEN
        TIME=SECNDS(0.)
        FILTER(15)= NP_SCALAR()
        TIME=SECNDS(TIME)
        TIME_SUM(15)=TIME_SUM(15)+TIME
        TIME_SQUARED_SUM(15)=TIME_SQUARED_SUM(15)+TIME**2
      ELSE
        FILTER(15)=.false.
      ENDIF
C
C  filter 16
C
      IF(FILTER_FLAG(16))THEN
        TIME=SECNDS(0.)
        FILTER(16)=NP_SQGL()
        TIME=SECNDS(TIME)
        TIME_SUM(16)=TIME_SUM(16)+TIME
        TIME_SQUARED_SUM(16)=TIME_SQUARED_SUM(16)+TIME**2
      ELSE
        FILTER(16)=.false.
      ENDIF
C
C  filter 17
C
      IF(FILTER_FLAG(17))THEN
        TIME=SECNDS(0.)
        FILTER(17)=TOP_EMU()
        TIME=SECNDS(TIME)
        TIME_SUM(17)=TIME_SUM(17)+TIME
        TIME_SQUARED_SUM(17)=TIME_SQUARED_SUM(17)+TIME**2
      ELSE
        FILTER(17)=.false.
      ENDIF
C
C  filter 18
C
      IF(FILTER_FLAG(18))THEN
        TIME=SECNDS(0.)
        FILTER(18)=TOP_MUJ()
        TIME=SECNDS(TIME)
        TIME_SUM(18)=TIME_SUM(18)+TIME
        TIME_SQUARED_SUM(18)=TIME_SQUARED_SUM(18)+TIME**2
      ELSE
        FILTER(18)=.false.
      ENDIF
C
C  filter 19
C
      IF(FILTER_FLAG(19))THEN
        TIME=SECNDS(0.)
        FILTER(19)=NP_MSP()
        TIME=SECNDS(TIME)
        TIME_SUM(18)=TIME_SUM(19)+TIME
        TIME_SQUARED_SUM(19)=TIME_SQUARED_SUM(19)+TIME**2
      ELSE
        FILTER(19)=.false.
      ENDIF
C
C  filter 20
C
      IF(FILTER_FLAG(20))THEN
        TIME=SECNDS(0.)
        FILTER(20)=NP_TAU()
        TIME=SECNDS(TIME)
        TIME_SUM(20)=TIME_SUM(20)+TIME
        TIME_SQUARED_SUM(20)=TIME_SQUARED_SUM(20)+TIME**2
      ELSE
        FILTER(20)=.false.
      ENDIF
C
C Time stats for all filters
C
      TIME_EVENT=SECNDS(TIME_EVENT)
      TIME_SUM_EVENT=TIME_SUM_EVENT+TIME_EVENT
      TIME_SQUARED_SUM_EVENT=TIME_SQUARED_SUM_EVENT+TIME_EVENT**2

C IF THIS IS THE FIRST TIME THROUGH, ZERO OUT TIMES
      IF(FIRST)THEN
        FIRST=.FALSE.
        DO I=1,NFILT
          TIME_SUM(I)=0.0
          TIME_SQUARED_SUM(I)=0.0
        ENDDO
        TIME_SUM_EVENT=0.0
        TIME_SQUARED_SUM_EVENT=0.0
        ELAPSED_TIME=SECNDS(0.)
      ENDIF
C
C----------------------------------------------------------------------
C Decide whether to keep the event
C and store correlation statistics
C----------------------------------------------------------------------
C

      KEEP_QCD=.FALSE.
      KEEP_RGE=.FALSE.
C     Filter bit word, one bit per filter.
      FILTER_BITS=0
      DO I=1,NFILT
C       Fill the filter bit word
        IF(FILTER(I))FILTER_BITS=FILTER_BITS.OR.2**(I-1)
        IF (I.EQ.1)THEN
C          KEEP_QCD=FILTER(I)
        ELSE
          KEEP_RGE=KEEP_RGE.OR.FILTER(I)
        ENDIF
        DO J=1,NFILT
          IF(FILTER(I).AND.FILTER(J))
     &             CORRELATION(I,J)=CORRELATION(I,J)+1
        ENDDO
      ENDDO
C     Put the filter bit word into the header
      IQ(LHEAD+10)=FILTER_BITS
C     Mark the event to be streamed to QCD stream
C      IF(KEEP_QCD)THEN
C        CALL EVSET_STREAM(IND_QCD)
C        NUM_QCD=NUM_QCD+1
C      ENDIF
C     Mark the event to be streamed to RGE stream
      IF(KEEP_RGE)THEN
        CALL EVSET_STREAM(IND_RGE)
        CALL EVENT_LIST(RUNNO(),EVONUM(),'RGE',FILTER_BITS)
        NUM_RGE=NUM_RGE+1
      ENDIF


      RETURN
C----------------------------------------------------------------------
C Initialization
C----------------------------------------------------------------------
      ENTRY UNI_FILTER_BOJ
      UNI_FILTER_BOJ=.TRUE.
      LUN = SSUNIT()
C----------------------------------------------------------------------
C Get the RCP parameters to control this routine
C----------------------------------------------------------------------
      PACKAGE_OFF    =.FALSE.
      PRE_FIX_FLAG   =.TRUE.
      VERIFY         =.TRUE.
      CALL INRCP('UNI_FILTER_RCP',IER)       ! read in RCP file
      IF(IER.NE.0) THEN
        WRITE(6,53)
 53     FORMAT(' UNI_FILTER package, UNI_FILTER_BOJ'/
     &  'UNI_FILTER_RCP file not found; use defaults')
      ELSE
        CALL EZPICK('UNI_FILTER_RCP')
        CALL EZGET('PACKAGE_OFF',PACKAGE_OFF,IER)
        CALL EZGET('PRE_FIX_FLAG'   ,PRE_FIX_FLAG   ,IER)
        CALL EZGET('VERIFY'   ,VERIFY   ,IER)
        CALL EZ_GET_CHARS('DISABLED_FILTERS',NDISABLED,
     &                              DISABLED_FILTERS,IER)
        CALL EZRSET
      ENDIF
      DO I=1,NFILT
        FILTER_FLAG(I)=.TRUE.
        DO J=1,NDISABLED
          IF(DISABLED_FILTERS(J).EQ.FILTER_NAMES(I))THEN
            FILTER_FLAG(I)=.FALSE.
            WRITE(6,55)FILTER_NAMES(I)
            WRITE(LUN,55)FILTER_NAMES(I)
   55       FORMAT(' UNI_FILTER ',1A20,' filter disabled by RCP ',
     &          'switch')
          ENDIF
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C Package control
C----------------------------------------------------------------------
      IF (PACKAGE_OFF)THEN
        WRITE(6,*)'UNI_FILTER package has been turned off'
	RETURN
      ELSE
        CALL EVTWOS_MULT(.TRUE.)
        ELAPSED_TIME=SECNDS(0.)
        CALL EVGET_STREAM('RGE01',IND_RGE)
        CALL EVENT_LIST_BOJ(0,0,'RGE')
C       CALL EVGET_STREAM('QCD01',IND_QCD)
      ENDIF
C
      RETURN
C----------------------------------------------------------------------
C End of job stuff
C----------------------------------------------------------------------
      ENTRY UNI_FILTER_EOJ
      UNI_FILTER_EOJ=.TRUE.
C----------------------------------------------------------------------
C Package control
C----------------------------------------------------------------------
      IF (PACKAGE_OFF)RETURN
C----------------------------------------------------------------------
C CLOSE the event lists
C----------------------------------------------------------------------
      CALL EVENT_LIST_EOJ(0,0,'   ')
C----------------------------------------------------------------------
C Completion, create summary file of average times per filter,
C filter correlations and do bookkeeping for PDB, etc.
C (For filter time stats, the first event is not included, thus
C use NUM_CALLS-1 .
C----------------------------------------------------------------------
C    ( DEAL WITH FILES WITH ONE OR 0 EVENTS)
      IF(NUM_CALLS.LT.2)THEN
	WRITE(LUN,*)'Number of events less than 2, stats unreliable'
	NUM_CALLS=2
      ENDIF
      WRITE(LUN,*)
      WRITE(LUN,*)
     &    '******* UNI_FILTER statistics ************************'
      WRITE(LUN,*)
      ELAPSED_TIME=SECNDS(ELAPSED_TIME)
      WRITE(LUN,*)'Total number of events processed:',NUM_CALLS
      WRITE(LUN,*)'Really Good Event stream events :',NUM_RGE,
     &    NUM_RGE/NUM_CALLS*100,'% of total.'
      WRITE(LUN,*)'Reduced JET stream (QCD) events :',NUM_QCD,
     &    NUM_QCD/NUM_CALLS*100,'% of total.'
      WRITE(LUN,*)'Elapsed time (sec)              :',ELAPSED_TIME
      WRITE(LUN,*)'Total time per event (sec/event):',
     &    ELAPSED_TIME/(NUM_CALLS-1)
      AVERAGE_TIME=TIME_SUM_EVENT/(NUM_CALLS-1)
      STD_DEV_TIME=(TIME_SQUARED_SUM_EVENT)/(NUM_CALLS-1)
     &    -AVERAGE_TIME**2
      IF(STD_DEV_TIME.GT.0)THEN
        STD_DEV_TIME=SQRT(STD_DEV_TIME)
      ELSE
        STD_DEV_TIME=0.
      ENDIF
      AVERAGE_TIME_EVENT=AVERAGE_TIME
      WRITE(LUN,*)'Filter time average and std dev.:',
     &    AVERAGE_TIME,STD_DEV_TIME
      WRITE(LUN,*)
      WRITE(LUN,*)'Filter selection normalized to total events (%)'
      WRITE(LUN,*)
      WRITE(LUN,*)' Filter      Select(%)  Error'
      WRITE(LUN,100)
  100 FORMAT(' ',28('-'))
      DO I=1,NFILT
        WRITE(LUN,101)FILTER_NAMES(I)(1:12),
     &                 (CORRELATION(I,I)/num_calls*100),
     &                 SQRT(CORRELATION(I,I))/num_calls*100

      ENDDO
  101 FORMAT(' ',A12,2F8.4)
      WRITE(LUN,100)
      IF(VERIFY)THEN
        WRITE(LUN,*)
        WRITE(LUN,*)'Filter correlations normalized to total events (%)'
        WRITE(LUN,*)
        WRITE(LUN,201)(FILTER_NAMES(I)(1:5),I=1,NFILT)
  201   FORMAT(' FILTERS     ',<NFILT>(1X,A5))
        WRITE(LUN,200)
  200   FORMAT(' ',122('-'))
        DO I=1,NFILT
          WRITE(LUN,202)FILTER_NAMES(I)(1:12),
     &                 (CORRELATION(I,J)/num_calls*100.,J=1,NFILT)
        ENDDO
        WRITE(LUN,200)
  202   FORMAT(' ',A12,<NFILT>F6.2)
        WRITE(LUN,*)
        WRITE(LUN,*)'FILTER TIMES'
        WRITE(LUN,*)
        WRITE(LUN,301)
  301   FORMAT(' FILTER    AVERAGE  STD Dev. % of total')
        WRITE(LUN,302)
  302   FORMAT(' --------------------------------------')
        DO I=1,NFILT
          AVERAGE_TIME=TIME_SUM(I)/NUM_CALLS
          STD_DEV_TIME=(TIME_SQUARED_SUM(I))/(NUM_CALLS-1)
     &      -AVERAGE_TIME**2
          IF(STD_DEV_TIME.GT.0)THEN
            STD_DEV_TIME=SQRT(STD_DEV_TIME)
          ELSE
            STD_DEV_TIME=0.
          ENDIF
	  IF(AVERAGE_TIME_EVENT.EQ.0)AVERAGE_TIME_EVENT=1.0
          WRITE(LUN,300)FILTER_NAMES(I),AVERAGE_TIME,STD_DEV_TIME,
     &              AVERAGE_TIME/AVERAGE_TIME_EVENT*100.
        ENDDO
        WRITE(LUN,302)
  300   FORMAT(' ',A10,2F8.5,'  (',F6.3,'%)')

C----------------------------------------------------------------------
C Call the EOJ entry points and dump final stats
C----------------------------------------------------------------------
        WRITE(LUN,*)
        WRITE(LUN,*)'EOJ SUMMARIES'
        WRITE(LUN,*)
        WRITE(LUN,403)(I,I+10,I=1,10)
  403   FORMAT(' FILTER    ',10('    ',I2,'/',I2,'   '))
        WRITE(LUN,402)
  402   FORMAT(' ',132('-'))

        IF(FILTER_FLAG(1))THEN
c      CALL  QCDJETS_EOJ(SUMMARY)               !QCD            1
c      WRITE(LUN,400)FILTER_NAMES(1),SUMMARY
        ENDIF
        IF(FILTER_FLAG(2))THEN
c      CALL QCD_GAMMA_EOJ(SUMMARY)              !QCD            2
c      WRITE(LUN,400)FILTER_NAMES(2),SUMMARY
        ENDIF
        IF(FILTER_FLAG(3))THEN
c      CALL EL2_B_EOJ(SUMMARY)                  !B              3
c      WRITE(LUN,400)FILTER_NAMES(3),SUMMARY
        ENDIF
        IF(FILTER_FLAG(4))THEN
c      CALL MU1_B_EOJ(SUMMARY)                  !B              4
c      WRITE(LUN,400)FILTER_NAMES(4),SUMMARY
        ENDIF
        IF(FILTER_FLAG(5))THEN
c      CALL MU2_B_EOJ(SUMMARY)                  !B              5
c      WRITE(LUN,400)FILTER_NAMES(5),SUMMARY
        ENDIF
        IF(FILTER_FLAG(6))THEN
c      CALL MU3_B_EOJ(SUMMARY)                  !B              6
c      WRITE(LUN,400)FILTER_NAMES(6),SUMMARY
        ENDIF
        IF(FILTER_FLAG(7))THEN
          CALL ELF_W_EOJ(SUMMARY)                  !WZ-TOP         7
          WRITE(LUN,400)FILTER_NAMES(7),SUMMARY
        ENDIF
        IF(FILTER_FLAG(8))THEN
          CALL ELF_Z_EOJ(SUMMARY)                  !WZ-TOP         8
          WRITE(LUN,400)FILTER_NAMES(8),SUMMARY
        ENDIF
        IF(FILTER_FLAG(9))THEN
          CALL MU1_WZT_EOJ(SUMMARY)                !WZ-TOP         9
          WRITE(LUN,400)FILTER_NAMES(9),SUMMARY
        ENDIF
        IF(FILTER_FLAG(10))THEN
          CALL MU2_WZT_EOJ(SUMMARY)                !WZ-TOP         10
          WRITE(LUN,400)FILTER_NAMES(10),SUMMARY
        ENDIF
        IF(FILTER_FLAG(11))THEN
c      CALL TOP_JETS_EOJ(SUMMARY)               !TOP            11
c      WRITE(LUN,400)FILTER_NAMES(11),SUMMARY
        ENDIF
        IF(FILTER_FLAG(12))THEN
          CALL NP_LQ_2EM_EOJ(SUMMARY)              !NEW PHE        12
          WRITE(LUN,400)FILTER_NAMES(12),SUMMARY
        ENDIF
        IF(FILTER_FLAG(13))THEN
          CALL NP_LQ_ENU_EOJ(SUMMARY)              !NEW PHE        13
          WRITE(LUN,400)FILTER_NAMES(13),SUMMARY
        ENDIF
        IF(FILTER_FLAG(14))THEN
          CALL NP_LSS_SELECT_EOJ(SUMMARY)            !NEW PHE        14
          WRITE(LUN,400)FILTER_NAMES(14),SUMMARY
        ENDIF
        IF(FILTER_FLAG(15))THEN
          CALL NP_SCALAR_EOJ(SUMMARY)              !NEW PHE        15
          WRITE(LUN,400)FILTER_NAMES(15),SUMMARY
        ENDIF
        IF(FILTER_FLAG(16))THEN
          CALL NP_SQGL_EOJ(SUMMARY)                !NEW PHE        16
          WRITE(LUN,400)FILTER_NAMES(16),SUMMARY
        ENDIF
        IF(FILTER_FLAG(17))THEN
C      CALL TOP_MUE_EOJ(SUMMARY)                !TOP            17
C      WRITE(LUN,400)FILTER_NAMES(17),SUMMARY
        ENDIF
        IF(FILTER_FLAG(18))THEN
          CALL TOP_MUJ_EOJ(SUMMARY)                !TOP        18
          WRITE(LUN,400)FILTER_NAMES(18),SUMMARY
        ENDIF
        IF(FILTER_FLAG(19))THEN
          CALL NP_MSP_EOJ(SUMMARY)                !NEW PHE        19
          WRITE(LUN,400)FILTER_NAMES(19),SUMMARY
        ENDIF
        IF(FILTER_FLAG(20))THEN
          CALL NP_TAU_EOJ(SUMMARY)                !New Phe        20
          WRITE(LUN,400)FILTER_NAMES(20),SUMMARY
        ENDIF
        WRITE(LUN,402)
  400   FORMAT(' ',A10,10G12.5/'           ',10G12.5)
      ENDIF
  999 RETURN
      END
