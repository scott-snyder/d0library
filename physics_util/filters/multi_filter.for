      FUNCTION MULTI_FILTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Call the functions provided by each Physics
C-      group and make a decision to send events to multi-physics
C-      streams.
C-
C-   Entry points:
C-      MULTI_FILTER_BOJ Beginning of job tasks
C-      MULTI_FILTER_EOJ End of job tasks
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  03-MAR-1993   Lee Lueking
C-   Updated  15-AUG-1993   Lee Lueking Generalized to have complete
C-			    RCP control of filters and stream definitions.
C-   Updated   1-SEP-1993   K. Wyatt Merritt  Added  check on CAPH stack
C-                              after each filter routine to catch CAPH
C-                              problems
C-   Updated   3-SEP-1993   Lee Lueking Added more filters. All filters above
C-                          number 31 show up in filter word bit 31.
C-
C-   Updated   15-OCT-1993  Lee Lueking Added a PASS_ALL filter option which,
C-                          when on, will
C-                          pass all of the events, it is a dummy filter.
C-   Updated   28-JAN-1994  Lee Lueking Added MU1_B filter to list.
C-   Updated   17-FEB-1994  Lee Lueking Added D0DADs.
C-   Updated   24-FEB-1994  Added new filters
C-   Updated   22-Apr-1994  Fixed D0DAD bug, must use hook POST_WRITE to call
C-                          MULTI_FILTER_POST_WRITE entry.
C-                          Added ONSTR_FILT function,
C-                          Added filter NP_MUNU
C-   Updated   10-MAY-1994  Changed header words to be 31 and 32 for filter
C-                          bits.
C-   Updates    2-NOV-1994  Added filter NEW_GAM_JET.
C-   Updates      Nov-1995  Added VERTEX_FILTER
C-   UPDATES   13-DEC-1996  Added MUJ_B() filter
C-   UPDATES   17-MAR-1997  Removed some obsolete declarations,added UPCASE call for 
C-                          GST and ASTS group strings to avoid confusion in future.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MULTI_FILTER,MULTI_FILTER_BOJ,MULTI_FILTER_EOJ
      LOGICAL MULTI_FILTER_POST_WRITE
      INTEGER MAX_FILTER,MAX_STREAM
      PARAMETER (MAX_FILTER=64) !Number of FILTER FUNCTIONS
      PARAMETER (MAX_STREAM=20) !Number of STREAMS
      LOGICAL FILTER(MAX_FILTER),STREAM(MAX_FILTER,MAX_STREAM)
      LOGICAL FILTER_FLAG(MAX_FILTER),PACKAGE_OFF,PRE_FIX_FLAG,VERIFY
      CHARACTER*20 FILTER_NAMES(MAX_FILTER),DISABLED_FILTERS(MAX_FILTER)
      CHARACTER*20 STREAM_NAMES(MAX_STREAM)
      CHARACTER*20 STREAM_DEFINITION(MAX_FILTER,MAX_STREAM)
      CHARACTER*20 D0DAD_STREAMS(MAX_STREAM)
      CHARACTER*26 D0DAD_NAMES(MAX_STREAM,2)
      CHARACTER*80 MSG
      CHARACTER*80 EMSG
      INTEGER NFILTERS(MAX_FILTER),NSTREAM,NSTREAMS,NDISABLED
      INTEGER ND0DAD_STREAMS
      INTEGER FILTER_BITS
      INTEGER D0DAD_FILTER_BITS(2)
      REAL TIME_SUM_EVENT,TIME_SQUARED_SUM_EVENT,TIME_EVENT
      REAL AVERAGE_TIME_EVENT
      REAL TIME_SUM(MAX_FILTER),TIME_SQUARED_SUM(MAX_FILTER),NUM_CALLS,
     &  TIME
      REAL ELAPSED_TIME,SECNDS
      REAL AVERAGE_TIME,STD_DEV_TIME
      REAL CORRELATION(MAX_FILTER,MAX_FILTER)
      INTEGER I,J,K,L,IER,IBIT,IOS
c,LW !removed 3-17-97 LL
      INTEGER IND_STREAM(MAX_STREAM)
      REAL    NUM_STREAM(MAX_STREAM)
      LOGICAL KEEP_STREAM(MAX_STREAM)
      INTEGER SSUNIT,LUN,LUN2
      INTEGER EVONUM,RUNNO
      LOGICAL CHOTINFO,LISTS_ON
      INTEGER ZBREC,ZBBYT
      LOGICAL D0DAD_FOR_INPUT,D0DAD_FOR_OUTPUT(MAX_STREAM)
      LOGICAL FIRST/.TRUE./
      LOGICAL CRASH_ON_ZERO_EVENTS
c      REAL SUMMARY(20)
      INTEGER VRECO,PASS,RVER,RVER_OLD,LHSTR,GZHSTR
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C Variables for off-line stream filter
C----------------------------------------------------------------------
      INTEGER B_GROUP,TOP_GROUP,WZ_GROUP,NP_GROUP,QCD_GROUP,ALL_GROUP
      CHARACTER*3 ASTS(10,6) ! Allowed Stream Types
      CHARACTER*3 GST,GET_STREAM_TYPE
      CHARACTER*10 GROUP_NAMES(6)
c,PHYSICS_GROUPS(6)
      LOGICAL OSF
      INTEGER NGROUPS,GROUP,NGROUP,NONFILT
c      DATA PHYSICS_GROUPS/'B_GROUP','TOP_GROUP','WZ_GROUP','NP_GROUP',
c     &  'QCD_GROUP','ALL_GROUP'/
      DATA ASTS/60*'XXX'/
C----------------------------------------------------------------------
C TYPE THE FILTERS
C----------------------------------------------------------------------
      LOGICAL
     &QCD_GAMMA,              			!QCD            1
     &MU1_B,MU_B,MU2_B,       			!B              2-4
     &NP_LQ_2EM,NP_LQ_ENU,NP_LSS_SELECT,
     &NP_SCALAR,NP_SQGL,NP_MSP,NP_TAU,
     &NP_MUNU,NP_MET,NP_WRIGHT,              !NEW            5-14
     &TOP_EE,TOP_EJET,TOP_EMU,TOP_JETS,
     &TOP_MUJET,TOP_MUMU,TOP_TOP_EE,
     &TOP_TOP_EJET,TOP_TOP_EMU,TOP_TOP_JETS,
     &TOP_TOP_MUJET,TOP_TOP_MUMU,               !TOP            15-26
     &FAKE_E_CANDIDATE,                         !TOP            27
     &ELF_W,ELF_Z,MU1_WZT,MU2_WZT,              !WZ-TOP         28-31
     &WZ_ELF_MU,ELF_MED,                   	!WZ             32-33
     &QCD_STA,QCD_GAP,QCD_JJJ,
     &QCD_QJT,QCD_NTR,                     	!QCD            34-38
     &TOP_BKG,                			!TOP            39
     &NP_CLEAN_TAU,NP_SCALAR_TIGHT,NP_SQGL_TIGHT,!NP HAD        40-42
     &NP_LQ_2EM_TIGHT,LQNUE,LQNN,NP_LSS_TIGHT,
     &NP_WRIGHT_TIGHT,NP_MULQ_TIGHT,		!NPLEP		43-48
     &ELF ,MINBIAS,MU1_FILT,PI0_FILTER,
     &TWOJET_FILTER,THREEJET_FILTER,           !Misc.          49-54
     &EMAG_JETS,NP_MULQ,TAU_FILTER,
     &TOP_SGL,TOP_TOP_SGL,NEW_GAM_JET,VERTEX_FILTER,MUJ_B
c,SPR0,
c     &PASS_ALL
C----------------------------------------------------------------------
C Data in the filter names
C----------------------------------------------------------------------
      DATA FILTER_NAMES/
     &'QCD_GAMMA',              		!QCD            1
     &'MU1_B','MU_B','MU2_B',       		!B              2-4
     &'NP_LQ_2EM','NP_LQ_ENU','NP_LSS_SELECT',
     &'NP_SCALAR','NP_SQGL','NP_MSP','NP_TAU',
     &'NP_MUNU','NP_MET','NP_WRIGHT',            !NEW            5-14
     &'TOP_EE','TOP_EJET','TOP_EMU','TOP_JETS',
     &'TOP_MUJET','TOP_MUMU','TOP_TOP_EE',
     &'TOP_TOP_EJET','TOP_TOP_EMU','TOP_TOP_JETS',
     &'TOP_TOP_MUJET','TOP_TOP_MUMU',            !TOP            15-26
     &'FAKE_E_CANDIDATE',                       !TOP            27
     &'ELF_W','ELF_Z','MU1_WZT','MU2_WZT',      !WZ-TOP         28-31
     &'WZ_ELF_MU','ELF_MED',                   	!WZ             32-33
     &'QCD_STA','QCD_GAP','QCD_JJJ',
     &'QCD_QJT','QCD_NTR',                     	!QCD            34-38
     &'TOP_BKG',                		!TOP            39
     &'NP_CLEAN_TAU','NP_SCALAR_TIGHT','NP_SQGL_TIGHT',!NP HAD  40-42
     &'NP_LQ_2EM_TIGHT','LQNUE','LQNN','NP_LSS_TIGHT',
     &'NP_WRIGHT_TIGHT','NP_MULQ_TIGHT',	!NPLEP		42-48
     &'ELF','MINBIAS','MU1_FILT','PI0_FILTER',
     &'TWOJET_FILTER','THREEJET_FILTER',         !Misc.  49-54
     &'EMAG_JETS','NP_MULQ','TAU_FILTER',
     &'TOP_SGL','TOP_TOP_SGL','NEW_GAM_JET','VERTEX_FILTER',
     &'MUJ_B','SPR0',
     &'PASS_ALL'
     &/
C----------------------------------------------------------------------
C This is the Online Stream Filter function definition
C   ST is the stream type from GET_STREAM_TYPE. ASTS are
C   the allowed stream types as a function of group.
C----------------------------------------------------------------------
      OSF(GROUP)=(INDEX(ASTS(1,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(2,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(3,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(4,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(5,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(6,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(7,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(8,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(9,GROUP),GST).NE.0).OR.
     &           (INDEX(ASTS(10,GROUP),GST).NE.0)
C----------------------------------------------------------------------
C Normal execution
C----------------------------------------------------------------------
      LUN = SSUNIT()
      MULTI_FILTER=.TRUE.
      NUM_CALLS=NUM_CALLS+1
      TIME_EVENT=SECNDS(0.)
C----------------------------------------------------------------------
C Package flag
C----------------------------------------------------------------------
      IF (PACKAGE_OFF)RETURN
C----------------------------------------------------------------------
C Calorimeter Hot Channel thing
C----------------------------------------------------------------------
      IF (CHOTINFO)THEN
C        CALL CHOTINFO_EVENT
      ENDIF
C----------------------------------------------------------------------
C PRE FILTER FIXES
C----------------------------------------------------------------------
      IF (PRE_FIX_FLAG)THEN
C----------------------------------------------------------------------
C EM fix
C----------------------------------------------------------------------
C        CALL EMFIX
      ENDIF
C----------------------------------------------------------------------
C CHECK THE RECO VERSION NUMBERS
C----------------------------------------------------------------------
      CALL RECO_VERSION(VRECO,PASS)
      RVER=VRECO*100+PASS
      WRITE(MSG,*)'V',RVER
      IF(FIRST)THEN
        CALL ERRMSG('Reco Version','MULTI_FILTER',MSG,'W')
      RVER_OLD=RVER
      CALL GTUNIT(200,LUN2,IER)
      CALL D0OPEN (LUN2,'MULTI_FILTER_SUMMARY','OFL',IER)
      CALL HSTRFL
      LHSTR=GZHSTR()
      CALL PRHSTR(LUN2,LHSTR,0,'ONE',0)
C      WRITE(LUN2,*)'PROCESSING SITE:'
C      WRITE(LUN2,*)'DATE:'
      ELSE
      ENDIF
      IF(RVER.NE.RVER_OLD)THEN
        CALL ERRMSG('Reco Version','MULTI_FILTER',MSG,'F')
      ENDIF
      RVER_OLD=RVER
C----------------------------------------------------------------------
C Go through all of the filters, keep track of times
C----------------------------------------------------------------------
C
C  filter 1
C
C   get the online stream name (first 3 characters of file name)
      GST=GET_STREAM_TYPE()
C   change stream name to upper case
      CALL UPCASE(GST,GST)
C
      DO I=1,MAX_FILTER
        IF(FILTER_FLAG(I))THEN
          TIME=SECNDS(0.)
          IF(I.EQ.1 )FILTER(I)=QCD_GAMMA()     .AND.OSF(QCD_GROUP)!QCD     1
          IF(I.EQ.2 )FILTER(I)=MU1_B()         .AND.OSF(  B_GROUP)!B       2
          IF(I.EQ.3 )FILTER(I)=MU_B()          .AND.OSF(  B_GROUP)!B       3
          IF(I.EQ.4 )FILTER(I)=MU2_B()         .AND.OSF(  B_GROUP)!B       4
          IF(I.EQ.5 )FILTER(I)=NP_LQ_2EM()     .AND.OSF( NP_GROUP)!NEW PHE 5
          IF(I.EQ.6 )FILTER(I)=NP_LQ_ENU()     .AND.OSF( NP_GROUP)!NEW PHE 6
          IF(I.EQ.7 )FILTER(I)=NP_LSS_SELECT() .AND.OSF( NP_GROUP)!NEW PHE 7
          IF(I.EQ.8 )FILTER(I)=NP_SCALAR()     .AND.OSF( NP_GROUP)!NEW PHE 8
          IF(I.EQ.9 )FILTER(I)=NP_SQGL()       .AND.OSF( NP_GROUP)!NEW PHE 9
          IF(I.EQ.10)FILTER(I)=NP_MSP()        .AND.OSF( NP_GROUP)!NEW PHE 10
          IF(I.EQ.11)FILTER(I)=NP_TAU()        .AND.OSF( NP_GROUP)!NEW PHE 11
          IF(I.EQ.12)FILTER(I)=NP_MUNU()       .AND.OSF( NP_GROUP)!NEW     12
          IF(I.EQ.13)FILTER(I)=NP_MET()        .AND.OSF( NP_GROUP)!NEW     13
          IF(I.EQ.14)FILTER(I)=NP_WRIGHT()     .AND.OSF( NP_GROUP)!NEW     14
          IF(I.EQ.15)FILTER(I)=TOP_EE()        .AND.OSF(TOP_GROUP)!TOP     15
          IF(I.EQ.16)FILTER(I)=TOP_EJET()      .AND.OSF(TOP_GROUP)!top     16
          IF(I.EQ.17)FILTER(I)=TOP_EMU()       .AND.OSF(TOP_GROUP)!TOP     17
          IF(I.EQ.18)FILTER(I)=TOP_JETS()      .AND.OSF(TOP_GROUP)!TOP     18
          IF(I.EQ.19)FILTER(I)=TOP_MUJET()     .AND.OSF(TOP_GROUP)!TOP     19
          IF(I.EQ.20)FILTER(I)=TOP_MUMU()      .AND.OSF(TOP_GROUP)!TOP     20
          IF(I.EQ.21)FILTER(I)=TOP_TOP_EE()    .AND.OSF(TOP_GROUP)!TOP     21
          IF(I.EQ.22)FILTER(I)=TOP_TOP_EJET()  .AND.OSF(TOP_GROUP)!top     22
          IF(I.EQ.23)FILTER(I)=TOP_TOP_EMU()   .AND.OSF(TOP_GROUP)!TOP     23
          IF(I.EQ.24)FILTER(I)=TOP_TOP_JETS()  .AND.OSF(TOP_GROUP)!TOP     24
          IF(I.EQ.25)FILTER(I)=TOP_TOP_MUJET() .AND.OSF(TOP_GROUP)!TOP     25
          IF(I.EQ.26)FILTER(I)=TOP_TOP_MUMU()  .AND.OSF(TOP_GROUP)!TOP     26
          IF(I.EQ.27)FILTER(I)=FAKE_E_CANDIDATE().AND.OSF(TOP_GROUP)!TOP   27
          IF(I.EQ.28)FILTER(I)=ELF_W()          .AND.OSF( WZ_GROUP)!WZ-TOP 28
          IF(I.EQ.29)FILTER(I)=ELF_Z()          .AND.OSF( WZ_GROUP)!WZ-TOP 29
          IF(I.EQ.30)FILTER(I)=MU1_WZT()        .AND.OSF( WZ_GROUP)!WZ-TOP 30
          IF(I.EQ.31)FILTER(I)=MU2_WZT()        .AND.OSF( WZ_GROUP)!WZ-TOP 31
          IF(I.EQ.32)FILTER(I)=WZ_ELF_MU()      .AND.OSF( WZ_GROUP)!WZ     32
          IF(I.EQ.33)FILTER(I)=ELF_MED()        .AND.OSF( WZ_GROUP)!WZ     33
          IF(I.EQ.34)FILTER(I)=QCD_STA()        .AND.OSF(QCD_GROUP)!QCD    34
          IF(I.EQ.35)FILTER(I)=QCD_GAP()        .AND.OSF(QCD_GROUP)!QCD    35
          IF(I.EQ.36)FILTER(I)=QCD_JJJ()        .AND.OSF(QCD_GROUP)!QCD	   36
          IF(I.EQ.37)FILTER(I)=QCD_QJT()        .AND.OSF(QCD_GROUP)!QC     37
          IF(I.EQ.38)FILTER(I)=QCD_NTR()        .AND.OSF(QCD_GROUP)!QCD    38
          IF(I.EQ.39)FILTER(I)=TOP_BKG()	.AND.OSF(TOP_GROUP)!TOP	   39
          IF(I.EQ.40)FILTER(I)=NP_CLEAN_TAU()   .AND.OSF( NP_GROUP)
	  IF(I.EQ.41)FILTER(I)=NP_SCALAR_TIGHT().AND.OSF( NP_GROUP)
	  IF(I.EQ.42)FILTER(I)=NP_SQGL_TIGHT()	.AND.OSF( NP_GROUP)!NP HAD 40-42
	  IF(I.EQ.43)FILTER(I)=NP_LQ_2EM_TIGHT().AND.OSF( NP_GROUP)
	  IF(I.EQ.44)FILTER(I)=LQNUE()          .AND.OSF( NP_GROUP)
	  IF(I.EQ.45)FILTER(I)=LQNN()           .AND.OSF( NP_GROUP)
	  IF(I.EQ.46)FILTER(I)=NP_LSS_TIGHT()   .AND.OSF( NP_GROUP)
	  IF(I.EQ.47)FILTER(I)=NP_WRIGHT_TIGHT().AND.OSF( NP_GROUP)
	  IF(I.EQ.48)FILTER(I)=NP_MULQ_TIGHT()  .AND.OSF( NP_GROUP)!NPLEP 43-48
          IF(I.EQ.49)FILTER(I)=ELF()		.AND.OSF(ALL_GROUP)!ELF	   49
          IF(I.EQ.50)FILTER(I)=MINBIAS()	.AND.OSF(ALL_GROUP)!MINBIAS50
          IF(I.EQ.51)FILTER(I)=MU1_FILT()	.AND.OSF( WZ_GROUP)!MU1	   51
          IF(I.EQ.52)FILTER(I)=PI0_FILTER()     .AND.OSF( WZ_GROUP)!PI0 WZ 52
          IF(I.EQ.53)FILTER(I)=TWOJET_FILTER()  .AND.OSF(  B_GROUP)!B      53
          IF(I.EQ.54)FILTER(I)=THREEJET_FILTER().AND.OSF(  B_GROUP)!B      54
          IF(I.EQ.55)FILTER(I)=EMAG_JETS()      .AND.OSF(QCD_GROUP)!QCD    55
          IF(I.EQ.56)FILTER(I)=NP_MULQ()        .AND.OSF( NP_GROUP)!NEW    56
          IF(I.EQ.57)FILTER(I)=TAU_FILTER()     .AND.OSF( NP_GROUP)!NEW    57
          IF(I.EQ.58)FILTER(I)=TOP_SGL()        .AND.OSF(TOP_GROUP)!TOP    58          58
          IF(I.EQ.59)FILTER(I)=TOP_TOP_SGL()    .AND.OSF(TOP_GROUP)!TOP    59
          IF(I.EQ.60)FILTER(I)=NEW_GAM_JET()    .AND.OSF( NP_GROUP)!NP B'  60
          IF(I.EQ.61)FILTER(I)=VERTEX_FILTER()  .AND.OSF(ALL_GROUP)!       61
          IF(I.EQ.62)FILTER(I)=MUJ_B()          .AND.OSF(  B_GROUP)!       62
          IF(I.EQ.63)FILTER(I)=.FALSE.                  !SPARE          63
          IF(I.EQ.64)FILTER(I)=.TRUE.                   !Pass all       64
C- Check that the CAPH stack pointer is 0
C
          CALL CHECK_CAPH_STACK(IER)
          IF ( IER .NE. 0 ) THEN
            WRITE (EMSG,1000) IER,FILTER_NAMES(I)
 1000       FORMAT('Stack pointer = ',I4,' after call to filter '
     &        , A20)
            CALL ERRMSG('MULTI_FILTER','MULTI_FILTER',EMSG,'F')
          ENDIF
          TIME=SECNDS(TIME)
          TIME_SUM(I)=TIME_SUM(I)+TIME
          TIME_SQUARED_SUM(I)=TIME_SQUARED_SUM(I)+TIME**2
        ELSE
          FILTER(I)=.false.
        ENDIF
      ENDDO
C
C Time stats for all filters
C
      TIME_EVENT=SECNDS(TIME_EVENT)
      TIME_SUM_EVENT=TIME_SUM_EVENT+TIME_EVENT
      TIME_SQUARED_SUM_EVENT=TIME_SQUARED_SUM_EVENT+TIME_EVENT**2

C IF THIS IS THE FIRST TIME THROUGH, ZERO OUT TIMES
      IF(FIRST)THEN
        FIRST=.FALSE.
        DO I=1,MAX_FILTER
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
      FILTER_BITS=0
      D0DAD_FILTER_BITS(1)=0
      D0DAD_FILTER_BITS(2)=0
      DO I=1,NSTREAMS
        KEEP_STREAM(I)=.FALSE.
        DO J=1,MAX_FILTER
          IF(I.EQ.1)THEN
C       Fill the filter bit word
            IF(FILTER(J))THEN
              IBIT=J
              IF(J.LE.32)THEN
                CALL SBIT1(D0DAD_FILTER_BITS(1),IBIT)
              ELSEIF(J.LE.64)THEN
                CALL SBIT1(D0DAD_FILTER_BITS(2),IBIT-32)
              ENDIF
            ENDIF
            DO K=1,MAX_FILTER
              IF(FILTER(J).AND.FILTER(K))
     &             CORRELATION(J,K)=CORRELATION(J,K)+1
            ENDDO
          ENDIF
C       KEEP THE STREAM IF ANY OF ITS FILTERS ARE TRUE
          KEEP_STREAM(I)=KEEP_STREAM(I).OR.(STREAM(J,I).AND.FILTER(J))
        ENDDO

C KEEP THIS EVENT OF THIS STREAM
        IF(KEEP_STREAM(I))THEN
          NUM_STREAM(I)=NUM_STREAM(I)+1
          CALL EVSET_STREAM(IND_STREAM(I))
        ENDIF
      ENDDO
C D0DAD LIST FOR input STREAM
            IF(D0DAD_FOR_INPUT)THEN
              CALL EVTIN_ZBPOS(ZBREC,ZBBYT)
              CALL D0DAD_LIST(RUNNO(),EVONUM(),
     &        ZBREC,ZBBYT,
     &        'D0DAD_INPUT',
     &        D0DAD_FILTER_BITS)
            ENDIF
C     Put the filter bit word into the header
      IQ(LHEAD+31)=D0DAD_FILTER_BITS(1)
      IQ(LHEAD+32)=D0DAD_FILTER_BITS(2)
      RETURN
C----------------------------------------------------------------------
C Initialization
C----------------------------------------------------------------------
      ENTRY MULTI_FILTER_BOJ
      MULTI_FILTER_BOJ=.TRUE.
      LUN = SSUNIT()
C----------------------------------------------------------------------
C Get the RCP parameters to control this routine
C----------------------------------------------------------------------
      CHOTINFO       =.FALSE.
      VERIFY         =.TRUE.
      PACKAGE_OFF    =.FALSE.
      PRE_FIX_FLAG   =.TRUE.
      LISTS_ON       =.TRUE.
      CRASH_ON_ZERO_EVENTS=.FALSE.
      CALL INRCP('MULTI_FILTER_RCP',IER)       ! read in RCP file
      IF(IER.NE.0) THEN
        WRITE(6,52)
   52   FORMAT(' MULTI_FILTER package, MULTI_FILTER_BOJ'/
     &      ' MULTI_FILTER_RCP file not found')
        STOP
      ELSE
        CALL EZPICK('MULTI_FILTER_RCP')
        CALL EZGET('PACKAGE_OFF',PACKAGE_OFF,IER)
        CALL EZGET('PRE_FIX_FLAG'   ,PRE_FIX_FLAG   ,IER)
        CALL EZGET('VERIFY'   ,VERIFY   ,IER)
        CALL EZGET('LISTS_ON'   ,LISTS_ON   ,IER)
        CALL EZGET('CRASH_ON_ZERO_EVENTS'   ,
     &              CRASH_ON_ZERO_EVENTS   ,IER)
        CALL EZGET('CHOTINFO'   ,CHOTINFO   ,IER)
        CALL EZ_GET_CHARS('DISABLED_FILTERS',NDISABLED,
     &                       DISABLED_FILTERS,IER)
        CALL EZ_GET_CHARS('D0DAD_STREAMS',ND0DAD_STREAMS,
     &                     D0DAD_STREAMS,IER)
        CALL EZ_GET_CHARS('STREAM_NAMES',NSTREAMS,STREAM_NAMES
     &                              ,IER)
        DO NSTREAM=1,NSTREAMS
          CALL EZ_GET_CHARS(STREAM_NAMES(NSTREAM),NFILTERS(NSTREAM),
     &                              STREAM_DEFINITION(1,NSTREAM),IER)
        ENDDO
        CALL EZ_GET_CHARS('GROUP_NAMES',NGROUPS,GROUP_NAMES
     &                              ,IER)
        DO NGROUP=1,NGROUPS
          CALL EZ_GET_CHARS(GROUP_NAMES(NGROUP),NONFILT,
     &                              ASTS(1,NGROUP),IER)
C         UPPER CASE all these strings so there is NO confusion when comparing
          DO I=1,10
                  CALL UPCASE(ASTS(I,NGROUP),ASTS(I,NGROUP))
          ENDDO
        ENDDO

        CALL EZRSET
      ENDIF
C----------------------------------------------------------------------
C Package control
C----------------------------------------------------------------------
      IF (PACKAGE_OFF)THEN
        WRITE(6,*)'MULTI_FILTER package has been turned off'
        RETURN
      ELSE
	WRITE(6,*)' Online streams allowed'
        DO NGROUP=1,NGROUPS
          IF(GROUP_NAMES(NGROUP).EQ.'ALL_GROUP')ALL_GROUP=NGROUP
          IF(GROUP_NAMES(NGROUP).EQ.'B_GROUP'  )  B_GROUP=NGROUP
          IF(GROUP_NAMES(NGROUP).EQ.'NP_GROUP' ) NP_GROUP=NGROUP
          IF(GROUP_NAMES(NGROUP).EQ.'QCD_GROUP')QCD_GROUP=NGROUP
          IF(GROUP_NAMES(NGROUP).EQ.'TOP_GROUP')TOP_GROUP=NGROUP
          IF(GROUP_NAMES(NGROUP).EQ.'WZ_GROUP' ) WZ_GROUP=NGROUP
          WRITE(6,60)GROUP_NAMES(NGROUP),(ASTS(J,NGROUP),J=1,10)
          WRITE(LUN,60)GROUP_NAMES(NGROUP),(ASTS(J,NGROUP),J=1,10)
        ENDDO
   60   FORMAT(' Group: ',1A10,' Names:',(10A4))
        CALL EVTWOS_MULT(.TRUE.)
        ELAPSED_TIME=SECNDS(0.)
C----------------------------------------------------------------------
C DISABLE FILTERS AND WARN IF THEY ARE SELECTED FOR ANY STREAM
C----------------------------------------------------------------------
        DO J=1,MAX_FILTER
          FILTER_FLAG(J)=.TRUE.
          DO I=1,NDISABLED
            IF(DISABLED_FILTERS(I).EQ.FILTER_NAMES(J))THEN
              FILTER_FLAG(J)=.FALSE.
            ENDIF
          ENDDO
        ENDDO
        WRITE(6,54)(DISABLED_FILTERS(I),I=1,NDISABLED)
        WRITE(LUN,54)(DISABLED_FILTERS(I),I=1,NDISABLED)
   54   FORMAT(' Disabled filters...',/8(' ',4A20/))
C----------------------------------------------------------------------
C Enable D0DAD for input , if in D0DAD_STREAMS list
C----------------------------------------------------------------------
        D0DAD_FOR_INPUT=.FALSE.
        DO K=1,ND0DAD_STREAMS
          IF('INPUT'.EQ.D0DAD_STREAMS(K))THEN
            D0DAD_FOR_INPUT=.TRUE.
            CALL D0DAD_LIST_BOJ('D0DAD_INPUT')
            WRITE(6  ,*)'D0DAD selected for input: D0DAD_INPUT.'
            WRITE(LUN,*)'D0DAD selected for input: D0DAD_INPUT.'
          ENDIF
        ENDDO
C----------------------------------------------------------------------
C Selectively, enable streams
C Selectively, ENABLE  filters for each stream
C----------------------------------------------------------------------
        DO I=1,NSTREAMS
          CALL EVGET_STREAM(STREAM_NAMES(I),IND_STREAM(I))
          IF(LISTS_ON)CALL EVENT_LIST_BOJ(0,0,STREAM_NAMES(I))
C----------------------------------------------------------------------
C D0DAD lists
C----------------------------------------------------------------------
          D0DAD_FOR_OUTPUT(I)=.FALSE.
          DO K=1,ND0DAD_STREAMS
            IF(STREAM_NAMES(I).EQ.D0DAD_STREAMS(K))THEN
              D0DAD_FOR_OUTPUT(I)=.TRUE.
C-             list for STA
              D0DAD_NAMES(I,1)='D0DAD_'//D0DAD_STREAMS(K)(1:5)//'_STA'
              CALL D0DAD_LIST_BOJ(D0DAD_NAMES(I,1))
C-             list for DST
              D0DAD_NAMES(I,2)='D0DAD_'//D0DAD_STREAMS(K)(1:5)//'_DST'
              CALL D0DAD_LIST_BOJ(D0DAD_NAMES(I,2))
            ENDIF
          ENDDO
          DO K=1,MAX_FILTER
            STREAM(K,I)=.FALSE.
            DO J=1,NFILTERS(I)
              IF(STREAM_DEFINITION(J,I).EQ.FILTER_NAMES(K))THEN
                STREAM(K,I)=.TRUE.
                IF(.NOT.FILTER_FLAG(K))THEN
                  WRITE(6,53)STREAM_NAMES(I),FILTER_NAMES(K)
                  WRITE(LUN,53)STREAM_NAMES(I),FILTER_NAMES(K)
   53             FORMAT(' Warning:stream ',1A5,
     &                ' requested disabled filter ',1A20)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          WRITE(6,55)STREAM_NAMES(I),(STREAM_DEFINITION(L,I),L=1,
     &            NFILTERS(I))
          IF(D0DAD_FOR_OUTPUT(I))WRITE(6  ,58)D0DAD_NAMES(I,1),
     &                                        D0DAD_NAMES(I,2)
          WRITE(LUN,55)STREAM_NAMES(I),(STREAM_DEFINITION(L,I),L=1,
     &            NFILTERS(I))
          IF(D0DAD_FOR_OUTPUT(I))WRITE(LUN,58)D0DAD_NAMES(I,1),
     &                                        D0DAD_NAMES(I,2)
   55     FORMAT(' MULTI_FILTER stream: ',1A5,
     &           ' Definition... '/8(' ',4A20/))
   58     FORMAT('                    D0DAD names: ',2A20)
        ENDDO
      ENDIF
      RETURN
C----------------------------------------------------------------------
C End of job stuff
C----------------------------------------------------------------------
      ENTRY MULTI_FILTER_EOJ
      MULTI_FILTER_EOJ=.TRUE.
C----------------------------------------------------------------------
C Calorimeter Hot channel thingy
C----------------------------------------------------------------------
      IF (CHOTINFO)THEN
C       CALL CHOTINFO_EOP
      ENDIF
C----------------------------------------------------------------------
C Package control
C----------------------------------------------------------------------
      IF (PACKAGE_OFF)RETURN
C----------------------------------------------------------------------
C Completion, create summary file of average times per filter,
C filter correlations and do bookkeeping for PDB, etc.
C (For filter time stats, the first event is not included, thus
C use NUM_CALLS-1 .
C----------------------------------------------------------------------
      WRITE(LUN2,*)'TOTAL EVENTS: ',NINT(NUM_CALLS)
      IF(CRASH_ON_ZERO_EVENTS)THEN
C    ( DEAL WITH FILES WITH ONE OR 0 EVENTS)
      IF(NUM_CALLS.EQ.0)THEN
C CRASH IF THERE WERE NO EVENTS
      WRITE(LUN2,*)'Abort; No events on input'
      WRITE(MSG,*)'No events on input'
      CALL ERRMSG('ZERO EVENTS','MULTI_FILTER',MSG,'F')
      ENDIF
      ENDIF
      IF(NUM_CALLS.LT.2)THEN
        WRITE(LUN,*)'Number of events less than 2, stats unreliable'
        NUM_CALLS=2
      ENDIF
      WRITE(LUN,*)
      WRITE(LUN,*)
     &    '******* MULTI_FILTER statistics ************************'
      WRITE(LUN,*)
      ELAPSED_TIME=SECNDS(ELAPSED_TIME)
      WRITE(LUN,*)'Total number of events processed:',NUM_CALLS
      DO I=1,NSTREAMS
        WRITE(LUN,56)STREAM_NAMES(I),NUM_STREAM(I),NUM_STREAM(I)
     &      /NUM_CALLS*100
   56   FORMAT(' ',1A5,' stream events :',F8.0,F8.3,'% of total.')
      ENDDO
      WRITE(LUN2,57)NSTREAMS,
     & (STREAM_NAMES(I),NINT(NUM_STREAM(I)),I=1,NSTREAMS)
   57 FORMAT(' STREAMS:',I5/12(5('  ',1A15,I8)/))
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
      DO I=1,MAX_FILTER
        WRITE(LUN,101)FILTER_NAMES(I)(1:12),
     &                 (CORRELATION(I,I)/num_calls*100),
     &                 SQRT(CORRELATION(I,I))/num_calls*100

      ENDDO
  101 FORMAT(' ',A12,2F8.4)
      WRITE(LUN2,102)MAX_FILTER,(FILTER_NAMES(I),NINT(CORRELATION(I,I)),
     &      I=1,MAX_FILTER)
  102 FORMAT(' FILTERS:',I5/14(5('  ',1A15,I8)/))
      WRITE(LUN,100)
      IF(VERIFY)THEN
        WRITE(LUN,*)
c        WRITE(LUN,*)
c     &    'Filter correlations normalized to total events (%)'
c
c        LW=MAX_FILTER/2
c
c        WRITE(LUN,*)
c        WRITE(LUN,201)(FILTER_NAMES(I)(1:5),I=1,LW)
c  201   FORMAT(' FILTERS     ',<LW>(1X,A5))
c        WRITE(LUN,200)
  200   FORMAT(' ',122('-'))
c        DO I=1,MAX_FILTER
c          WRITE(LUN,202)FILTER_NAMES(I)(1:12),
c     &                 (CORRELATION(I,J)/num_calls*100.,J=1,LW)
c        ENDDO
c        WRITE(LUN,200)
c  202   FORMAT(' ',A12,<LW>F6.2)
c        WRITE(LUN,*)
c        WRITE(LUN,251)(FILTER_NAMES(I)(1:5),I=LW+1,MAX_FILTER)
c  251   FORMAT(' FILTERS     ',<MAX_FILTER-LW>(1X,A5))
c        WRITE(LUN,200)
c        DO I=1,MAX_FILTER
c          WRITE(LUN,252)FILTER_NAMES(I)(1:12),
c     &      (CORRELATION(I,J)/num_calls*100.,J=LW+1,MAX_FILTER)
c        ENDDO
c        WRITE(LUN,200)
c  252   FORMAT(' ',A12,<MAX_FILTER-LW>F6.2)
c        WRITE(LUN,*)
c        WRITE(LUN,*)'FILTER TIMES'
c        WRITE(LUN,*)
        WRITE(LUN,301)
  301   FORMAT(' FILTER    AVERAGE  STD Dev. % of total')
        WRITE(LUN,302)
  302   FORMAT(' --------------------------------------')
        DO I=1,MAX_FILTER
          AVERAGE_TIME=TIME_SUM(I)/NUM_CALLS
          STD_DEV_TIME=(TIME_SQUARED_SUM(I))/(NUM_CALLS-1)
     &        -AVERAGE_TIME**2
          IF(STD_DEV_TIME.GT.0)THEN
            STD_DEV_TIME=SQRT(STD_DEV_TIME)
          ELSE
            STD_DEV_TIME=0.
          ENDIF
          IF(AVERAGE_TIME_EVENT.EQ.0.0)AVERAGE_TIME_EVENT=1.0
          WRITE(LUN,300)FILTER_NAMES(I),AVERAGE_TIME,STD_DEV_TIME,
     &              AVERAGE_TIME/AVERAGE_TIME_EVENT*100.
        ENDDO
        WRITE(LUN,302)
  300   FORMAT(' ',A10,2F8.5,'  (',F6.3,'%)')
      ENDIF
  999 RETURN
C----------------------------------------------------------------------
C Write D0DAD info
C----------------------------------------------------------------------
      ENTRY MULTI_FILTER_POST_WRITE
      MULTI_FILTER_POST_WRITE=.TRUE.
C----------------------------------------------------------------------
C D0DAD info for previous event, After the event is written stuff
C Must be done at end of job to record the last event
C----------------------------------------------------------------------
C First, check to see that this is an event header, if not, do not put it
C into the D0DAD list.
C----------------------------------------------------------------------
      IOS=MOD(IQ(LHEAD+1),1000)
      IF(IOS.LE.4)THEN
         WRITE(MSG,*)'Current header type not event'
         CALL ERRMSG('Not event','MULTI_FILTER_POST_WRITE',MSG,'W')
	 RETURN
      ENDIF
C----------------------------------------------------------------------
C Loop over all streams and write to the D0DAD file
C----------------------------------------------------------------------
      DO I=1,NSTREAMS
C KEEP THIS EVENT OF THIS STREAM
        IF(KEEP_STREAM(I))THEN
          IF(LISTS_ON)CALL EVENT_LIST(RUNNO(),EVONUM(),
     &        STREAM_NAMES(I),
     &        FILTER_BITS)
C D0DAD LIST FOR THIS STREAM
          DO K=1,2
            IF(D0DAD_FOR_OUTPUT(I))THEN
              CALL EVWR_MULT_ZBPOS(I,K,ZBREC,ZBBYT)
              CALL D0DAD_LIST(RUNNO(),EVONUM(),
     &        ZBREC,ZBBYT,
     &        D0DAD_NAMES(I,K),
     &        D0DAD_FILTER_BITS)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      END
