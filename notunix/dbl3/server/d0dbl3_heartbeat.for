      FUNCTION D0DBL3_HEARTBEAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sends Alarm Server heartbeats informing it that 
C-   this DBl3 server is running. This routine functions only on VAXVMS 
C-   platforms (forced by machine block). The HEARTBEAT name is 
C-   XXX_DBL3_YYY where XXX is the DETECTOR name (CAL,VTX,FDC,CDC,TRD,MUO...).
C-   and the YYY is (part of) the CLUSTER name (D0 for online, D0F for 
C-   offline) where the server is running. There is some recovery logic
C-   internal to this routine which is not guaranteed to function 
C-   in the event of a ALARM or HEARTBEAT failure. The SERVER must have 
C-   the ALARM_SERVER_NODE logical (typicaly D0HSA) defined to be able to
C-   connect to the ALARM_SERVER. If this logical is not set to a string
C-   begining with 'D0' the heartbeats will be turned off. 
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-SEP-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D0DBL3_HEARTBEAT
      LOGICAL D0DBL3_HEARTBEAT_INIT,D0DBL3_HEARTBEAT_END
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER   CHANNEL
      INTEGER   PERIOD
      INTEGER   PRIORITY
      INTEGER   SUBSYSTEM
      INTEGER   PATH
      INTEGER   MAX_TRIES,TRIES
C
      REAL    DELAY,THIS_TIME,LAST_TIME,DELTA_TIME,VALUE
      REAL    TIME0,TIME1,TIME2
      CHARACTER  ATTRIBUTE*4,MSG*80,NAME*12,ALARM_NODE*20,CLUSTER*4
      CHARACTER*28 TEXT, STRING
C
      PARAMETER( PRIORITY =  0 )
      PARAMETER( SUBSYSTEM=  0 )
      PARAMETER( PATH     =  0 )
      PARAMETER( ATTRIBUTE= 'HRBT' )
      PARAMETER( TEXT     = 'D0DBL3_SERVER HEARTBEAT' )
      INTEGER STATUS, I, J, K, L
      INTEGER START_HEARTBEAT,STOP_HEARTBEAT,SEND_HEARTBEAT
      INTEGER CONNECT_ALARM, DISCONNECT_ALARM
      LOGICAL THUMPING,STOPPED,HEARTBEAT
      DATA STOPPED/.TRUE./
C----------------------------------------------------------------------
      EXTERNAL  START_HEARTBEAT,STOP_HEARTBEAT,SEND_HEARTBEAT
C----------------------------------------------------------------------
C&IF VAXVMS
      IF(.NOT.HEARTBEAT) GOTO 999
      THIS_TIME = SECNDS(0.)
      DELTA_TIME = THIS_TIME - LAST_TIME
      D0DBL3_HEARTBEAT = .TRUE.
      IF(DELTA_TIME.LT.0) LAST_TIME = THIS_TIME  ! SECNDS RESET AT MIDNIGHT
      IF(DELTA_TIME.GT.DELAY) THEN
        IF(.NOT.THUMPING) GOTO 1
        STATUS = SEND_HEARTBEAT ( NAME, ATTRIBUTE, CHANNEL )
        IF (.NOT. STATUS) CALL ERRMSG 
     &    ('SEND_HEARTBEAT','D0DBL3_HEARTBEAT','NO SEND','W')
        THUMPING = STATUS
        LAST_TIME = THIS_TIME
      END IF
      D0DBL3_HEARTBEAT = STATUS
C&ELSE
C&
C&ENDIF
  999 CONTINUE
      RETURN
C----------------------------------------------------------------------
      ENTRY D0DBL3_HEARTBEAT_INIT
C&IF VAXVMS
C
      MAX_TRIES =5       ! TO CONNECT TO 
C
C ********************************************
C ****  Setup waits etc.
C ********************************************
C
      CALL TRNLNM('D0DBL3$HEARTBEAT_PERIOD',STRING,L)
      DELAY = VALUE(STRING(1:L),I,J,K)
      IF ( DELAY .LE. 0.0 ) THEN
        DELAY = 300.       ! 5 MINUTES OR SO = 300 SECONDS 
      ENDIF
      CALL TRNLNM('D0DBL3$HEARTBEAT_ALARM',STRING,L)
      PERIOD = VALUE(STRING(1:L),I,J,K)
      IF ( PERIOD .LE. DELAY ) THEN
        PERIOD = DELAY*1.5 ! DEMAND HEARTBEAT WITHIN 1.5 BEAT PERIODS
      ENDIF
      WRITE (MSG,22) DELAY,PERIOD
   22 FORMAT(' HEARTBEAT PERIOD ',F9.1,' ALARM WAIT ',I9,' SECONDS')
      CALL INTMSG(MSG)
C
C ********************************************
C ****  FIND ALARM_SERVER_NODE 
C ********************************************
C
      HEARTBEAT = .TRUE.
      CALL TRNLNM('ALARM_SERVER_NODE',ALARM_NODE,L)
      K = INDEX(ALARM_NODE,'ALARM_SERVER')
      IF (K.GT.0) THEN
        ALARM_NODE = 'NONE' ! DEFAULT to D0HSA
        HEARTBEAT = .FALSE.
      ELSE IF (ALARM_NODE(1:2).NE.'D0') THEN
        HEARTBEAT = .FALSE.
      END IF
      CALL INTMSG(' ALARM_SERVER_NODE = '//ALARM_NODE)
      IF(.NOT.HEARTBEAT) THEN
        CALL ERRMSG(' ALARM_SERVER_NODE NOT D0XXX','D0DBL3_HEARBEAT',
     &    ' SERVER WILL NOT send HEARTBEATS ','W')
        GOTO 1999
      END IF
      ALARM_NODE = 'ALARM_SERVER_NODE'
C
C ********************************************
C ****  Connect to ALARM SERVER
C ********************************************
C
    1 TRIES = 0
      IF (.NOT.STOPPED) GOTO 2
  100 CONTINUE
C
      TIME0 = SECNDS(0.)
      TIME2 = TIME0 - TIME1
      IF(TIME2.LT.0) TIME1 = TIME0   ! SECNDS RESET AT MIDNIGHT
      IF(TIME2.LT.DELAY) GOTO 1999 
      TIME1 = TIME0
C
      TRIES = TRIES + 1
      IF ( TRIES .GT. MAX_TRIES ) THEN
        CALL ERRMSG('HEARTBEAT_FAILED','D0DBL3_HEARBEAT',
     & 'MAXIMUM NUMBER TRIES to ALARM_CONNECT REACHED','W')
        D0DBL3_HEARTBEAT_INIT = .FALSE.
        THUMPING = .FALSE.
        GOTO 1999
      ENDIF
C
      STATUS = CONNECT_ALARM(ALARM_NODE,CHANNEL)
      IF (.NOT. STATUS) GOTO 100
C
C ********************************************
C ****  FIND NAME OF THIS SERVER (DET_DBL3SRVR - DET D0DBL3_SERVR.INC)
C ********************************************
C
      CALL TRNLNM('SYS$CLUSTER_NODE',CLUSTER,L)
      CALL WORD(CLUSTER,I,J,K)
      IF(K.EQ.0) THEN
        CLUSTER = 'OFFL'
        IF (IONLINE.EQ.1) CLUSTER = 'ONL'
      END IF
      I = INDEX(CLUSTER,':')
      IF(I.GT.0)CLUSTER = CLUSTER(1:I-1)
      NAME = 'DBL3_'//SRVRNM(1:3)//'_'//CLUSTER
      CALL INTMSG(' HEARTBEAT NAME = '//NAME)
C
C ********************************************
C ****  Start HEARTBEAT
C ********************************************
C
      STATUS = START_HEARTBEAT ( NAME,
     &                           ATTRIBUTE,
     &                           TEXT,
     &                           PRIORITY,
     &                           SUBSYSTEM,
     &                           PATH,
     &                           PERIOD,
     &                           CHANNEL )
      IF ( .NOT. STATUS ) CALL ERRMSG ('START_HEARTBEAT'//NAME,
     &  'D0DBL3_HEARTBEAT','STATUS=FALSE','W')
      THUMPING = STATUS
      STOPPED = .NOT.STATUS
      LAST_TIME = SECNDS(0.)
      D0DBL3_HEARTBEAT_INIT = STATUS
C&ELSE
C&
C&ENDIF
C
 1999 RETURN
C----------------------------------------------------------------------
      ENTRY D0DBL3_HEARTBEAT_END
      IF(.NOT.HEARTBEAT) GOTO 2999
C
C ****  STOP HEARTBEAT
C
C&IF VAXVMS
    2 CONTINUE
      STATUS = STOP_HEARTBEAT (  NAME,
     &                           ATTRIBUTE,
     &                           CHANNEL )
      IF ( .NOT. STATUS ) THEN
        CALL ERRMSG('STOP_HEARTBEAT','D0DBL3_HEARTBEAT',
     &    'NO CONNECT','W')
C
C ****  Disconnect from Alarm Server (STATUS?)
C
      ELSE
        STATUS = DISCONNECT_ALARM()
        IF ( .NOT. STATUS ) CALL ERRMSG
     &    ('DISCONNECT_ALARM','D0DBL3_HEARTBEAT','NO CONNECT','W')
      END IF
      D0DBL3_HEARTBEAT_END = STATUS
      STOPPED = STATUS
      THUMPING = .FALSE.
C&ELSE
C&
C&ENDIF
 2999 RETURN
      END

