      PROGRAM ACP_INPUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INPUT process for farm processing. Works
C-                         in conjunction with processes doing ZEBRA 
C-                         I/O via ACP_FZIO.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-AUG-1990   Michael Diesburg
C-   Modified 29-OCT-1992   Kirill Denisenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER *12 EVN
      INTEGER ILUN,NBLKS,NBYTS,FLAG(512)
      INTEGER PNIN,PN,NB,NREC,NSTAT,MBYT,NRTYP,NLTYP
      INTEGER FDSCRP,ECOUNT,BLOCK_READ
      INTEGER INDEV, INDISK, ACPACT, RBSTATUS
      INTEGER SENDDATA,MESSAGE,ALL_PROCESSES
      INCLUDE 'D0$INC:ACP_USER.INC'
      INCLUDE 'D0$INC:ACPINP.INC'
      INCLUDE 'D0$PARAMS:ACPDEF.PARAMS'
      INTEGER NEVENT(4)
      INTEGER SIGUSR1,ll
      DATA SIGUSR1 / 30 /
      DATA NEVENT / 4*0 /
      DATA ILUN,NREC / 31, 0 /
      DATA MBYT /2048/
      DATA INDEV/30/, INDISK/40/
C----------------------------------------------------------------------

      CALL ACP_INIT
      CALL ACP_UPDATE_USER_STATUS(4,NEVENT)
      CALL ACP_SYNC(ACP$ALL_PROCESSES,STARTUP)
      ALL_PROCESSES = ACP_SET_OF_PROCESSES(2,2,3)
      SENDDATA = ACP_SET_OF_MESSAGE_TYPES(1,SEND_DATA)

      CALL GETENV('EVENT_COUNT',EVN)
      READ(UNIT=EVN,FMT=*,END=100) ECOUNT
100   IF(ECOUNT.LE.0)  ECOUNT = 999999
      CALL FTYPE('[ACP_INPUT] EVENTS TO BE READ : ',ECOUNT,0)

      CALL ACP_OPEN(INDEV,'READ','INFILE',0,.FALSE.,NSTAT)
      IF (NSTAT .EQ. -1) THEN
        CALL FTYPE('[ACP INPUT] FAILED TO OPEN INPUT',0,0)
        CALL ACP_STOP_JOB
      ENDIF

10    NREC = NREC + 1
      CALL ACP_SIGHOLD
      CALL RBREAD(INDEV,INPBUF,INPBYT,ACPACT,RBSTATUS,*400,*500)
400   CALL ACP_SIGRELEASE
      IF(RBSTATUS.NE.0) THEN
        IF(ACPACT.NE.INPBYT) THEN
          CALL FTYPE('[ACP_INPUT]: NSTAT != INPBYT:',NSTAT,INPBYT)
        ENDIF
      ENDIF
      NBLKS = INPBUF(8)                 ! No. of additional blocks
      NBYTS = INPBUF(5)*4               ! No. bytes this record
      NLTYP = INPBUF(10)		! Logical record type
      NRTYP = INPBUF(22)		! NRUN (=0 for EOR)  
      INPBUF(6) = 0			! Prevent event count checking
      IF(NLTYP.EQ.1.AND.NRTYP.LE.0)  GO TO 500

      CALL ACP_DEQUEUE_PROCESS(PNIN,INPUTQ)
      CALL ACP_TRANSMIT_MESSAGE(PNIN,QACKNOWLEDGE,PN,4)

      CALL ACP_RECEIVE_MESSAGE(SENDDATA,PN,MESSAGE,FLAG,MBYT)
      CALL ACP_SEND(PNIN,INPBUF,NBYTS,INPBLOCK,0)
      FLAG(1) = 0
      FLAG(2) = NBYTS/4
      CALL ACP_TRANSMIT_MESSAGE(PNIN,DATA_READY,FLAG,8)

      IF(NBLKS.GT.0)  THEN              ! Don't lose control if more
        DO 25 NB = 1,NBLKS              ! to come.
          NREC = NREC + 1
          CALL ACP_SIGHOLD
          CALL RBREAD(INDEV,INPBUF,INPBYT,ACPACT,RBSTATUS,*401,*500)
401       CALL ACP_SIGRELEASE
          IF(RBSTATUS.NE.0) THEN
            IF(ACPACT.NE.INPBYT) THEN
              CALL FTYPE('[ACP_INPUT] NSTAT != INPBYT:',NSTAT,1)
            ENDIF
          ENDIF
          CALL ACP_RECEIVE_MESSAGE(SENDDATA,PN,MESSAGE,FLAG,MBYT)
          CALL ACP_SEND(PNIN,INPBUF,INPBYT,INPBLOCK,0)
          FLAG(1) = 0
          FLAG(2) = INPWDS
          CALL ACP_TRANSMIT_MESSAGE(PNIN,DATA_READY,FLAG,8)
25      CONTINUE
      ENDIF

      NEVENT(1) = NEVENT(1) + 1
      CALL ACP_UPDATE_USER_STATUS(4,NEVENT)
      IF(NEVENT(1).LT.ECOUNT)  GO TO 10

500   CONTINUE
      CALL ACP_SIGRELEASE
      CALL FTYPE(' [ACP_INPUT] INPUT EVENTS READ : ',NEVENT(1),0)
      CALL ACP_CLOSE(INDEV,.FALSE.,NSTAT)
      CALL ACP_WAIT_QUEUE(INPUTQ,ACP$FULL)
      CALL ACP_TRANSMIT_MESSAGE(ALL_PROCESSES,END_OF_INPUT,PN,4)
      CALL ACP_WAIT_QUEUE(DONEQ,ACP$FULL)
      CALL ACP_QUEUE_PROCESS(ACP$END_OF_QUEUE,OUTPUTQ)
      CALL ACP_SYNC(ACP$ALL_PROCESSES,RUNDOWN)
      CALL ACP_STOP_PROCESS

      END
