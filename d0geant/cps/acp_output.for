      PROGRAM ACP_OUTPUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : OUTPUT process for SG farm processing. Works
C-                         in conjunction with processes doing ZEBRA 
C-                         I/O via ACP_FZIO.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-AUG-1990   Michael Diesburg
C-   Modified 16-JUL-1992   Kirill Denisenko Now uses RBIO for output
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ACP_USER.INC'
      INCLUDE 'D0$INC:ACPOUT.INC'
      INCLUDE 'D0$PARAMS:ACPDEF.PARAMS'
      INTEGER NTYPE,NBYTS,FLAG(512),PNOUT,MSGSET
      INTEGER PN,MESSAGE,MBYT,NSTAT
      INTEGER DATAREADY
      INTEGER NEVENT(4),NREAL_EVENT
      CHARACTER*4 ODEV
      CHARACTER*1 NULL
      INTEGER FDSCRP,BLOCKS
      LOGICAL TAPE
      DATA NEVENT / 4*0 /
      DATA MBYT / 2048 /
      DATA NREAL_EVENT / 999999 /
C     Variables necessary for RBIO
      CHARACTER*160 NAME
      CHARACTER*80 TAPEFILE,FILE1,MSG
      CHARACTER*10 MAXEV
      CHARACTER*2 FSEQ
      CHARACTER*6 VSN
      INTEGER OUTDEV / 20 /
      INTEGER ACPACT,RBSTATUS,IFSEQ
      INTEGER RBEVENT,FMAXEVENT,TLEN,FLEN

C----------------------------------------------------------------------

      CALL ACP_INIT
      CALL ACP_DECLARE_BLOCK(OUTBUF,OUTBYT,OUTBLOCK)
      DATAREADY = ACP_SET_OF_MESSAGE_TYPES(1,DATA_READY)
      CALL ACP_SYNC(ACP$ALL_PROCESSES,STARTUP)

      RBEVENT = 0                          ! Event number in a file sequence
      BLOCKS  = 0                          ! Blocks in file
      IFSEQ   = 0                          ! File sequence number
      NULL = CHAR(0)
      TAPE = .FALSE.
      CALL GETENV('CPS_OUTPUT',ODEV)
      IF(ODEV.EQ.'tape')  TAPE = .TRUE.
      CALL GETENV('FMAXEVENT',MAXEV)
      READ(MAXEV,'(I4)') FMAXEVENT 
      CALL GETENV('CPS_TAPEFILE',TAPEFILE)
      TLEN = INDEX(TAPEFILE,' ') - 1
      IF (TLEN .LE. 0) TLEN = LEN(TAPEFILE)
      FLEN = TLEN + 11
C
C--- Mount output tape
C
      CALL ACP_MOUNT(OUTDEV,'WRITE',TAPE,VSN,NSTAT)
      IF (NSTAT .EQ. -1) THEN
        CALL ACP_STOP_JOB
      ELSE
        CALL FTYPE('[ACP_OUTPUT] MOUNTED OUTPUT '//VSN,NSTAT,0)
      ENDIF


C
C--- Loop over events


10    CONTINUE
 
      CALL ACP_DEQUEUE_PROCESS(PNOUT,OUTPUTQ)
      IF(PNOUT.EQ.ACP$END_OF_QUEUE)  GO TO 500
      CALL ACP_TRANSMIT_MESSAGE(PNOUT,QACKNOWLEDGE,NEVENT,4)

C
C--- If it is time to close the old dataset and open a new one -
C
      IF(RBEVENT.GE.FMAXEVENT) THEN
        CALL ACP_CLOSE(OUTDEV,TAPE,NSTAT)
        IF(NSTAT.NE.0) THEN
          CALL FTYPE('[ACP_OUTPUT] CLOSE FAILED:',NSTAT,1)
          CALL ACP_STOP_JOB
        ELSE
          CALL RCPTYPE(VSN,FILE1(1:FLEN),IFSEQ,RBEVENT,BLOCKS)
        ENDIF
        RBEVENT = 0
        BLOCKS  = 0
      ENDIF
C
C--- Open output file if necessary
C
      IF(RBEVENT.LE.0)  THEN
        IFSEQ = IFSEQ + 1
        WRITE(FSEQ,'(I2.2)') IFSEQ
        FILE1 = TAPEFILE(1:TLEN)//'_'//FSEQ//'.X_RAW01'
        CALL ACP_OPEN(OUTDEV,'WRITE',FILE1(1:FLEN),IFSEQ,TAPE,NSTAT)
        IF (NSTAT .EQ. -1) THEN
          CALL ACP_STOP_JOB
        ELSE
          MSG = '[ACP_OUTPUT] OPENED OUTPUT '//FILE1(1:FLEN)
          CALL FTYPE(MSG,IFSEQ,0)
        ENDIF
      ENDIF


20    CONTINUE
      CALL ACP_TRANSMIT_MESSAGE(PNOUT,SEND_DATA,FLAG,4)
      CALL ACP_RECEIVE_MESSAGE(DATAREADY,PN,MESSAGE,FLAG,MBYT)
      NTYPE = FLAG(1)
      CALL ACP_SIGHOLD
      CALL RBWRITE(OUTDEV,OUTBUF,OUTBYT,ACPACT,RBSTATUS)
      CALL ACP_SIGRELEASE
      IF(RBSTATUS.EQ.0) THEN
        IF(ACPACT.NE.OUTBYT) THEN
          CALL FTYPE('[ACP_OUTPUT] NSTAT != OUTBYT:',NSTAT,0)
        ENDIF
      ELSE
        CALL CPERROR('ACP_OUTPUT')
      ENDIF

      NEVENT(3) = NEVENT(3) + RBSTATUS
      NEVENT(2) = NEVENT(2) + 1
      BLOCKS = BLOCKS + 1
      IF(NTYPE.LE.1)  GO TO 20
      NEVENT(1) = NEVENT(1) + 1
      RBEVENT = RBEVENT + 1

      CALL ACP_UPDATE_USER_STATUS(4,NEVENT)
      IF(NEVENT(1)/10*10.EQ.NEVENT(1)) THEN
        CALL FTYPE('[ACP_OUTPUT] WRITING EVENT : ',NEVENT(1),1)
      ENDIF
      GO TO 10

500   CONTINUE
      CALL ACP_CLOSE(OUTDEV,TAPE,NSTAT)
      IF(NSTAT.NE.0) THEN
        CALL FTYPE('[ACP_OUTPUT] CLOSE FAILED:',NSTAT,1)
        CALL ACP_STOP_JOB
      ELSE
        CALL RCPTYPE(VSN,FILE1(1:FLEN),IFSEQ,RBEVENT,BLOCKS)
      ENDIF
      CALL ACP_SUCCESS(VSN,NEVENT(1),NEVENT(2))
      CALL ACP_DISMOUNT(OUTDEV,TAPE,NSTAT)
      CALL FTYPE('[ACP_OUTPUT] EVENTS WRITTEN : ',NEVENT(1),1)
      CALL ACP_SYNC(ACP$ALL_PROCESSES,RUNDOWN)
      CALL ACP_STOP_JOB

      END
