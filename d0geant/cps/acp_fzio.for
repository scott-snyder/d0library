      SUBROUTINE ACP_FZIO(ZBUF,IOFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ZEBRA USERIO routine. Specified to ZEBRA
C-                         with CALL FZHOOK(LUN,ACP_FZIO,DUMMY). Must
C-                         be called AFTER call to FZFILE(LUN,0,'C').
C-                         FZFILE mode must be channel I/O ('C' option).
C-                         This operates only in Exchange mode and
C-                         basically replaces calls to XINBF and XOUTBF
C-                         to do physical I/O.
C-
C-   Inputs  : IOFLAG    = I/O Direction
C-                       = 1 => Output Request (called from FZOUT)
C-                       = 0 => Input  Request (called from FZIN)
C-             ZBUF      = Data to be sent for IOFLAG = 1
C-             IQUEST(3) = Record type
C-                         For IOFLAG = 0 :
C-                           = 0 => Get next continuation record
C-                           = 1 => Get start of new logical record
C-                         For IOFLAG = 1 :
C-                           = 0 => Continuation block
C-                           = 1 => First block of new logical record
C-                           = 2 => Last block of current logical record
C-                           = 3 => First and only block of logical record
C-             IQUEST(2) = No. of data words.
C-                         For IOFLAG = 0 :
C-                           = Maximum number of words ZBUF can accept
C-                         For IOFLAG = 1 :
C-                           = Number of words in ZBUF to be transmitted
C-             IQUEST(1) = LUN for I/O stream ID
C-
C-   Outputs : ZBUF      = Data received for IOFLAG = 0
C-             IQUEST(1) = I/O status
C-                       =  0 => Success
C-                       = -1 => End of Data for IOFLAG = 0
C-                       >  0 => I/O error
C-   Controls: 
C-
C-   Created  13-AUG-1990   Michael Diesburg
C-   Modified 05-AUG-1992   Kirill Denisenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ACP_USER.INC'
      INCLUDE 'D0$INC:ACPINP.INC'
      INCLUDE 'D0$INC:ACPOUT.INC'
      INCLUDE 'D0$PARAMS:ACPDEF.PARAMS'
      INCLUDE 'D0$INC:QUEST.INC'

      INTEGER EVIN, EVOUT
      INTEGER IOFLAG,ZBUF(OUTWDS),NWDS,NTYP,NW,MESSAGE
      INTEGER MBYT,PN,NEVENT(4),IDUM,nblk,lun
      INTEGER ACKNOWLEDGE,DATAREADY,SENDDATA, DEATH
      INTEGER FLAG(512)                 ! FLAG(1) = I/O Status
                                        ! FLAG(2) = No. Bytes sent
      LOGICAL FIRST
      SAVE FIRST,PN
      DATA FIRST /.TRUE./
      DATA NEVENT / 4*0 /
      DATA MBYT /2048/
C
      IF(FIRST)  THEN
        CALL ACP_SIGRELEASE
        CALL ACP_DECLARE_BLOCK(INPBUF,INPBYT,INPBLOCK)
        CALL ACP_DECLARE_QUEUE(INPUTQ,0)
        CALL ACP_DECLARE_QUEUE(OUTPUTQ,0)
        CALL ACP_DECLARE_QUEUE(DONEQ,0)
        ACKNOWLEDGE = ACP_SET_OF_MESSAGE_TYPES
     &                (2,QACKNOWLEDGE,END_OF_INPUT)
        SENDDATA = ACP_SET_OF_MESSAGE_TYPES(1,SEND_DATA)
        DATAREADY = ACP_SET_OF_MESSAGE_TYPES(1,DATA_READY)
        EVIN = 0
        EVOUT = 0
        CALL ACP_SYNC(ACP$ALL_PROCESSES,STARTUP)
        FIRST = .FALSE.
      ENDIF

      NWDS = IQUEST(2)                ! No. words to send/get
      NTYP = IQUEST(3)                ! Type of record wanted


      IF(IOFLAG.EQ.0)  THEN             ! Data Input Request

        IF(NTYP.EQ.1)  THEN             ! Start new dataset
          NEVENT(1) = NEVENT(1) + 1
          CALL ACP_QUEUE_PROCESS(ACP$THIS_PROCESS,INPUTQ)
          CALL ACP_RECEIVE_MESSAGE(ACKNOWLEDGE,PN,MESSAGE,FLAG,MBYT)
          IF(MESSAGE.EQ.END_OF_INPUT)  THEN
            IQUEST(1) = -1
            IQUEST(2) =  0
            RETURN
          ENDIF
          CALL DATIME(IDUM,NEVENT(2))
          NEVENT(3) = MOD(NEVENT(2),100)
          NEVENT(2) = NEVENT(2)/100
          CALL ACP_UPDATE_USER_STATUS(4,NEVENT)
          EVIN = EVIN + 1
        ENDIF

        CALL ACP_TRANSMIT_MESSAGE(PN,SEND_DATA,NTYP,4)
        CALL ACP_RECEIVE_MESSAGE(DATAREADY,PN,MESSAGE,FLAG,MBYT)

        IQUEST(1) = FLAG(1)             ! Return I/O status
        IQUEST(2) = FLAG(2)             ! Return No. bytes sent

        IF(FLAG(2).GT.0)  THEN          ! Copy to ZEBRA buffer
          DO 20 NW = 1,FLAG(2)
            ZBUF(NW) = INPBUF(NW)
20        CONTINUE
        ENDIF

      ELSE                              ! Data Output Request

        IF(NTYP.EQ.1.OR.NTYP.EQ.3)  THEN             ! Start new dataset

          EVOUT = EVOUT + 1
          IF(EVIN.EQ.0.OR.(EVIN.EQ.1.AND.EVOUT.EQ.1.AND.NTYP.EQ.3)) THEN
            EVOUT = 0
            RETURN
          ELSEIF(EVOUT.GT.EVIN)  THEN
            CALL FTYPE('[ACP_FZIO] TRYING TO OUTPUT MORE THAN GOT',
     &                  EVIN,EVOUT)
            RETURN
          ENDIF

          CALL ACP_QUEUE_PROCESS(ACP$THIS_PROCESS,OUTPUTQ) 
          CALL ACP_RECEIVE_MESSAGE(ACKNOWLEDGE,PN,MESSAGE,FLAG,MBYT)
        ENDIF

        CALL ACP_RECEIVE_MESSAGE(SENDDATA,PN,MESSAGE,FLAG,MBYT)
        CALL ACP_SEND(PN,ZBUF,NWDS*4,OUTBLOCK,0)
        CALL ACP_TRANSMIT_MESSAGE(PN,DATA_READY,NTYP,4)

        IQUEST(1) = 0
        
      ENDIF

      RETURN

      END
