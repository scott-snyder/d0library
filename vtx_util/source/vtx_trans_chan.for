      SUBROUTINE VTX_TRANS_CHAN(LOGID,CRATE,ECHAN,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate between VTX logical channel numbers and
C-                         electronic channel numbers.  A value of -1 is
C-                         returned for invalid calls.
C-
C-    IFL = 1:  LOGID ==> CRATE, ECHAN
C-    IFL = 2:  CRATE, ECHAN ==> LOGID
C-
C-  NOTE: CRATE = 10*ICRT + 3, ie, 3, 13, 23, . . . , 93
C-
C-   Controls: IFL
C-
C-   Created   3-OCT-1992   Peter M. Grudberg from V.D.Elvira's routines
C-                              VTX_LOGI_CHA and COMP_FILE
C-   Updated   5-FEB-1993   Susan K. Blessing  Check existance of VTXCOFF_RCP
C-    before calling EZPICK and possibly generating an error message.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LOGID, CRATE, ECHAN, IFL
C
      INTEGER MXLOGIC, NCRT, NCHAN
      PARAMETER ( MXLOGIC = 16383 ) ! Allow 13 bits
      PARAMETER ( NCRT = 10 )
      PARAMETER ( NCHAN = 256 )
      INTEGER ELECT(2,0:MXLOGIC), LOGIC(0:NCHAN-1,0:NCRT-1)
      INTEGER CHAN, LCHAN, LEN, IER, ICRT
      INTEGER IUSER, LUN
      INTEGER LRCP
      CHARACTER*60 FILENAME(0:9)
      LOGICAL FIRST, EZERROR, OPENED
      DATA IUSER / 666 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZLOC('VTXCOFF_RCP',LRCP)
        IF (LRCP.GT.0) THEN 
          CALL EZPICK('VTXCOFF_RCP')
        END IF
        IF ( LRCP.EQ.0.OR.EZERROR(IER) ) THEN
          DO ICRT = 0, 9
            WRITE(FILENAME(ICRT),111) ICRT*10+3
  111       FORMAT('D0$L2CALIB$PARAMS:CHN_PARAMS_',I3.3,'.DAT')
          ENDDO
        ELSE
          DO ICRT = 0, 9
            CALL EZGETS('CHN_PARAMS_FILES',ICRT+1,FILENAME(ICRT),
     &        LEN,IER)
            IF ( IER .NE. 0 ) THEN
              WRITE(FILENAME(ICRT),111) ICRT*10+3
            ENDIF
          ENDDO
          CALL EZRSET
        ENDIF
        CALL VZERO(ELECT,2*MXLOGIC)
        CALL VZERO(LOGIC,NCRT*NCHAN)
        CALL GTUNIT(IUSER,LUN,IER)
        DO ICRT = 0, 9
          CALL D0OPEN(LUN,FILENAME(ICRT),'IF',OPENED)
          IF ( .NOT. OPENED ) THEN
            CALL ERRMSG('File not found','VTX_TRANS_CHAN',
     &        'CHN_PARAMS file not found','F')
          ENDIF
    5     READ(LUN,112,END=15) CHAN, LCHAN
  112     FORMAT(3X,I3,15X,I4,/)
          ELECT(1,LCHAN) = 10*ICRT + 3
          ELECT(2,LCHAN) = CHAN
          LOGIC(CHAN,ICRT) = LCHAN
          GO TO 5
   15     CONTINUE
          CLOSE(LUN)
        ENDDO
        CALL RLUNIT(IUSER,LUN,IER)
      ENDIF
C
C ****  The translation arrays are now filled, so use them.
C
      IF ( IFL .LE. 1 ) THEN  ! LOGICAL ==> ELECTRONIC
        IF ( LOGID .LE. MXLOGIC .AND. LOGID .GE. 0 ) THEN
          CRATE = ELECT(1,LOGID)
          ECHAN = ELECT(2,LOGID)
        ELSE
          CRATE = -1
          ECHAN = -1
        ENDIF
      ELSEIF ( IFL .GE. 2 ) THEN
        ICRT = CRATE / 10
        IF ( ECHAN .LT. NCHAN .AND. ECHAN .GE. 0 .AND.
     &       ICRT .LT. NCRT .AND. ICRT .GE. 0 ) THEN
          LOGID = LOGIC(ECHAN,ICRT)
        ELSE
          LOGID = -1
        ENDIF
      ENDIF
C
  999 RETURN
      END
