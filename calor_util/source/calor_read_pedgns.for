      SUBROUTINE CALOR_READ_PEDGNS(FILENAM1,FILENAM2,TASK,NCRT,CRATES,
     &  IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in Pedestal and/or gain file from DBL3
C-                         or from a file
C-
C-   Inputs  : FILENAM1 : Pedestal file name 
C-             FILENAM2 : Gains file name 
C-             TASK     : 0 = peds and gains
C-                        1 = peds
C-                        3 = gains
C-             NCRT     : Number of crates being used
C-             CRATES   : Array containing list of crates being used
C-   Outputs : IER      : 0 ok
C-
C-   Created  29-JUN-1990   Chip Stewart, Marcel Demarteau
C-   Updated  20-NOV-1990   Jan Guida  Added CRATES variable 
C-   Updated  17-Mar-1992   Herbert Greenlee
C-     Changed OPEN to D0OPEN
C-   Updated  19-APR-1993   Meenakshi Narain  Add Bypass DBL3 error switch 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
C
      LOGICAL OK,YES,OPENED
      LOGICAL DO_PEDFILE,DO_GNSFILE
C
      INTEGER LIN,ERR,IDEX1,IDEX2,IZREAD,LREAD,BASE
      INTEGER LSTPC,N1,N2,LEN,LRUN,TASK,IER,J1,J2,ICRT,DB_NCRT,DB_ICRT
      INTEGER IOS,I,NN,LOGLEV,RUN,NRUN,PEDRUN(0:11),GAINRUN(0:11)
      INTEGER PEDRUNNO,GNSRUNNO,PRUN,NCRT,CRATES(*),DB_CRATES(15)
C
      CHARACTER*(*) FILENAM1, FILENAM2
      CHARACTER*80 MSG_STRING
      CHARACTER*80 FILENAME,FILE1, FILE2 
      LOGICAL BYPASS_DBL3_ERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C
C----------------------------------------------------------------------
      DATA LOGLEV/-2/
C----------------------------------------------------------------------
C
C ****  READ THE DBL3 BYPASS ERROR SWITCH STATE FROM CAHITS_RCP
C
      IF(FIRST)THEN                     ! LOCAL INIT
        BYPASS_DBL3_ERROR = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0) THEN
          CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
          IF(IER.NE.0) CALL ERRMSG('NO_ERR_SWITCH','READ_PEDGNS',
     &      'USE DBL3 BYPASS ERROR SWITCH  = FALSE AS DEFAULT','W')
        END IF
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF

C
C ****  MAKE LIST DB_CRATES TO FETCH ALL POSSIBLE CRATES FROM DBL3
C
      DB_NCRT = 0
      DO BASE = ADCR00,ADCR01
        DO I = 0, NADCRC-1
         DB_NCRT = DB_NCRT + 1
         DB_CRATES(DB_NCRT) = 10*I + BASE
       END DO
      END DO
C
C ****  Add TB91 CRATE=87 to list if CRATES wants it.
C
      IF (CRATES(1).EQ.87) THEN
        DB_NCRT = 1
        DB_CRATES(1) = 87
      END IF
C
C ****  If the filename is DBL3 then read pedestals from the data base
C
      PRUN=0
      YES=.TRUE.
      CALL UPCASE(FILENAM1,FILE1)
      CALL UPCASE(FILENAM2,FILE2)
      IF(TASK.EQ.1) FILENAME = FILE1
      IF(TASK.EQ.3) FILENAME = FILE2
      IF(TASK.EQ.0) THEN
        IDEX1=INDEX(FILE1,'DBL3')
        IDEX2=INDEX(FILE2,'DBL3')
        IF(IDEX1.GT.0 .AND. IDEX2.GT.0) THEN    ! both DBL3 
          J1 = INDEX(FILE1,'@')
          J2 = INDEX(FILE2,'@')
          IF (J1.GT.0 .AND. J2.GT.0) THEN         ! both have run numbers
            IF(FILE1.NE.FILE2) THEN      
              CALL ERRMSG('PEDGNS','READ_PEDGNS',
     &          'Weird case, DBL3 ped run number taken','W')
            ENDIF 
            FILENAME = FILE1
          ELSEIF (J1.GT.0) THEN                 ! only ped has run number
            FILENAME = FILE1
          ELSEIF(J2.GT.0) THEN                  ! only gains has run number
            FILENAME = FILE2                  
          ELSE                                  ! neither has run number
            FILENAME = FILE1
          ENDIF
C                                                
        ELSEIF(IDEX1.GT.0 ) THEN                ! peds DBL3, gains file
          TASK = 1
          FILENAME = FILE1
          DO_GNSFILE = .TRUE.
        ELSEIF(IDEX2.GT.0 ) THEN                ! gains DBL3, peds file
          TASK = 3
          FILENAME = FILE2
          DO_PEDFILE = .TRUE.
        ELSE                                    ! both from file
          DO_GNSFILE = .TRUE.
          DO_PEDFILE = .TRUE.
          FILENAME = FILE1
        ENDIF
      ENDIF
C          
      IDEX1=INDEX(FILENAME,'DBL3')
      IF (IDEX1.GT.0.) THEN
        NRUN = IQ(LHEAD+6)
        WRITE(MSG_STRING,1001)NRUN
        CALL INTMSG(MSG_STRING)
        LRUN = INDEX(FILENAME,'@')
        YES = (LRUN.EQ.0)
        IF ( YES ) THEN
          CALL CDBINI(NRUN,DB_NCRT,DB_CRATES,TASK,OK)
          IF (.NOT. OK) THEN
            IF (BYPASS_DBL3_ERROR) THEN
              CALL INTMSG(' Cannot READ DBL3 GAINS FILE')          
            ELSE
              CALL ERRMSG('CALORIMETER','READ_PEDGNS',
     &        ' Error in reading DBL3 GAINS file','F')
            ENDIF
            GOTO 999
          END IF
          GOTO 20
        ELSE
          CALL SWORDS(FILENAME,N1,N2,LEN)
          IF ( (N2-LRUN).EQ.4) THEN
            READ(FILENAME(LRUN+1:N2),'(I4)')PRUN
            PRUN = PRUN + 1000000
          ELSE IF ( (N2-LRUN).EQ.7) THEN
            READ(FILENAME(LRUN+1:N2),'(I7)')PRUN
          ELSE
            PRUN=NRUN
            CALL INTMSG(' No DBL3 gains run given, default taken')
          ENDIF
          CALL CDBINI(PRUN,DB_NCRT,DB_CRATES,TASK,OK)
          IF (.NOT. OK) THEN
            IF (BYPASS_DBL3_ERROR) THEN
              CALL INTMSG(' Cannot READ DBL3 GAINS FILE')          
            ELSE
              CALL ERRMSG('CALORIMETER','READ_PEDGNS',
     &        ' Error in reading DBL3 GAINS file','F')
            ENDIF
            GOTO 999
          END IF
        ENDIF
   20   CALL CLBRUN(NCRT,CRATES,PEDRUN,GAINRUN)
        IF(TASK .EQ. 0 .OR. TASK.EQ. 3) THEN
          DO ICRT = 1, NCRT 
            WRITE(MSG_STRING,1002)CRATES(ICRT),' GAINS ',GAINRUN(ICRT-1)
            CALL INTMSG(MSG_STRING)
          END DO
        ENDIF
        IF(TASK .EQ. 0 .OR. TASK.EQ. 1) THEN
          DO ICRT = 1, NCRT 
            WRITE(MSG_STRING,1002)CRATES(ICRT),' PEDS  ',PEDRUN(ICRT-1)
            CALL INTMSG(MSG_STRING)
          END DO
        END IF
        IF(DO_PEDFILE) THEN 
          FILENAME = FILE1
          GOTO 10
        ENDIF
        IF(DO_GNSFILE) THEN 
          FILENAME = FILE2
          GOTO 10
        ENDIF
        GOTO 900
      END IF
C
C ****  To read from a file.
C
C      
   10 CALL GTUNIT (2,LIN,ERR)
      CALL D0OPEN (LIN,FILENAME,'IU',OPENED)
      IF (.NOT.OPENED)THEN
        CALL INTMSG(' Cannot open file')
        CALL RLUNIT(2,LIN,ERR)
        OK = .FALSE.
        GO TO 999
      ENDIF
C
      CALL FZFILE(LIN,0,'I')
      CALL FZLOGL (LIN,LOGLEV)
C
C  Read.
C
      LSTPC=LC(LSTPH-IZSTPC)
      LSCAL=LC(LSTPC-IZSCAL)
      IF(LSCAL.EQ.0) 
     &  CALL MZBOOK(IDVSTP,LSCAL,LSTPC,-IZSCAL,'SCAL',4,4,10,2,0)
      IF(DO_PEDFILE) THEN
        IZREAD = IZCPDH
        DO_PEDFILE = .FALSE.
      ELSEIF(DO_GNSFILE) THEN
        IZREAD = IZCGNH
        DO_GNSFILE = .FALSE.
      ENDIF
      CALL FZIN (LIN,IDVSTP,LSCAL,-IZREAD,' ',0,0)
      IOS = IQUEST(1)
      IF (IOS.NE.0) THEN
        WRITE(MSG_STRING,51) IOS
        CALL INTMSG(MSG_STRING)
        OK = .FALSE.
        GO TO 999
      ENDIF
      LREAD=LC(LSCAL-IZREAD)
C
C   Get run number
C
      IF(LREAD.LE.0) THEN
        MSG_STRING=' Cannot find pedestal/gains header'
        CALL INTMSG(MSG_STRING)
        OK = .FALSE.
        GOTO 999
      ELSE
        RUN=IC(LREAD+6)
        IF(IZREAD.EQ.IZCPDH) THEN 
          WRITE(MSG_STRING,55)RUN
   55     FORMAT(' RUN NUMBER of pedestal file:',I10)
          PEDRUN(0) = RUN
        ELSE
          WRITE(MSG_STRING,56)RUN
   56     FORMAT(' RUN NUMBER of gains file:',I10)
          GAINRUN(0) = RUN
        ENDIF
        CALL INTMSG(MSG_STRING)
      ENDIF
C
C  CLOSE FILE.
C
      CALL FZENDI(LIN,'T')
      CLOSE(UNIT=LIN)
      CALL RLUNIT(2,LIN,ERR)
      IF(DO_PEDFILE .OR. DO_GNSFILE) GOTO 10
C
   51 FORMAT(' Error reading file, IQUEST(1)=',I3)
  900 CALL INTMSG(' CALOR_READ_PEDGNS FINISH')
 1001 FORMAT(' CALOR_READ_PEDGNS: DATAFILE RUN =',I10)
 1002 FORMAT('    CRATE ',   I5,      A8,' RUN =',I10)
C
C
  999 RETURN
C
      ENTRY PEDRNO(PEDRUNNO)
      PEDRUNNO = PEDRUN(0)
      RETURN
C
      ENTRY GNSRNO(GNSRUNNO)
      GNSRUNNO = GAINRUN(0)
      RETURN
      END
