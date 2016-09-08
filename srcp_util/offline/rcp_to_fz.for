      PROGRAM RCP_TO_FZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan an RCP file and compute the number of
C-                         identifiers (including comments) and values
C-                         it contains and update the \SIZE parameter.
C-                         Then use INRCP to read the RCP file into memory
C-                         and EZOUT to write the SRCP bank to an FZ file.
C-
C-                         Define RCP$INPUT to be the input RCP file
C-
C-                         Define FZ$OUTPUT to be the output FZ file
C-
C-   Created  19-SEP-1990   Harrison B. Prosper
C-   Updated  10-APR-1991   Harrison B. Prosper
C-      Can now operate on multiple RCP-banks/file
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version
C-   Updated  23-APR-1992   James T. Linnemann  
C-      Use M switch in D0OPEN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RCP_INPUT
      PARAMETER( RCP_INPUT  = 'RCP$INPUT' )
      CHARACTER*(*) FZ_OUTPUT
      PARAMETER( FZ_OUTPUT  = 'FZ$OUTPUT' )
C
      INTEGER LUN,LUNOUT,MAXBANKS
      PARAMETER( LUN = 20 )
      PARAMETER( LUNOUT = 40 )
      PARAMETER( MAXBANKS = 100 )
C
      CHARACTER*80 STRING,FILENAME(MAXBANKS),RECORD
C JTL .........      CHARACTER*32 RCP_BANK(10)
      CHARACTER*32 RCP_BANK(MAXBANKS)
      INTEGER NUMBER_VALUES, NUMBER_IDENTIFIERS
      INTEGER NUMBER_RCP_BANKS, IER, TRNLNM, STATUS, I,J,K,L
      INTEGER NUMBER_FILES
      LOGICAL ACTIVE,OPENED,OK
C----------------------------------------------------------------------
C
C ****  Check logicals
C
      STATUS = TRNLNM(RCP_INPUT,STRING,L)
      IF ( iand(STATUS,1).eq.0 ) THEN
        CALL INTMSG
     &   (' %RCP_TO_FZ-E-UNDEFINED, Please DEFINE logical '//RCP_INPUT)
        GOTO 999
      ENDIF
C
      STATUS = TRNLNM(FZ_OUTPUT,STRING,L)
      IF ( iand(STATUS,1).eq.0 ) THEN
        CALL INTMSG
     &   (' %RCP_TO_FZ-E-UNDEFINED, Please DEFINE logical '//FZ_OUTPUT)
        GOTO 999
      ENDIF
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA (0)
C
C ****  Initialize STP structure
C
      CALL INZSTP
C
C ****  Split RCP-file into separate files
C
      CALL D0OPEN (LUN, RCP_INPUT, 'I', OK)
      IF(.NOT.OK)GO TO 900
C
      ACTIVE = .TRUE.
      OPENED = .FALSE.
      NUMBER_FILES = 0
C
      DO WHILE ( ACTIVE )
        READ(LUN,FMT='(A)',END=100) RECORD
        CALL WORD(RECORD,I,J,L)
        CALL UPCASE(RECORD(I:J),STRING)
C
C ****  Open file if record contains \START
C
        IF ( STRING(1:L) .EQ. '\START' ) THEN
          NUMBER_FILES = NUMBER_FILES + 1
          STRING = ' '
          WRITE(STRING,'(I3.3,''TMP.RCP'')') NUMBER_FILES
          FILENAME(NUMBER_FILES) = STRING
          CALL D0OPEN (LUNOUT,STRING,'O',OK)
          IF(.NOT.OK)GO TO 950
          OPENED = .TRUE.
        ENDIF
C
C ****  Write out record
C
        IF ( OPENED ) THEN
          CALL SWORDS(RECORD,I,J,K)
          WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:J)
        ENDIF
C
C ****  Close file if record contains \STOP
C
        IF ( STRING(1:L) .EQ. '\STOP' ) THEN
          CLOSE(UNIT=LUNOUT)
          OPENED = .FALSE.
        ENDIF
      ENDDO
  100 CONTINUE
      CLOSE(UNIT=LUN)
C
C ****  Compute exact parameters for \SIZE and update the
C ****  RCP file.
C
      CALL INTMSG(' ')
      CALL INTMSG(' %RCP_TO_FZ-I-GETSIZE,  Computing \SIZE parameters')
C
      DO I =  1, NUMBER_FILES
        STRING = FILENAME(I)
        CALL D0OPEN (LUN,FILENAME(I),'M',OK)
        IF(.NOT.OK)GO TO 950
        CALL EZASIZ (LUN,LUNOUT,FILENAME(I),
     &             NUMBER_VALUES,
     &             NUMBER_IDENTIFIERS)
        CLOSE(UNIT=LUN,STATUS='DELETE')
        STRING = ' '
        WRITE(STRING,910) NUMBER_VALUES+10,NUMBER_IDENTIFIERS+10
        CALL INTMSG(STRING)
      ENDDO
C
C ****  Read RCP file into memory
C
      CALL INTMSG(' %RCP_TO_FZ-I-READ,  Reading RCP file')
      CALL INRCP_COMPRESS_BANK
      DO I  =  1, NUMBER_FILES
        CALL INRCP(FILENAME(I),IER)
        IF ( IER .NE. 0 ) GOTO 900
        CALL INRCP_LIST(RCP_BANK(I),NUMBER_RCP_BANKS)
        CALL INTMSG(' Read RCP bank: '//RCP_BANK(I))
C
C ****  Delete temporary RCP file
C
        STRING = FILENAME(I)
        CALL D0OPEN (LUN,FILENAME(I),'M',OK)
        IF(.NOT.OK)GO TO 950
        CLOSE(UNIT=LUN,STATUS='DELETE')
      ENDDO
      NUMBER_RCP_BANKS = NUMBER_FILES
C
C ****  Chain RCP banks together
C
      CALL EZCHAIN(RCP_BANK,NUMBER_RCP_BANKS)
C
C ****  Write out RCP banks to an FZ file
C
      CALL ZZOPEN (LUNOUT,FZ_OUTPUT,IER,'OUTPUT')
      IF ( IER .EQ. 0 ) THEN
C
        CALL INTMSG(' %RCP_TO_FZ-I-GETSIZE,  Writing FZ file')
        CALL EZOUT  (LUNOUT,RCP_BANK(1))
C
        CALL ZZCLOS (LUNOUT,IER,'OUTPUT')
        IF ( IER .NE. 0 ) THEN
          CALL INTMSG
     &      (' %RCP_TO_FZ-E-CLOSE, Error closing output FZ file')
        ENDIF
      ELSE
        CALL INTMSG
     &      (' %RCP_TO_FZ-E-OPEN, Error opening output FZ file')
      ENDIF
      GOTO 999
C
C ****  ERROR OPENING RCP file
C
  900 CONTINUE
      STRING = ' '
      WRITE(STRING,920) RCP_INPUT
      CALL INTMSG(STRING)
      GOTO 999

  950 CONTINUE
      CALL INTMSG(' Error opening file '//STRING)
C
C ****  FORMATS
C
  910 FORMAT(' %RCP_TO_FZ-I-RCPSIZE,   \SIZE   ',2i10)
  920 FORMAT(' %RCP_TO_FZ-F-NOTFOUND, RCP file ',A,' Not found')
C
  999 CONTINUE
      END
