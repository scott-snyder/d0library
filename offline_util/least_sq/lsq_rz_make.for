      SUBROUTINE LSQ_RZ_MAKE(NEW,READONLY,UNIT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make RZ file for holding LSQ_MATRICES
C-
C-   Inputs  :NEW = TRUE, WILL OPEN NEW FILE, ELSE OPEN RZ FILE
C-   Outputs :UNIT = UNIT NUMBER OF OPENED FILE
C-            OK = TRUE IF AOK
C-   Controls:
C-
C-   Updated  21-FEB-1992   Rajendran Raja   
C-   Updated   2-Sep-1992   Herbert Greenlee
C-       Changed OPEN to D0RZOPEN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NEW,OK,OPENOK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LSQ_USER
      INTEGER NSTRING,LENGTH,IER,UNIT
      CHARACTER*80 FILENAME
C
      INCLUDE 'D0$INC:LSQ_PARS.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER PRIM_ALLOC,LRECL
      INTEGER NCHAR,TRULEN,LENTOP,I,RZ_LOG_LEVEL
      CHARACTER*40 DIR_SUB
      LOGICAL READONLY
      CHARACTER*4 RZF_MODE
C----------------------------------------------------------------------
      OK = .TRUE.
      CALL EZPICK('LSQ_RCP')
      CALL EZGET('LSQ_USER',LSQ_USER,IER)
      CALL EZGET('PRIMARY_ALLOCATION',PRIM_ALLOC,IER)
      CALL EZGET('RECORD_LENGTH',LRECL,IER)
      CALL EZ_GET_CHARS('RZ_FILE_NAME',NCHAR,FILENAME,IER)
      LENGTH = TRULEN(FILENAME)
      CALL EZ_GET_CHARS('RZ_FILE_MODE',NCHAR,RZF_MODE,IER)
      CALL EZ_GET_CHARS('SUB_DIRECTORY',NCHAR,SUB_DIRECTORY,IER)
      CALL EZ_GET_CHARS('TOP_DIRECTORY',NCHAR,TOP_DIRECTORY,IER)
      CALL EZGET('RZ_LOG_LEVEL',RZ_LOG_LEVEL,IER)
      CALL EZRSET
C
      IF(IER.NE.0)OK = .FALSE.
      IF ( NEW ) THEN
        CALL GTUNIT(LSQ_USER,UNIT,IER)
        CALL D0RZOPEN(UNIT,FILENAME,'OU',4*LRECL,OPENOK)
        IF(.NOT.OPENOK)GO TO 777
        CALL RZMAKE(UNIT,TOP_DIRECTORY,NKEYS,KEY_DESCR,CHTAG,
     &  PRIM_ALLOC,' ')
        CALL INTMSG(' Opened new RZ file '//FILENAME(1:LENGTH)
     &  // ' successfully')
C
        CALL RZCDIR('//'//TOP_DIRECTORY,' ')
      ELSE
        CALL GTUNIT(LSQ_USER,UNIT,IER)
        IF ( READONLY ) THEN
          CALL D0RZOPEN(UNIT,FILENAME,'IU',4*LRECL,OPENOK)
        ELSE
          CALL D0RZOPEN(UNIT,FILENAME,'IOU',4*LRECL,OPENOK)
        ENDIF
        IF(.NOT.OPENOK)GO TO 777
        CALL RZFILE(UNIT,TOP_DIRECTORY,RZF_MODE)
        IF ( IQUEST(1).EQ.1 ) THEN
          CALL INTMSG(' RZ File is already open '//FILENAME(1:LENGTH))
        ELSEIF(IQUEST(1).EQ.0) THEN
          CALL INTMSG(' Opened existing RZ File '//FILENAME(1:LENGTH)
     &  // ' Successfully')
        ENDIF
C
        CALL RZCDIR('//'//TOP_DIRECTORY,' ')
        CALL RZCDIR(SUB_DIRECTORY,' ')
        IF(IQUEST(1).EQ.1)THEN
          LENTOP = TRULEN(TOP_DIRECTORY)
          DIR_SUB = TOP_DIRECTORY(1:LENTOP)//'/'//SUB_DIRECTORY
          IF ( READONLY ) THEN
            WRITE(MSG,2)DIR_SUB
    2       FORMAT(' Non existent subdirectory ',A)
            CALL INTMSG(MSG)
          ELSE
            WRITE(MSG,3)DIR_SUB
    3       FORMAT(' Creating new subdirectory ',A)
            CALL INTMSG(MSG)
            CALL RZMDIR(SUB_DIRECTORY,NKEYS,KEY_DESCR,CHTAG)
          ENDIF
        ENDIF
      ENDIF
C
      CALL RZLOGL(UNIT,RZ_LOG_LEVEL)    ! SET THE LOGGING LEVEL
      GO TO 999
  777 CALL INTMSG(' Error Opening RZ file ')
      OK = .FALSE.
  999 RETURN
      END
