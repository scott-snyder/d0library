      SUBROUTINE HMATRIX_RZ_MAKE(NEW,READONLY,UNIT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make RZ file for holding HMATRICES
C-
C-   Inputs  :NEW = TRUE, WILL OPEN NEW FILE, ELSE OPEN RZ FILE
C-   Outputs :UNIT = UNIT NUMBER OF OPENED FILE
C-            OK = TRUE IF AOK
C-   Controls:
C-
C-   Created  28-NOV-1990   Rajendran Raja
C-   Updated  25-FEB-1992   Meenakshi Narain   
C-                          add freedom to create nested sub-directories
C-   Updated  27-Aug-1992   Change OPEN to D0RZOPEN
C-   Updated   3-FEB-1994   Rajendran Raja  Zebra IQUEST convention change 
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NEW,OK,OPENOK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER HMATRIX_USER
      INTEGER NSTRING,LENGTH,IER,UNIT
      CHARACTER*80 FILENAME
C
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER PRIM_ALLOC,LRECL
      INTEGER NCHAR,TRULEN,LENTOP,I,RZ_LOG_LEVEL
      INTEGER START_INDEX, END_INDEX, LENSUB
      CHARACTER*40 DIR_SUB
      LOGICAL READONLY
      CHARACTER*4 RZF_MODE
      LOGICAL LIST_DIR
C----------------------------------------------------------------------
      OK = .TRUE.
      CALL EZPICK('HMATRIX_RCP')
      CALL EZGET_i('HMATRIX_USER',HMATRIX_USER,IER)
      CALL EZGET_i('PRIMARY_ALLOCATION',PRIM_ALLOC,IER)
      CALL EZGET_i('RECORD_LENGTH',LRECL,IER)
      CALL EZ_GET_CHARS('RZ_FILE_NAME',NCHAR,FILENAME,IER)
      LENGTH = TRULEN(FILENAME)
      CALL EZ_GET_CHARS('RZ_FILE_MODE',NCHAR,RZF_MODE,IER)
      CALL EZ_GET_CHARS('SUB_DIRECTORY',NCHAR,SUB_DIRECTORY,IER)
      CALL EZ_GET_CHARS('TOP_DIRECTORY',NCHAR,TOP_DIRECTORY,IER)
      CALL EZGET('RZ_LOG_LEVEL',RZ_LOG_LEVEL,IER)
      CALL EZGET_l('LIST_RZ_DIRECTORY',LIST_DIR,IER)
      CALL EZRSET
C
      IF(IER.NE.0)OK = .FALSE.
      IF ( NEW ) THEN
        CALL GTUNIT(HMATRIX_USER,UNIT,IER)
        CALL D0RZOPEN(UNIT,FILENAME,'OU',4*LRECL,OPENOK)
        IF(.NOT.OPENOK)GO TO 777
        CALL RZMAKE(UNIT,TOP_DIRECTORY,NKEYS,KEY_DESCR,CHTAG,
     &  PRIM_ALLOC,' ')
        CALL INTMSG(' Opened new RZ file '//FILENAME(1:LENGTH)
     &  // ' successfully')
C
        CALL RZCDIR('//'//TOP_DIRECTORY,' ')
C  check for nested sub-directory structure
        LENSUB = TRULEN(SUB_DIRECTORY)
        START_INDEX = 1
   50   END_INDEX = INDEX(SUB_DIRECTORY(START_INDEX:LENSUB),'/')
        IF (END_INDEX.NE.0) THEN
          CALL RZMDIR(SUB_DIRECTORY(START_INDEX:END_INDEX-1)
     &                ,NKEYS,KEY_DESCR,CHTAG)
          CALL RZCDIR(SUB_DIRECTORY(START_INDEX:END_INDEX-1),' ')
          START_INDEX = END_INDEX + 1
          GO TO 50
        ELSE
          CALL RZMDIR(SUB_DIRECTORY(START_INDEX:LENSUB)
     &                ,NKEYS,KEY_DESCR,CHTAG)
          CALL RZCDIR(SUB_DIRECTORY(START_INDEX:LENSUB),' ')
        END IF
        LENTOP = TRULEN(TOP_DIRECTORY)
        DIR_SUB = TOP_DIRECTORY(1:LENTOP)//'/'//SUB_DIRECTORY
        WRITE(MSG,1)DIR_SUB
    1   FORMAT(' Created sub_directory ',A)
        CALL INTMSG(MSG)
      ELSE
        CALL GTUNIT(HMATRIX_USER,UNIT,IER)
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
        IF(IQUEST(1).NE.0)THEN    !Zebra changed its convention
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
C  check for nested sub-directory structure
            LENSUB = TRULEN(SUB_DIRECTORY)
            START_INDEX = 1
    5       END_INDEX = INDEX(SUB_DIRECTORY(START_INDEX:LENSUB),'/')
            IF (END_INDEX.NE.0) THEN
              CALL RZCDIR(SUB_DIRECTORY(START_INDEX:END_INDEX-1),' ')
              IF (IQUEST(1).EQ.1) THEN
                CALL RZMDIR(SUB_DIRECTORY(START_INDEX:END_INDEX-1)
     &                ,NKEYS,KEY_DESCR,CHTAG)
                CALL RZCDIR(SUB_DIRECTORY(START_INDEX:END_INDEX-1),' ')
              END IF
              START_INDEX = END_INDEX + 1
              GO TO 5
            ELSE
              CALL RZMDIR(SUB_DIRECTORY(START_INDEX:LENSUB)
     &                ,NKEYS,KEY_DESCR,CHTAG)
              CALL RZCDIR(SUB_DIRECTORY(START_INDEX:LENSUB),' ')
            END IF
          ENDIF
        ENDIF
      ENDIF
C
      CALL RZLOGL(UNIT,RZ_LOG_LEVEL)    ! SET THE LOGGING LEVEL
      CALL RZCDIR('//'//TOP_DIRECTORY,' ')
      IF ( LIST_DIR ) THEN
      CALL HMATRIX_RZ_LIST
      ENDIF
      GO TO 999
  777 CALL INTMSG(' Error Opening RZ file ')
      OK = .FALSE.
  999 RETURN
      END
