      PROGRAM TESTRCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TEST of RCP
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAY-1990   Harrison B. Prosper
C-   Updated  24-APR-1991   Harrison B. Prosper  
C-      Add command line input 
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER
      LOGICAL ACTIVE,EZERROR,EZCHEK
      CHARACTER*32 RCP_BANK,PARAM
      CHARACTER*80 ZEBRA,STRING,RCP_FILE,FILENAME
      INTEGER I,NEXT,LINE
      INTEGER LUNOUT
      PARAMETER( LUNOUT = 2 )
      INTEGER  STATUS,LENGTH
      LOGICAL OK
      INTEGER  FLAGS
C----------------------------------------------------------------------
C
C ****  Get command line
C
C
C&IF VAXVMS
      INTEGER  LIB$GET_FOREIGN
      FLAGS = 0
      STATUS = LIB$GET_FOREIGN(RCP_FILE,,LENGTH,FLAGS)
      IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C&ELSE
C&      INTEGER LENOCC
C&C--------------------------------------------------------------------
C&      PRINT '(A,$)',' RCP_FILE >'
C&      READ (5,'(A)') RCP_FILE
C&      LENGTH=LENOCC(RCP_FILE)
C&ENDIF
      RCP_FILE = RCP_FILE(1:LENGTH)
C----------------------------------------------------------------------
C
      CALL MZEBRA(0)                    ! (1)  Initialize ZEBRA
C
      CALL INZSTP                       ! (2)  Setup /ZEBSTP/
C
C ****  Do NOT abort on bad rcp-value
C
      CALL EZ_ABORT_ON_BAD_VALUE(.FALSE.)
C
C ****  Command line input
C
      IF ( LENGTH .GT. 0) THEN
C
C ****  Check extension
C
        CALL UPCASE(RCP_FILE(1:LENGTH),RCP_FILE(1:LENGTH))
        I = INDEX(RCP_FILE(1:LENGTH),'.RCP')
        IF ( I .LE. 0 ) THEN
          CALL INTMSG(' %RCPTEST-E-BADFILEEXT: '//RCP_FILE(1:LENGTH))
          GOTO 999
        ENDIF
C
C ****  Read RCP-file
C
        CALL INRCP(RCP_FILE(1:LENGTH),IER)
C
        IF ( IER .NE. 0 ) THEN
          CALL INTMSG(' %RCPTEST-E-Problem reading file: '//
     &    RCP_FILE(1:LENGTH))
        ELSE
          CALL INTMSG
     &    (' %RCPTEST-S-SUCCESS, Normal successful completion')
        ENDIF
        GOTO 999
      ENDIF
C
C
C ****  Interactive input
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        WRITE(6,FMT='('' Name of RCP file: '',$)')
        READ (5,FMT='(A)') RCP_FILE
C
        IF ( RCP_FILE(1:1) .EQ. ' ' ) THEN
          WRITE(6,FMT='('' Name of RCP ZEBRA file: '',$)')
          READ (5,FMT='(A)') ZEBRA
C
C ****  Read in banks
C
          IF ( ZEBRA(1:1) .EQ. ' ' ) THEN
            IER =-1
            ACTIVE = .FALSE.
          ELSE
            CALL ZZOPEN(LUNOUT,ZEBRA,IER,'INPUT')
            CALL EZIN  (LUNOUT,' ')
            CALL ZZCLOS(LUNOUT,IER,'INPUT')
          ENDIF
        ELSE
          CALL INRCP(RCP_FILE,IER)
        ENDIF
C
        IF ( IER .NE. 0 ) THEN
          IF ( ACTIVE ) THEN
            WRITE(6,FMT='('' Problem reading file!!'')')
          ENDIF
        ELSE
          DO WHILE ( ACTIVE )
            CALL EZDBUG(6)
            WRITE(6,FMT='('' Name of RCP BANK: '',$)')
            READ (5,FMT='(A)') RCP_BANK
C
            IF ( RCP_BANK(1:1) .EQ. ' ' ) THEN
              ACTIVE = .FALSE.
            ELSE
              CALL UPCASE (RCP_BANK,RCP_BANK)
              I = INDEX(RCP_BANK,'/DROP')
              IF ( I .GT. 0 ) THEN
                CALL EZDROP (RCP_BANK(1:I-1))
              ELSE
C
C ****  PICK SRCP bank
C
                CALL EZPICK(RCP_BANK)
                IF ( EZERROR(IER) ) THEN
                  WRITE(6,FMT='('' Problem PICKING bank: '',A)')
     &              RCP_BANK
                ELSE
C
C ****  Check if all parameters are present
C
                  WRITE(6,FMT='('' Looking for parameters...'')')
                  NEXT = 1
                  LINE = 0
                  DO WHILE ( NEXT .GT. 0 )
                    LINE = LINE + 1
                    IF ( LINE .EQ. 23 ) THEN
                      LINE = 0
                      WRITE(6,FMT=
     &                  '(''          Press RETURN to continue'',$)')
                      READ (5,FMT='(A)') STRING
                    ENDIF
C
                    CALL EZGET_NEXT_NAME(PARAM,NEXT)
                    WRITE(6,FMT='('' EZCHEK ('',A,'')'')') PARAM
                    IF ( .NOT. EZCHEK(PARAM) ) THEN
                      WRITE(6,FMT='('' *** Could not find: '',A)') PARAM
                    ENDIF
                  ENDDO
C
                  WRITE(6,FMT='('' '')')
                  WRITE(6,FMT='('' Dumping parameters...'')')
                  FILENAME = 'EZDUMP_'//RCP_BANK
                  CALL D0OPEN (LUNOUT,FILENAME,'OFL',OK)
                  IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
                  CALL EZDUMP(LUNOUT,0,0)
                  CLOSE(UNIT=LUNOUT)
C
                  FILENAME = 'EZDUMP2_'//RCP_BANK
                  CALL D0OPEN (LUNOUT,FILENAME,'OFL',OK)
                  IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
                  CALL EZDUMP(LUNOUT,0,2)
                  CLOSE(UNIT=LUNOUT)
                  WRITE(6,FMT='('' Done!'')')
C
                  CALL EZRSET
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          ACTIVE = .TRUE.
        ENDIF
      ENDDO
  999 CONTINUE
      END
