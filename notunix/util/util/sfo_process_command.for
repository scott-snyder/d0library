      SUBROUTINE SFO_PROCESS_COMMAND(COMMAND,RECORD,LUN,LUNOUT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process !SQL macro commands in source file.
C-
C-              !SQL                    sql-statement
C-              !CONT                   sql-statement
C-              !CONT ALL               variable-name structure-type
C-
C-              !SQLINCLUDE             [Indicator-post-fix]
C-              !SQLCOPY   variable-name1 FROM variable-name2 structure-type
C-
C-   Inputs  : COMMAND  [C*]    Macro command
C-             RECORD   [C*]    Current input record
C-             LUN      [I]     Unit for reading include files
C-             LUNOUT   [I]     Unit number for output
C-   Outputs : IER      [I]      0 -- OK
C-                              -1 -- Invalid macro command
C-                              -2 -- No INCLUDE declaration
C-                              -3 -- Unable to open INCLUDE file
C-                              -4 -- Structure NOT declared
C-   Controls:
C-
C-   Created   7-OCT-1990   Harrison B. Prosper
C-   Updated   9-JUL-1992   Harrison B. Prosper  
C-      Fix processing of includes 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
      CHARACTER*(*) RECORD
      INTEGER LUN
      INTEGER LUNOUT
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$UTIL:CLIST.INC'       ! List processing codes
      INTEGER MAXSTRUCT
      PARAMETER( MAXSTRUCT = 100 )
      INTEGER FIELD_LIST(MAXSTRUCT),NSTRUCT,LFIELD,ISTRUCT
      CHARACTER*32 STRUCT(MAXSTRUCT),IND(MAXSTRUCT),FIELD
C----------------------------------------------------------------------
      INTEGER I,J,K,II,JJ,KK,LL,NN,JCOM,LCOM,STATUS,ID
      INTEGER LCTOKEN(50),NCTOKEN,LIND,LOOP

      LOGICAL ACTIVE,OUTPUT,FOUND_START,FOUND_END,LOG
      CHARACTER*64  CTOKEN(50),NAME,VARIABLE
      CHARACTER*80  STRING,STRING1,FILENAME

      LOGICAL FIRST
      SAVE FIRST,NSTRUCT
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NSTRUCT = 0                     ! Structure count
        CALL TRNLNM('SFO$LOG',STRING,LL)
        LOG = STRING(1:LL) .NE. 'SFO$LOG'
      ENDIF

      IER  = 0                          ! Error code
      JCOM = INDEX(RECORD,'!')          ! Position of command
      LCOM = INDEX(RECORD(JCOM:),' ')-1 ! Length of command

COMMAND !SQLINCLUDE

      IF     ( COMMAND .EQ. '!SQLINCLUDE' )   THEN
C
C ****  Check for include statement
C
        I = INDEX(RECORD,'INCLUDE ')
        IF ( I .LE. 0 ) THEN
          IER = -2                      ! No INCLUDE file statement
          GOTO 999
        ENDIF
C
C ****  Get name of include file
C
        STRING = RECORD(I+7:JCOM-1)
        CALL WORD(STRING,II,JJ,KK)
        FILENAME = STRING(II+1:JJ-1)    ! Name of INCLUDE file
        I = INDEX(FILENAME,'/')         ! Check for /LIST
        IF ( I .GT. 0 ) THEN
          FILENAME = FILENAME(1:I-1)    ! Remove /LIST
        ENDIF
        CALL WORD(FILENAME,II,JJ,KK)
C
        IF ( LOG ) THEN
          WRITE(6,'('' %SFORTRAN-I-ProcDef, '',A)') FILENAME(1:KK)
        ENDIF
C
C ****  Open include file
C
        IER =-3
        OPEN (UNIT=LUN,FILE=FILENAME,STATUS='OLD',READONLY,ERR=999)
        IER = 0
C
C ****  Create list for field names
C
        I = INDEX(FILENAME,':') + 1
        J = INDEX(FILENAME,'.DEF') - 1

        STATUS = LCREATE(STRING_TYPE,FILENAME(I:J))
        IF ( STATUS .LT. 0 ) THEN
          CALL LERROR(STATUS)
        ENDIF
C
C ****  Note the id of the corresponding field list.
C
        NSTRUCT = NSTRUCT + 1
        FIELD_LIST(NSTRUCT) = STATUS
C
C ****  Get indicator 'post-fix'
C
        STRING = RECORD(JCOM+LCOM:)
        CALL WORD(STRING,I,J,LIND)
        IF ( LIND .LE. 0 ) THEN
          IND(NSTRUCT) = '_IND'
          LIND         = 4
        ELSE
          IND(NSTRUCT) = STRING(I:J)
        ENDIF
C
C ****  Loop over include file
C
        FOUND_START= .FALSE.
        FOUND_END  = .FALSE.
        ACTIVE     = .TRUE.
        OUTPUT     = .FALSE.
        NN         = 0                  ! List counter

        DO WHILE ( ACTIVE )
          READ(UNIT=LUN,FMT='(A)',END=200) STRING
C
C ****  Ignore comments
C
          IF ( STRING(1:1) .EQ. ' ' ) THEN
C
C ****  Look for start of STRUCTURE
C
            IF ( .NOT. FOUND_START ) THEN

              I = INDEX(STRING,'STRUCTURE /')
              IF ( I .GT. 0 ) THEN
                FOUND_START = .TRUE.
C
C ****  Note structure name
C
                I = INDEX(STRING,'/')
                NAME = STRING(I+1:)
                I = INDEX(NAME,'/')
                NAME = NAME(1:I-1)
                STRUCT(NSTRUCT) = NAME
C
                IF ( LOG ) THEN
                  WRITE(6,'('' %SFORTRAN-I-StrucName, '',A)') NAME
                ENDIF
                OUTPUT = .TRUE.
C
C ****  Skip next 3 lines
C
                DO I = 1 , 3
                  READ(UNIT=LUN,FMT='(A)',END=200) STRING1
                ENDDO

              ENDIF

            ELSE
C
C ****  Look for first END MAP
C
              OUTPUT = .TRUE.

              IF ( INDEX(STRING,' END MAP ') .GT. 0 ) THEN
                STRING = '      END STRUCTURE'
                CALL SWORDS(STRING,II,JJ,KK)
                FOUND_END = .TRUE.
C
C ****  Skip next 7 lines
C
                DO I = 1 , 7
                  READ(UNIT=LUN,FMT='(A)',END=200) STRING1
                ENDDO

              ELSE
C
C ****  If end of structure found write out the remaining lines
C
                IF ( FOUND_END ) THEN
                  OUTPUT = .TRUE.
                ELSE
C
C ****  Note name of each field for later use if this is a
C ****  structure type
C
                  NN = NN + 1           ! Increment list count
                  CALL CHOP(STRING,CTOKEN,LCTOKEN,NCTOKEN)
                  STATUS = LWRITE(FIELD_LIST(NSTRUCT),
     &                            CTOKEN(2)(1:LCTOKEN(2)))
                  IF ( STATUS .LT. 0 ) THEN
                    CALL LERROR(STATUS)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE
            OUTPUT = .FALSE.
          ENDIF

          IF ( OUTPUT ) THEN
            CALL SWORDS(STRING,II,JJ,KK)
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)
          ENDIF
        ENDDO

  200   CONTINUE
        CLOSE(UNIT=LUN)
C
C ****  Write indicator declarations
C
        IF ( FOUND_START ) THEN

          ID = FIELD_LIST(NSTRUCT)
          STATUS = LRESET(ID) ! Point to first element

          DO LOOP = 1, NN
C
C ****  Read next element in field list and
C ****  write individual indicator declarations
C
            STATUS = LREAD(ID,FIELD,LFIELD)

            STRING = '      INTEGER*2 '//FIELD(1:LFIELD)//IND(NSTRUCT)
            CALL SWORDS(STRING,II,JJ,KK)
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)

            JJ = LOOP
            CALL VNUMI(1,JJ,'(',' ',')',STRING,LL)
            STRING = '      EQUIVALENCE(INDICATOR'//STRING(1:LL)//','//
     &          FIELD(1:LFIELD)//IND(NSTRUCT)(1:LIND)//')'
            CALL SWORDS(STRING,II,JJ,KK)
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)
          ENDDO
        ENDIF

COMMAND !SQLCOPY

      ELSEIF ( COMMAND .EQ. '!SQLCOPY' )  THEN
C
C ****  CTOKEN(1) = Name of variable
C ****  CTOKEN(2) = FROM
C ****  CTOKEN(3) = Name of variable
C ****  CTOKEN(4) = Structure type
C
        CALL SFO_PARSE_COMMAND(4,STRUCT,NSTRUCT,
     &                         RECORD,CTOKEN,LCTOKEN,NCTOKEN,ISTRUCT)
        IF ( ISTRUCT .LE. 0 ) THEN
          IER = -4
          GOTO 999                      ! Structure not declared
        ENDIF
        ID = FIELD_LIST(ISTRUCT)
        STATUS = LRESET(ID) ! Point to first element

        ACTIVE = .TRUE.
        DO WHILE ( ACTIVE )
C
C ****  Read next element in field list
C
          STATUS = LREAD(ID,FIELD,LFIELD)
          ACTIVE = STATUS .GT. 0

          IF ( ACTIVE ) THEN
            STRING = RECORD(1:JCOM-1)//
     &               CTOKEN(1)(1:LCTOKEN(1))//'.'//
     &               FIELD(1:LFIELD)//' = '

            IF ( JCOM .GT. 7 ) THEN
              STRING1 = RECORD(1:5)//'&'//RECORD(7:JCOM-1)//'   '//
     &                  CTOKEN(3)(1:LCTOKEN(3))//'.'//
     &                  FIELD(1:LFIELD)
            ELSE
              STRING1 = RECORD(1:5)//'&'//'   '//
     &                  CTOKEN(3)(1:LCTOKEN(3))//'.'//
     &                  FIELD(1:LFIELD)
            ENDIF

            CALL SWORDS(STRING,II,JJ,KK)
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)
            CALL SWORDS(STRING1,II,JJ,KK)
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING1(1:JJ)
          ENDIF
        ENDDO

COMMAND !SQL

      ELSEIF ( COMMAND .EQ. '!SQL' )      THEN

        STRING = RECORD(1:JCOM-1)//'EXEC '//RECORD(JCOM+1:)
        CALL SWORDS(STRING,II,JJ,KK)
        WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)

COMMAND !CONT

      ELSEIF ( COMMAND .EQ. '!CONT' )     THEN
C
C ****  Check for ALL command used with the SET command
C
        I = INDEX(RECORD,'ALL ')

        IF ( I .GT. 0 ) THEN
C
C ****  CTOKEN(1) = ALL
C ****  CTOKEN(2) = Name of variable
C ****  CTOKEN(3) = Structure type
C
          CALL SFO_PARSE_COMMAND(3,STRUCT,NSTRUCT,
     &                          RECORD,CTOKEN,LCTOKEN,NCTOKEN,ISTRUCT)
          IF ( ISTRUCT .LE. 0 ) THEN
            IER = -3
            GOTO 999                      ! Structure not declared
          ENDIF
          ID = FIELD_LIST(ISTRUCT)
          STATUS = LRESET(ID) ! Point to first element
C
          CALL WORD(IND(ISTRUCT),I,J,LIND)
          VARIABLE = CTOKEN(2)
          LL       = LCTOKEN(2)
          NAME     = CTOKEN(3)

          STRING  = ' '
          STRING1 = ' '
C
C ****  Loop over field names
C
          ACTIVE = .TRUE.
          DO WHILE ( ACTIVE )
C
C ****  Read next element in field list
C
            STATUS = LREAD(ID,FIELD,LFIELD)
            ACTIVE = STATUS .GT. 0
            IF ( ACTIVE ) THEN
C
C ****  Write out previously created strings
C
              CALL SWORDS(STRING,II,JJ,KK)
              IF ( KK .GT. 0 ) THEN
                WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)
              ENDIF
              CALL SWORDS(STRING1,II,JJ,KK)
              IF ( KK .GT. 0 ) THEN
                WRITE(UNIT=LUNOUT,FMT='(A)') STRING1(1:JJ)
              ENDIF
C
C ****  Define next strings to be written out
C
              IF ( JCOM .GT. 7 ) THEN
                STRING = RECORD(1:5)//'&'//RECORD(7:JCOM-1)//
     &            FIELD(1:LFIELD)//' = '
                STRING1= RECORD(1:5)//'&'//RECORD(7:JCOM-1)//'   '//
     &            ':'//VARIABLE(1:LL)//'.'//FIELD(1:LFIELD)//
     &            ':'//FIELD(1:LFIELD)//IND(ISTRUCT)(1:LIND)//','
              ELSE
                STRING = RECORD(1:5)//'&'//
     &            FIELD(1:LFIELD)//' = '
                STRING1= RECORD(1:5)//'&'//'   '//
     &            ':'//VARIABLE(1:LL)//'.'//FIELD(1:LFIELD)//
     &            ':'//FIELD(1:LFIELD)//IND(ISTRUCT)(1:LIND)//','
              ENDIF
            ENDIF
          ENDDO
C
C ****  Write out last pair of strings (remove comma)
C
          CALL SWORDS(STRING,II,JJ,KK)
          IF ( KK .GT. 0 ) THEN
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)
          ENDIF
          CALL SWORDS(STRING1,II,JJ,KK)
          IF ( KK .GT. 0 ) THEN
            WRITE(UNIT=LUNOUT,FMT='(A)') STRING1(1:JJ-1)    ! Remove comma
          ENDIF

        ELSE
C
C ****  This is a regular continuation
C
          IF ( JCOM .GT. 7 ) THEN
            STRING = RECORD(1:5)//'&'//RECORD(7:JCOM-1)//
     &        '    '//RECORD(JCOM+5:)
          ELSE
            STRING = RECORD(1:5)//'&'//
     &        '    '//RECORD(JCOM+5:)
          ENDIF
          CALL SWORDS(STRING,II,JJ,KK)
          WRITE(UNIT=LUNOUT,FMT='(A)') STRING(1:JJ)
        ENDIF
      ELSE

        IER = -1                        ! Invalid macro

      ENDIF

  999 RETURN
      END
