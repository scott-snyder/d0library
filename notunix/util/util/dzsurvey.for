      PROGRAM DZSURVEY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Super-simple program to call DZSURV.
C-
C-   Created   1-OCT-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LUN, LUNOUT
      PARAMETER( LUNOUT = 80 )
C
      INTEGER LENGTH, IOS, I, J, K, IEVENT, NEVENT
      INTEGER IEV, RUN, EVONUM, RUNNO
      LOGICAL OK, EVENT, ACTIVE, OUTPUT
      REAL    VALUE
C
      CHARACTER*4   MODE
      CHARACTER*132 RECORD,DATAFILE
      CHARACTER*255 STRING
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      CALL LIB$GET_FOREIGN
     &  (STRING,'Datafile [Number of Events] [Mode] >',LENGTH)
      IF ( LENGTH .LE. 0 ) THEN
        STOP
      ENDIF
      CALL UPCASE(STRING(1:LENGTH),STRING(1:LENGTH))
C
C ****  Decode Command Line
C
      CALL WORD(STRING(1:LENGTH),I,J,K)
      DATAFILE = STRING(I:J)
      STRING   = STRING(J+1:)
      NEVENT   = VALUE(STRING,I,J,K)
      STRING   = STRING(J+1:)
      MODE     = STRING
C
      IF ( NEVENT .LE. 0 ) THEN
        NEVENT = 1
      ENDIF
C
C ****  Setup zebra
C
      CALL D0OPEN(3,'DZTEMP','OF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open scratch file'
      ENDIF
      CALL MZEBRA(0)                    ! Initialize ZEBRA
      CALL INZCOM(2)                    ! Initialize /ZEBCOM/
C
C ************************************************************
C ****  Open input file and declare its existence to ZEBRA
C ************************************************************
C
      CALL EVOPIN(DATAFILE,MODE,LUN,OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open datafile'
      ENDIF
C
C ****  Loop over events
C
      WRITE(6,'(''  Working...'')')
      IEVENT = 0
      DO WHILE ( IEVENT .LT. NEVENT )
C
C ****  Read event
C
        IOS = 0
        CALL EVTIN (LUN,IOS)
C
        EVENT = IOS .EQ. 0                ! Set Event flag
C
        IF ( EVENT ) THEN
          IEVENT = IEVENT + 1
C
          IEV = EVONUM()
          RUN = RUNNO()
          WRITE(STRING,'(2I10)') RUN, IEV
          STRING = 'BankMap for Event '//STRING(1:20)
          CALL DZSURV(STRING(1:40),IXCOM,LQ)
C
C ****  Check for E-of-F
C
        ELSEIF ( IOS .GE. 3 ) THEN
          IEVENT = NEVENT
        ENDIF
      ENDDO
C
C ****  Close datafile
C
      CALL FZENDI (LUN,'TU')
      CLOSE(UNIT=LUN)
      CLOSE(UNIT=3)
C
C ****  Filter file
C
      CALL D0OPEN(LUN,'DZTEMP','MIF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open scratch file'
      ENDIF

      CALL D0OPEN(LUNOUT,'DZSURV','OF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open DZSURV output file'
      ENDIF
C
      ACTIVE = .TRUE.
      OUTPUT = .FALSE.
C
      DO WHILE ( ACTIVE )
        READ(UNIT=LUN,FMT='(A)',ERR=900,END=900) RECORD
        CALL SWORDS(RECORD,I,J,K)
C
        IF ( RECORD(I:I+5) .EQ. 'DZSURV' ) THEN
          OUTPUT = .NOT. OUTPUT
        ENDIF
C
        IF ( OUTPUT ) THEN
          WRITE(LUNOUT,'(A)') RECORD(1:J)
        ENDIF
      ENDDO
C
  900 CONTINUE
      CLOSE(UNIT=LUN,DISP='DELETE')
      CLOSE(UNIT=LUNOUT)
C
  999 CONTINUE
      END
