      PROGRAM EZBENCHMARK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Bench-mark program for RCP.
C-
C-   Created  19-DEC-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LUN
      PARAMETER( LUN    = 10 )
C
      INTEGER LUNOUT
      PARAMETER( LUNOUT = 20 )
C
      INTEGER MAXNUM
      PARAMETER( MAXNUM = 100 )
C
      INTEGER MAXBUF
      PARAMETER( MAXBUF = 100 )
C
      REAL    RVAL(MAXNUM)
      INTEGER IVAL(MAXNUM),TYPES(MAXNUM),ITYPE,SIZE,ID
      INTEGER STATUS,TOTAL,LENGTH,NBUF
      INTEGER NUMREC,NUMVAL
      INTEGER NSTR,I,J,K,L,N,NPAR
C
      LOGICAL EZERROR,OK,EZCHECK,FOUND
C
      CHARACTER*32 NAME,PAR(MAXBUF)
      CHARACTER*132 STRING,BUFFER(MAXBUF),STR(10)
C----------------------------------------------------------------------
C
C ****  INITIALIZE ZEBRA
C
      CALL MZEBRA(0)
C
C ****  INITIALIZE /ZEBSTP/
C
      CALL INZSTP
C
C ****  READ RCP FILE
C
      CALL INRCP ('EZBENCHMARK_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Unable to open file EZBENCHMARK_RCP'
      ENDIF
C
C ****  Open dump file
C
      CALL D0OPEN(LUNOUT,'EZBENCHMARK_DUMP','OF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open file EZBENCHMARK_DUMP'
      ENDIF
C
C ****  Pick RCP bank
C
      CALL EZPICK ('EZBENCHMARK_RCP')
      IF ( EZERROR(STATUS) ) THEN
        STOP 'Unable to pick bank EZBENCHMARK_RCP'
      ENDIF
C
C ****  Get 3rd string in MIXED_ARRAY
C
      CALL EZGETS ('MIXED_ARRAY',3,STRING,LENGTH,STATUS)   ! (6)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot GET 3rd string of MIXED_ARRAY'
      ENDIF
C
C ****  Set 4th string in MIXED_ARRAY use first 4-chars of 3rd string
C
      CALL EZSETS ('MIXED_ARRAY',4,STRING,4,STATUS)    ! (7)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot SET 4th string of MIXED_ARRAY'
      ENDIF
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP of MIXED_ARRAY 3rd string'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='(''  '',A)') '<'//STRING(1:LENGTH)//'>'
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Get parameter LONG_STRING
C
      CALL EZGETS ('LONG_STRING',1,STRING,LENGTH,STATUS)   ! (8)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot find parameter LONG_STRING'
      ENDIF
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP of LONG_STRING'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='(''  '',A)') '<'//STRING(1:LENGTH)//'>'
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Get first NSTR contiguous strings from array MIXED_ARRAY
C
      CALL EZ_GET_CHARS ('MIXED_ARRAY',NSTR,STR,STATUS)    ! (9)
C
      WRITE(UNIT=LUNOUT,FMT=
     &  '('' DUMP from EZ_GET_CHARS: MIXED_ARRAY'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      DO I =  1,NSTR
        CALL SWORDS (STR(I),J,L,K)
        WRITE(UNIT=LUNOUT,FMT='('' '',A)') STR(I)(1:L)
      ENDDO
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Add some parameters to the existing RCP bank
C
      STR(1) = 'ADDED_VAR_ONE ''Hello there'' ! Added from PROGRAM'
      STR(2) = '\ARRAY ADDED_VAR_TWO ! Added from PROGRAM'
      STR(3) = ' 1 2 3 4 5 ''Thou lump of foul deformity'''
      STR(4) = '\END'
      NSTR = 4
      CALL EZADD (STR,NSTR,STATUS)         ! (10)
      CALL EZEND                        ! (11)
C
C ****  Book an NEW empty RCP bank called MY_NEW_BANK with:
C ****  1) room for 20*4 character long records
C ****  2) room for 30 such records and
C ****  3) room for 40 values
C
      CALL EZBOOK ('MY_NEW_BANK',20,30,40)
      IF ( EZERROR(STATUS) ) THEN
        WRITE(UNIT=LUNOUT,FMT='('' Problem booking bank MY_NEW_BANK'')')
      ELSE
C
C ****  Add parameters to the new SRCP bank
C
        CALL EZPICK('MY_NEW_BANK')
        STR(1) = 'ADDED_THIS 12.345E-5  ! Added from PROGRAM'
        STR(2) = ' '
        STR(3) = '! Twas brillig and the slithy tothes...'
        STR(4) = '\ARRAY AND_ADDED_THAT ! Added from program'
        STR(5) = ' ''Let all the evil that lurks in the mud'''
        STR(6) = ' 1 TRUE 2.00 ''hatch out..'''
        STR(7) = '\END'
        STR(8) = ' '
        NSTR   = 8
        CALL EZADD (STR,NSTR,STATUS)
        CALL EZEND               ! Complete construction of bank
C
        WRITE(UNIT=LUNOUT,FMT='('' EZDUMP of bank MY_NEW_BANK'')')
        WRITE(UNIT=LUNOUT,FMT='('' '')')
        CALL EZDUMP(LUNOUT,0,0)
        WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Fetch parameter
C
        WRITE(UNIT=LUNOUT,FMT='('' EZFETCH of AND_ADDED_THAT'')')
        WRITE(UNIT=LUNOUT,FMT='('' '')')
C
        CALL EZFETCH('AND_ADDED_THAT',MAXBUF,NBUF,BUFFER,STATUS)
        IF ( NBUF .GT. 0 ) THEN
          DO I =  1, NBUF
            CALL SWORDS(BUFFER(I),J,L,K)
            CALL EZUDMP(LUNOUT,BUFFER(I)(1:L))
          ENDDO
        ENDIF
        WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
        CALL EZRSET
      ENDIF
C
C ****  Dump RCP bank
C
      CALL D0OPEN(LUN,'EZBENCHMARK','OF',OK)
      CALL EZDUMP(LUN,0,0)
      CLOSE(UNIT=LUN)
C
C ****  Compute size
C
      CALL D0OPEN(LUN,'EZBENCHMARK','IF',OK)
      CALL EZSIZE (LUN,NUMVAL,NUMREC)
      CLOSE(UNIT=LUN)
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZSIZE'')')
      WRITE(UNIT=LUNOUT,FMT='('' Values: '',I10,5x,''Records: '',I10)')
     &  NUMVAL,NUMREC
      WRITE(UNIT=LUNOUT,FMT='('' '')')
C
C ****  Decode file
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZDECODE'')')
      CALL D0OPEN(LUN,'EZBENCHMARK','IF',OK)
      STATUS = 0
      DO WHILE ( STATUS .EQ. 0 )
        CALL EZDECODE (LUN,NAME,RVAL,TYPES,TOTAL,STATUS)
        IF ( STATUS .EQ. 0 ) THEN
          CALL EZZDMP(LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
        ENDIF
      ENDDO
      CLOSE(UNIT=LUN)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZPAR'')')
      CALL D0OPEN(LUN,'EZBENCHMARK','IF',OK)
      CALL EZPAR(LUN,'ALL',PAR,NPAR)
      CLOSE(UNIT=LUN)
C
      NPAR = NPAR + 1
      PAR(NPAR) = 'NULL_ARRAY'
C
      DO I =  1,NPAR
        FOUND = EZCHECK(PAR(I))
C
        IF ( FOUND ) THEN
C
          CALL EZGETA(PAR(I),0,0,0,N,STATUS)       ! Get parameter size
          IF ( STATUS .NE. 0 ) THEN
            WRITE(6,220) I,PAR(I),STATUS
            WRITE(LUNOUT,220) I,PAR(I),STATUS
          ENDIF
          CALL EZGETI(PAR(I),ID,STATUS)            ! Get parameter index
          CALL EZGETT(ID,NAME,L,ITYPE,SIZE)       ! Get parameter type
C
          WRITE(6,200) I,PAR(I),N,SIZE,ITYPE
          WRITE(LUNOUT,200) I,PAR(I),N,SIZE,ITYPE
C
        ELSE
          WRITE(6,210) I,PAR(I)
          WRITE(LUNOUT,210) I,PAR(I)
        ENDIF
      ENDDO
C
      CALL EZRSET
      CLOSE(UNIT=LUNOUT)
C
  200 FORMAT(1X,I5,5X,A32,3I5,'  FOUND')
  210 FORMAT(1X,I5,5X,A32,' ******* NOT FOUND')
  220 FORMAT(1X,I5,5X,A32,' STATUS: ',I5)
      END
