      PROGRAM EZEXAMPLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simple RCP cxample.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  7-DEC-1989   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper
C-   Updated  12-JUN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
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
      INTEGER IVAL(MAXNUM),TYPES(MAXNUM)
      INTEGER IER,TOTAL,LENGTH,NBUF
      INTEGER NUMREC,NUMVAL
      INTEGER NSTR,I,J,K,L
C
      LOGICAL EZERROR
C
      CHARACTER*32 NAME
      CHARACTER*80 STRING,BUFFER(MAXBUF),STR(10)
C----------------------------------------------------------------------
C
      CALL MZEBRA(0)                    ! (1)  Initialize ZEBRA
C
      CALL INZSTP                       ! (2)  Setup /ZEBSTP/
C
C ****  Read RCP file
C
      CALL INRCP ('EZEXAMPLE_RCP',IER)  ! (3)  Read RCP file
      IF ( IER .NE. 0 ) GOTO 999
C
C ****  Open dump file
C
      OPEN (UNIT=LUNOUT,FILE='DUMP',STATUS='NEW'
C&IF VAXVMS,ULTRIX,SIUNIX
     &          ,CARRIAGECONTROL='LIST'
C&ENDIF
     &  )
C
C ****  Pick RCP bank: EZEXAMPLE_RCP
C
      CALL EZPICK ('EZEXAMPLE_RCP')        ! (4) Select RCP bank
C
C ****  Get values and types from parameter MIXED_ARRAY
C
C                                          ! (5) Get both values and types
      CALL EZGET_VALUE_TYPE ('MIXED_ARRAY',IVAL,TYPES,TOTAL,IER)
C
C
      WRITE(UNIT=LUNOUT,FMT=
     &  '('' DUMP from EZGET_VALUE_TYPE of MIXED_ARRAY'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' '',6x,''IVAL'',5x,''TYPE'')')
      WRITE(UNIT=LUNOUT,FMT='('' '',I10,5x,I4)')
     &  (IVAL(I),TYPES(I),I=1,TOTAL)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Set 4th string in MIXED_ARRAY
C
      CALL EZSETS ('MIXED_ARRAY',4,'Chopped',10,IER)    ! (6)
C
C ****  Get 3rd string in MIXED_ARRAY
C
      CALL EZGETS ('MIXED_ARRAY',3,STRING,LENGTH,IER)   ! (7)
C
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZGETS:1'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='(''  '',A)') '<'//STRING(1:LENGTH)//'>'
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Get parameter LONG_STRING
C
      CALL EZGETS ('LONG_STRING',1,STRING,LENGTH,IER)   ! (8)
C
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZGETS:2'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='(''  '',A)') '<'//STRING(1:LENGTH)//'>'
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Get first NSTR contiguous strings from array MIXED_ARRAY
C
      CALL EZ_GET_CHARS ('MIXED_ARRAY',NSTR,STR,IER)    ! (9)
C
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
C ****  Add some parameters to the existing SRCP bank
C
      STR(1) = 'ADDED_VAR_ONE ''Hello there'' ! Added from PROGRAM'
      STR(2) = '\ARRAY ADDED_VAR_TWO ! Added from PROGRAM'
      STR(3) = ' 1 2 3 4 5 ''Thou lump of foul deformity'''
      STR(4) = '\END'
      NSTR = 4
      CALL EZADD (STR,NSTR,IER)         ! (10)
      CALL EZEND                        ! (11)
C
C ****  Get values and types from parameter DUMP_BANKS
C
      CALL EZGET_VALUE_TYPE ('DUMP_BANKS',IVAL,TYPES,TOTAL,IER) ! (12)
C
C
      WRITE(UNIT=LUNOUT,FMT=
     &  '('' DUMP from EZGET_VALUE_TYPE of DUMP_BANKS'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' '',6x,''IVAL'',5x,''TYPE'')')
      WRITE(UNIT=LUNOUT,FMT='('' '',I10,5x,I4)')
     &  (IVAL(I),TYPES(I),I=1,TOTAL)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Dump RCP bank
C
      WRITE(UNIT=LUNOUT,FMT='('' EZDUMP of EZEXAMPLE '')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      CALL EZDUMP (LUNOUT,0,0)          ! (13)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
      CALL EZRSET                       ! (14)
C
C ****  Book an NEW empty SRCP bank called MY_NEW_BANK with:
C ****  1) room for 20*4 character long records
C ****  2) room for 30 such records and
C ****  3) room for 40 values
C
      CALL EZBOOK ('MY_NEW_BANK',20,30,40)
      IF ( EZERROR(IER) ) THEN
        WRITE(UNIT=LUNOUT,FMT='('' Problem booking SRCP bank'')')
      ELSE
C
C ****  Add parameters to the new SRCP bank
C
        STR(1) = 'ADDED_THIS 12.345E-5  ! Added from PROGRAM'
        STR(2) = ' '
        STR(3) = '! Twas brillig and the slithy tothes...'
        STR(4) = '\ARRAY AND_ADDED_THAT ! Added from program'
        STR(5) = ' ''Let all the evil that lurks in the mud'''
        STR(6) = ' 1 TRUE 2.00 ''hatch out..'''
        STR(7) = '\END'
        STR(8) = ' '
        NSTR   = 8
        CALL EZADD (STR,NSTR,IER)
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
        CALL EZFETCH('AND_ADDED_THAT',MAXBUF,NBUF,BUFFER,IER)
        IF ( NBUF .GT. 0 ) THEN
          DO I =  1, NBUF
            CALL SWORDS(BUFFER(I),J,L,K)
            CALL EZUDMP(LUNOUT,BUFFER(I)(1:L))
          ENDDO
        ENDIF
        WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
      ENDIF
C
C ****  Dump names and addresses of SRCP banks in memory
C
      CALL EZDBUG (LUNOUT)
C
C ****  Close DUMP file
C
      CLOSE(UNIT=LUNOUT)

  999 CONTINUE
      END
