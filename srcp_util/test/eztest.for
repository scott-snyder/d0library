C DEC/CMS REPLACEMENT HISTORY, Element EZTEST.FOR
C *1    11-DEC-1989 22:40:40 HARRISON "Harrison B. Prosper: Test routine"
C DEC/CMS REPLACEMENT HISTORY, Element EZTEST.FOR
      PROGRAM EZTEST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test new EZBOOK, VALUSY, EZGNXT and EZDECODE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-SEP-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LUN
      PARAMETER( LUN    = 10 )
      INTEGER LUNOUT
      PARAMETER( LUNOUT = 20 )
      INTEGER MAXNUM
      PARAMETER( MAXNUM = 100 )
C
      INTEGER ID,NID,LENREC,IER,TOTAL,TYPES(MAXNUM),LENGTH
      INTEGER NUMREC,NUMVAL
C
      LOGICAL LVAL(MAXNUM)
      INTEGER IVAL(MAXNUM)
      REAL    RVAL(MAXNUM)
      CHARACTER*4 CVAL(MAXNUM),TYPE(MAXNUM)
      EQUIVALENCE (LVAL(1),IVAL(1),RVAL(1),CVAL(1))
C
      CHARACTER*32 NAME
      CHARACTER*80 RECORD,STRING,REC(10)
      INTEGER NREC,I,J,K,L
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL INZBRA
      CALL INZSTP
C
C ****  Read RCP file
C
      CALL INRCP ('EZTEST_RCP',IER)
C
C ****  Open dump file
C
      OPEN (UNIT=LUNOUT,FILE='EZTEST_DUMP',STATUS='NEW'
C&IF VAXVMS,ULTRIX,SIUNIX
     &          ,CARRIAGECONTROL='LIST'
C&ENDIF
     &  )
C
      CALL EZPICK ('EZTEST_RCP')
C
C ****  Compute size
C
      OPEN (UNIT=LUN,FILE='EZTEST_RCP',STATUS='OLD')
      CALL EZSIZE (LUN,NUMVAL,NUMREC)
      CLOSE(UNIT=LUN)
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZSIZE'')')
      WRITE(UNIT=LUNOUT,FMT='('' Values: '',I10,5x,''Records: '',I10)') 
     &  NUMVAL,NUMREC
      WRITE(UNIT=LUNOUT,FMT='('' '')')

      WRITE(UNIT=LUNOUT,FMT='('' DUMP'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')

      CALL EZDUMP (LUNOUT,0,0)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Test EZINIT
C
C      CALL EZINIT

      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZDECODE'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
C
C ****  Decode file
C
      OPEN (UNIT=LUN,FILE='EZTEST_RCP',STATUS='OLD')
      IER = 0
      DO WHILE ( IER .EQ. 0 )
        CALL EZDECODE (LUN,NAME,RVAL,TYPES,TOTAL,IER)
        IF ( IER .EQ. 0 ) THEN
          CALL EZZDMP   (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
        ENDIF
      ENDDO
      CLOSE(UNIT=LUN)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
      CALL EZGET_VALUE_TYPE ('MIXED_ARRAY',IVAL,TYPES,TOTAL,IER)
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZGET_VALUE_TYPE'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' '',6x,''IVAL'',5x,''TYPE'')')
      WRITE(UNIT=LUNOUT,FMT='('' '',I10,5x,I4)')
     &  (IVAL(I),TYPES(I),I=1,TOTAL)
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
      CALL EZSETS ('MIXED_ARRAY',1,'Chopped',10,IER)
      CALL EZSETS ('MIXED_ARRAY',3,'Suey',10,IER)
C
      CALL EZ_GET_CHARS ('MIXED_ARRAY',NREC,REC,IER)
      WRITE(UNIT=LUNOUT,FMT=
     &  '('' DUMP from EZ_GET_CHARS: MIXED_ARRAY'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      DO I =  1,NREC
        CALL SWORDS (REC(I),J,L,K)
        WRITE(UNIT=LUNOUT,FMT='('' '',A)') REC(I)(1:L)
      ENDDO
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
      CALL EZ_GET_CHARS ('STRING_ARRAY',NREC,REC,IER)
      WRITE(UNIT=LUNOUT,FMT=
     &  '('' DUMP from EZ_GET_CHARS: STRING_ARRAY'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      DO I =  1,NREC
        CALL SWORDS (REC(I),J,L,K)
        WRITE(UNIT=LUNOUT,FMT='('' '',A)') REC(I)(1:L)
      ENDDO
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')
C
C ****  Test EZADD
C
      REC(1) = 'ADD_VAR_ONE ''Hello there'' ! Added from EZTEST'
      REC(2) = '\ARRAY ADD_VAR_TWO ! Added from EZTEST'
      REC(3) = ' 1 2 3 4 5 ''Thou lump of foul deformity'''
      REC(4) = '\END'
      NREC = 4
      CALL EZADD (REC,NREC,IER)
      CALL EZEND
C
C ****  Test of EZFILL
C
      RVAL(1) = 11.11
      IVAL(2) = 2222
      RVAL(3) = 0.0333
      CVAL(4) = 'Rich'
      CVAL(5) = 'ard '
      CVAL(6) = 'III '
      CVAL(7) = 'Foul'
      CVAL(8) = 'Lump'
      IVAL(9) = 3333
      IVAL(10)= 4444
C
      TYPE(1) = 'R'
      TYPE(2) = 'I'
      TYPE(3) = 'R'
      TYPE(4) = 'C'
      TYPE(5) = 'C'
      TYPE(6) = '&'
      TYPE(7) = '&'
      TYPE(8) = '&'
      TYPE(9) = 'I'
      TYPE(10)= 'I'
      TOTAL   = 10
      CALL EZFILL ('THOU_LUMP','Of foul deformity',RVAL,TYPE,TOTAL)
      
      LVAL(1) = .TRUE.
      CVAL(2) = 'VAX '
      TYPE(1) = 'L'
      TYPE(2) = 'C'
      TOTAL   = 2
      CALL EZFILL ('VAX_STATIONS','Are great',RVAL,TYPE,TOTAL)
      CALL EZEND
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP of added parameters'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      CALL EZGETI ('ADD_VAR_TWO',ID,IER)
      CALL EZGETN (ID,NAME,L)
      CALL EZGETD (ID,RVAL,TYPES,TOTAL,RECORD,LENREC)
      CALL EZZDMP (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')

      CALL EZGETI ('THOU_LUMP',ID,IER)
      CALL EZGETN (ID,NAME,L)
      CALL EZGETD (ID,RVAL,TYPES,TOTAL,RECORD,LENREC)
      CALL EZZDMP (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')

      CALL EZGETI ('VAX_STATIONS',ID,IER)
      CALL EZGETN (ID,NAME,L)
      CALL EZGETD (ID,RVAL,TYPES,TOTAL,RECORD,LENREC)
      CALL EZZDMP (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')

      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZGETS'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      CALL EZGETS ('MIXED_ARRAY',3,STRING,LENGTH,IER)
      WRITE(UNIT=LUNOUT,FMT='(''  '',A)') '<'//STRING(1:LENGTH)//'>'
      WRITE(UNIT=LUNOUT,FMT='('' '')')
      WRITE(UNIT=LUNOUT,FMT='('' --------------------------'')')

      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZGNXT'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
C
      NID = 1
      ID = 1
      DO WHILE ( ID .GT. 0 )
        CALL EZGNXT ('HISTO',NID,ID)
        IF ( ID .GT. 0 ) THEN
          CALL EZGETN (ID,NAME,L)
          CALL EZGETD (ID,RVAL,TYPES,TOTAL,RECORD,LENREC)
          CALL EZZDMP (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
        ENDIF
      ENDDO
C
      WRITE(UNIT=LUNOUT,FMT='('' DUMP from EZGNXT 2'')')
      WRITE(UNIT=LUNOUT,FMT='('' '')')
C
      NID = 3
      CALL EZGNXT ('HISTO',NID,ID)
      IF ( ID .GT. 0 ) THEN
        CALL EZGETN (ID,NAME,L)
        CALL EZGETD (ID,RVAL,TYPES,TOTAL,RECORD,LENREC)
        CALL EZZDMP (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
      ENDIF

      NID = 2
      CALL EZGNXT ('HISTO',NID,ID)
      IF ( ID .GT. 0 ) THEN
        CALL EZGETN (ID,NAME,L)
        CALL EZGETD (ID,RVAL,TYPES,TOTAL,RECORD,LENREC)
        CALL EZZDMP (LUNOUT,NAME,' ',RVAL,TYPES,TOTAL)
      ENDIF
C
      CLOSE(UNIT=LUNOUT)

      END
