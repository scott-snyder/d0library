      SUBROUTINE ACP_SUCCESS(VSN,NEVENT,NBLOCKS)
      CHARACTER*(*) VSN
      CHARACTER*20 STATFILE
      CHARACTER*5 HOST
      CHARACTER*60 COMMENT
      INTEGER NEVENT,NBLOCKS,STAT,HLEN
      LOGICAL FIRST

      CHARACTER*26 CHT
      CHARACTER*3 MON
      CHARACTER*2 DAY
      CHARACTER*4 YEAR
      CHARACTER*8 TM
      CHARACTER*20 DATE
      INTEGER  TIME,STIME
      DATA HLEN /5/

      STIME = TIME()
      CALL CCTIME(STIME,CHT)
      DAY = CHT(9:10)
      MON = CHT(5:7)
      YEAR = CHT(21:24)
      TM = CHT(12:19)
      DATE = DAY//'-'//MON//'-'//YEAR//' '//TM
      STATFILE = VSN//'.success'

      CALL ACP_SIGHOLD
      OPEN(77,FILE=STATFILE)
      STAT = CGETHOSTNAME(HOST,%VAL(HLEN))
      CALL GETENV('COMMENT',COMMENT)
      WRITE(77,101) VSN,HOST,NEVENT,NBLOCKS,DATE
      WRITE(77,102) COMMENT
      CLOSE(77)
      CALL ACP_SIGRELEASE
      RETURN

101   FORMAT(1X,A,3X,A,2I8,3X,A)
102   FORMAT(1x,'COMMENT   ',A)
      END
