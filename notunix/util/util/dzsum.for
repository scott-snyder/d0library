      PROGRAM DZSUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Program to summarise the output of DZSURV.
C-
C-   Created  14-APR-1993   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER LUN, LUNOUT, NMAX
      PARAMETER( LUN    = 20 )
      PARAMETER( LUNOUT = 80 )
      PARAMETER( NMAX   = 1000)
C----------------------------------------------------------------------
      INTEGER LENGTH, I, J, K, NEVENT, SUM_WORDS, SUM_BANKS, II
      INTEGER WORDS(NMAX), NLIST, IDLIST(NMAX), IMAP(NMAX)
      INTEGER COUNT1(NMAX),COUNT2(NMAX)
      INTEGER BANKS(NMAX)
      INTEGER LSTR(50), NSTR, NW, NBK,NWBK
      REAL    VALUEX, AVE_WORDS, AVE_BANKS, BLOCKS
      LOGICAL ACTIVE, OK, NEW
C----------------------------------------------------------------------
      CHARACTER*4   LIST(NMAX), BANK
      CHARACTER*16  STR(50)
      CHARACTER*132 RECORD,DZSURVFILE
      CHARACTER*255 STRING
C----------------------------------------------------------------------
      SAVE COUNT1, COUNT2, LIST, IDLIST, NLIST
      DATA NLIST  /0/
      DATA COUNT1 /NMAX*0/
      DATA COUNT2 /NMAX*0/
C----------------------------------------------------------------------
C
      CALL LIB$GET_FOREIGN(STRING,'DzsurvFile >',LENGTH)
      IF ( LENGTH .LE. 0 ) THEN
        STOP
      ENDIF
      CALL UPCASE(STRING(1:LENGTH),STRING(1:LENGTH))
C
C ****  Decode Command Line
C
      CALL WORD(STRING(1:LENGTH),I,J,K)
      DZSURVFILE = STRING(I:J)
C
C ****  Open Input file
C
      CALL D0OPEN(LUN,DZSURVFILE,'IF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open DZSURV file'
      ENDIF
C
C ****  Loop over events in file
C
      ACTIVE = .TRUE.
      NEVENT = 0
C
      DO WHILE ( ACTIVE )
        READ(UNIT=LUN,FMT='(A)',ERR=900,END=900) RECORD
C
C ****  Decode line (NWCUM NW NWBK NBK [link] NAME)
C ****  Extract Words word count, NW per set of banks
C ****  and name of bank
C
        CALL CHOP(RECORD,STR,LSTR,NSTR)
C
C ****  Skip blank lines and page markers
C
        IF ( NSTR .GT. 0 ) THEN
C
          IF ( STR(3)(1:LSTR(3)) .NE. 'PAGE' ) THEN
C
C ****  Check for start of event
C
            IF ( STR(1)(1:LSTR(1)) .EQ. 'DZSURV' ) THEN
              CALL SWORDS(RECORD,I,J,K)
              WRITE(6,'(1X,2A)') STR(6), STR(7)
              NEVENT = NEVENT + 1
            ENDIF
C
            IF ( NSTR .GE. 5 ) THEN
              II = VALUEX(STR(1)(1:LSTR(1)),I,J,K)
              IF ( K .EQ. VTINT ) THEN
C
C ****  NW  : Sum of words for bank
C
                NW = VALUEX(STR(2)(1:LSTR(2)),I,J,K)
                IF ( K .EQ. VTINT ) THEN
C
                  NWBK = VALUEX(STR(3)(1:LSTR(3)),I,J,K)
                  IF ( K .EQ. VTINT ) THEN
C
C ****  NBK : Number of banks
C
                    NBK = VALUEX(STR(4)(1:LSTR(4)),I,J,K)
C
C ****  BANK: Bank Name
C
                    IF ( NSTR .EQ. 5 ) THEN
                      BANK = STR(5)(1:LSTR(5))
                    ELSE
                      BANK = STR(6)(1:LSTR(6))
                    ENDIF
C
C ****  Keep totals
C
                    CALL LOCSTR1(BANK,LIST,IDLIST,NLIST,NEW,J)
                    COUNT1(IDLIST(J)) = COUNT1(IDLIST(J)) + NW
                    COUNT2(IDLIST(J)) = COUNT2(IDLIST(J)) + NBK
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
  900 CONTINUE
      CLOSE(UNIT=LUN)
C
C ****  Sort banks according to counts
C
      DO I = 1, NLIST
        WORDS(I) = COUNT1(IDLIST(I))
        BANKS(I) = COUNT2(IDLIST(I))
        IMAP(I)  = I
      ENDDO
      CALL SRTINT(WORDS,NLIST,IMAP)
C
C ****  Write out summary
C
      CALL D0OPEN(LUN,'DZSUM','OF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Unable to open DZSUM output file'
      ENDIF
      WRITE(LUN,'(1X,10X,''     WORDS'',''     BANKS'',
     &                   ''   <WORDS>'',''   <BANKS>'',
     &                   ''      BANK'')')
      WRITE(LUN,'(1X,15(''----''))')
C
      SUM_WORDS = 0
      SUM_BANKS = 0
C
      DO I = 1, NLIST
        J = NLIST + 1 - I
        SUM_WORDS = SUM_WORDS + WORDS(J)
        SUM_BANKS = SUM_BANKS + BANKS(IMAP(J))
        AVE_WORDS = FLOAT(WORDS(J))/NEVENT
        AVE_BANKS = FLOAT(BANKS(IMAP(J)))/NEVENT
C
        WRITE(LUN,'(1X,I10,2I10,2F10.2,6X,A4)')
     &    I, WORDS(J), BANKS(IMAP(J)), AVE_WORDS, AVE_BANKS, 
     &    LIST(IMAP(J))
      ENDDO
C
      AVE_WORDS = FLOAT(SUM_WORDS)/NEVENT
      AVE_BANKS = FLOAT(SUM_BANKS)/NEVENT
      BLOCKS    = 4.0*AVE_WORDS/512.0
C
      WRITE(LUN,'(1X,15(''----''),/,1X,''TOTALS'',4X,2I10,2F10.2),/,
     &            1X,15(''----''))')
     &            SUM_WORDS, SUM_BANKS, AVE_WORDS, AVE_BANKS
      WRITE(LUN,'(1X,''Number of Events             '',I7)' ) NEVENT
      WRITE(LUN,'(1X,''Average Event Size in Blocks '',F10.2)') BLOCKS
      CLOSE(LUN)
C
      END
