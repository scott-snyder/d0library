      FUNCTION ZBANK_TEST (RCP_TEST,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TEST BANK WORD SPECIFIED in RCP
C-
C-   Returned value  : TRUE if test sucessful.
C-   Inputs  : RCP_TEST  [C]   ARRAY name assocaited with test - in previously
c-                             EZPICKed RCP bank.
C-   Outputs : IER        [I]  Error code 0 = OK
C-   Controls: Currently EZPICKed RCP bank
C-
C-   Created  7-AUG-1991   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ZBANK_TEST
      CHARACTER*(*) RCP_TEST
      INTEGER MAXVAL,MAXDEP,MAXTEST
      PARAMETER( MAXVAL = 100 )
      PARAMETER( MAXDEP = 100 )
      PARAMETER( MAXTEST = 50 )
      CHARACTER BANK(5)*8,WORD(5)*8,MSG*80
      CHARACTER*32 TEST(MAXTEST),TEST1,TEST0(MAXTEST),C,ZEBRA,COMPA
      INTEGER VALUE(MAXVAL),TYPE(MAXVAL),NVAL,IER,J(MAXVAL)
      INTEGER LNBANK(5),NBANK,IZLINK(MAXDEP,MAXTEST),NDEP(MAXTEST)
      INTEGER NWORD,I,K,L,LUP,LDOWN,NTEST
      INTEGER NCHAR,LCHAR,LBANK,LCPATH,LCHAIN
      REAL    RVALUE(MAXVAL),R(MAXVAL)
      LOGICAL FIRST,ITEST_BANK,RTEST_BANK,INTE(MAXTEST)
      EQUIVALENCE (VALUE(1),RVALUE(1))
C
      INTEGER IWORD(MAXTEST),COMP(MAXTEST),IMAP(MAXTEST)
      REAL    LIMIT(MAXTEST)
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      SAVE NTEST,IMAP
      DATA FIRST /.TRUE./,NTEST/0/
C-------------------------------------------------------
      ZBANK_TEST = .FALSE.
      IER = 0
      TEST1 = ' '
      TEST1 = RCP_TEST
C
C ****  Do a binary search for RCP_TEST
C
      LUP = NTEST + 1
      LDOWN = 0        ! To handle boundary conditions
      L = 1
  100 IF((LUP-LDOWN).LE.1) GOTO 400     ! Finish search unsucessfully
      L = (LUP+LDOWN)/2                 ! Divide search region by two
      IF( TEST1  .LT.  TEST(L) )THEN
        LUP=L                           ! Look in upper half
        GOTO 100
      ELSE IF( TEST1  .GT.  TEST(L) )THEN
        LDOWN=L
        GOTO 100                        ! Look in lower half
      ELSE   ! IF( TEST1  .EQ.  TEST(L) )THEN
        GOTO 500                        ! Found match
      END IF
  400 CONTINUE                          ! No matches
      NTEST = NTEST + 1                 ! Bump number of tests
      IF(NTEST.GT.MAXTEST) THEN
        IER = -2
        CALL ERRMSG('TOO_MANY_TESTS','ZBANK_TEST',' CHAIN '//ZEBRA ,'W')
        GOTO 999
      ENDIF
      TEST0(NTEST) = TEST1
      DO I = 1, NTEST
        TEST(I) = TEST0(I)
        IMAP(I) = I
      END DO
      CALL SRTCHR(TEST,NTEST,IMAP)     ! Sort TEST
C
      CALL EZGET_VALUE_TYPE (TEST1,VALUE,TYPE,NVAL,IER) ! Unpack TEST array
C
      I = 1
      DO WHILE (I.LE.NVAL)
        IF ( TYPE(I).GT.10) THEN  !CHARACTER
          LCHAR = TYPE(I) - 10
          NWORD = (LCHAR+3)/4                       ! NUMBER OF WORDS
          CALL UHTOC(VALUE(I),4,C,LCHAR)
          IF(I.EQ.1) THEN
            ZEBRA = C
            COMPA = ' '
          ELSE
            COMPA = C
          ENDIF
          I = I + NWORD - 1
        ELSE IF ( TYPE(I).EQ.1) THEN  !INTEGER
          IF(COMPA.EQ.' ') THEN
            IWORD(NTEST) = VALUE(I)
          ELSE
            INTE(NTEST) = .TRUE.
            LIMIT(NTEST) = VALUE(I)
          ENDIF
        ELSE                          !REAL
          IF(COMPA.EQ.' ') THEN
            IWORD(NTEST) = RVALUE(I)
          ELSE
            INTE(NTEST) = .FALSE.
            LIMIT(NTEST) = RVALUE(I)
          ENDIF
        END IF
        I = I + 1
      END DO
C
C ****  TARGET BANK
C
      CALL CHOP (ZEBRA,BANK,LNBANK,NBANK)
      LBANK = LCPATH (IXCOM,BANK,NBANK,LHEAD)
      CALL ZCHAIN (IXCOM,LHEAD,LBANK,MAXDEP,BANK,
     &  IZLINK(1,NTEST),NDEP(NTEST),IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('BANK_NOT_FOUND','ZBANK_TEST',' CHAIN '//ZEBRA ,'W')
        NDEP(NTEST) = -1
        GOTO 500
      END IF
C
C ****  EQ NE GT LT GE LT ?
C
      IF (INDEX(COMPA,'LT').GT.0) THEN
        COMP(NTEST) = 1
      ELSE IF (INDEX(COMPA,'LE').GT.0) THEN
        COMP(NTEST) = 2
      ELSE IF (INDEX(COMPA,'EQ').GT.0) THEN
        COMP(NTEST) = 3
      ELSE IF (INDEX(COMPA,'NE').GT.0) THEN
        COMP(NTEST) = 4
      ELSE IF (INDEX(COMPA,'GE').GT.0) THEN
        COMP(NTEST) = 5
      ELSE IF (INDEX(COMPA,'GT').GT.0) THEN
        COMP(NTEST) = 6
      ELSE
        CALL ERRMSG('ARRAY_WRONG','ZBANK_TEST',
     &        COMPA//' NOT LT LE EQ NE GE GT','W')
        NDEP(NTEST) = -1
      END IF
C
C ****  DO CUT/PASS
C
  500 CONTINUE
      I = IMAP(L)
      IF( NDEP(I) .LT.0) THEN
        IER = -1
        GOTO 999
      END IF
      LBANK = LCHAIN(LHEAD,IZLINK(1,I),NDEP(I))
      IF (LBANK.EQ.0) THEN
        CALL ERRMSG('TEST_BANK_MISSING','ZBANK_TEST',
     &    ' ARRAY '//TEST(I),'W')
        IER = -1
        GOTO 999
      ELSE IF (INTE(NTEST)) THEN
        ZBANK_TEST = ITEST_BANK(LBANK,IWORD(I),COMP(I),NINT(LIMIT(I)),1)
      ELSE
        ZBANK_TEST = RTEST_BANK(LBANK,IWORD(I),COMP(I),LIMIT(I),1)
      END IF
  120 FORMAT(1X,' BANK PATH TO EDIT ',20(/1X,I4,5X,A4,5X,I4))
  130 FORMAT(1X,'TARGET BANK: ',A4,/,' ----------------------------'/)
  999 RETURN
      END
