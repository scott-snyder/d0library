      LOGICAL FUNCTION ISARCP_EDIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : hook for editing events written to output stream
C-
C-   Returned value  : TRUE if write event
C-   Inputs  : none
C-   Outputs : none
C-   Controls: ISARCP_RCP bank
C-
C-   Created  13-FEB-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      INTEGER MAXVAL,MAXDEP
      PARAMETER( MAXVAL = 100 )
      PARAMETER( MAXDEP = 100 )
      CHARACTER*8 BANK(5),WORD(5)
      CHARACTER C(MAXVAL)*20,MSG*80
      INTEGER VALUE(MAXVAL),TYPE(MAXVAL),NVAL,IER,J(MAXVAL)
      INTEGER LNBANK(5),NBANK,IZLINK(MAXDEP),NDEP
      INTEGER LNWORD(5),NWORD,COMPA,I,K
      INTEGER NINTE,NREAL,NCHAR,LCHAR,LBANK,LCPATH,IWORD,LCHAIN
      REAL   RVALUE(MAXVAL),R(MAXVAL)
      LOGICAL FIRST,EDIT,CUT,ITEST_BANK,RTEST_BANK,TEST
      EQUIVALENCE (VALUE(1),RVALUE(1))
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      DATA FIRST /.TRUE./
C-------------------------------------------------------
      ISARCP_EDIT=.TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ISARCP_RCP')
        CALL EZGET('DO_EDIT',EDIT,IER)
        IF (EDIT) THEN
          CALL EZGET_VALUE_TYPE('EDIT_LIST',VALUE,TYPE,NVAL,IER)
C
C ****  SORT OUT EDIT LIST
C
          I = 1
          NINTE = 0
          NCHAR = 0
          NREAL = 0
          DO WHILE (I.LE.NVAL) 
C TYPE INTEGER=1 REAL=2 REAL(E-format)=3 LOGICAL=4 CHARACTER=10+LEN
            IF ( TYPE(I).GT.10) THEN  !CHARACTER
              LCHAR = TYPE(I) - 10
              NWORD = (LCHAR+3)/4                       ! NUMBER OF WORDS
              NCHAR = NCHAR + 1
              C(NCHAR) = ' '                            ! Initializing it.
              CALL UHTOC(VALUE(I),4,C(NCHAR),LCHAR)
              I = I + NWORD - 1
            ELSE IF ( TYPE(I).EQ.1) THEN  !INTEGER
              NINTE = NINTE + 1
              J(NINTE) = VALUE(I)
            ELSE                          !REAL 
              NINTE = NINTE + 1
              R(NINTE) = RVALUE(I)              
            END IF 
            I = I + 1
          END DO
C
C ****  CUT OR PASS
C
          CUT = .FALSE.
          IF( INDEX (C(1),'CUT').GT.0) CUT = .TRUE.
C
C ****  TARGET BANK
C
          CALL CHOP (C(2),BANK,LNBANK,NBANK)
          LBANK = LCPATH (IXCOM,BANK,NBANK,LHEAD)
          CALL ZCHAIN (IXCOM,LHEAD,LBANK,MAXDEP,BANK,IZLINK,NDEP,IER)
C
          IF ( IER .EQ. 0 ) THEN
C            WRITE(MSG,120) (K,BANK(K),IZLINK(K),K=1,NDEP)
            CALL ERRMSG('ISARCP','ISARCP_EDIT',MSG,'I')
            CALL DHTOC (4,IQ(LBANK-4),BANK(NDEP+1))
C            WRITE(MSG,130) BANK(NDEP+1)
            CALL ERRMSG('ISARCP','ISARCP_EDIT',MSG,'I')
          END IF
C
C ****  WORD NUMBER ( / ADDRESS / BIT MASK ?)
C
          CALL CHOP (C(3),WORD,LNWORD,NWORD)          
          IF (INDEX(WORD(1),'WORD').GT.0) THEN
            READ (WORD(2),*) IWORD
          ELSE
            MSG = ' NO OTHER ADDRESSING SCHEME IN ISARCP_EDIT YET'
            CALL ERRMSG('ISARCP','ISARCP_EDIT',MSG,'I')
          END IF
C
C ****  EQ NE GT LT GE LT ?
C
          IF (INDEX(C(4),'LT').GT.0) THEN
            COMPA = 1
          ELSE IF (INDEX(C(4),'LE').GT.0) THEN
            COMPA = 2
          ELSE IF (INDEX(C(4),'EQ').GT.0) THEN
            COMPA = 3
          ELSE IF (INDEX(C(4),'NE').GT.0) THEN
            COMPA = 4
          ELSE IF (INDEX(C(4),'GE').GT.0) THEN
            COMPA = 5
          ELSE IF (INDEX(C(4),'GT').GT.0) THEN
            COMPA = 6
          ELSE
            TYPE *, ' NO OTHER COMPARISON SCHEME IN ISARCP_EDIT YET'
          END IF
        END IF
      END IF
C
C ****  DO CUT/PASS
C
      IF (EDIT) THEN
        LBANK = LCHAIN(LHEAD,IZLINK,NDEP)
        IF (LBANK.LE.0) THEN
          MSG = ' NO BANK AS SPECIFIED'
          CALL ERRMSG('ISARCP','ISARCP_EDIT',MSG,'I')          
          ISARCP_EDIT =  .TRUE.
          GOTO 999
        END IF
        IF (NINTE.GT.0) THEN
          TEST = ITEST_BANK(LBANK,IWORD,COMPA,J,NINTE)
        ELSE
          TEST = RTEST_BANK(LBANK,IWORD,COMPA,R,NREAL)
        END IF
        IF (CUT) THEN
          ISARCP_EDIT = .NOT. TEST
        ELSE
          ISARCP_EDIT =  TEST          
        END IF
      END IF
  120 FORMAT(1X,' BANK PATH TO EDIT ',20(/1X,I4,5X,A4,5X,I4))
  130 FORMAT(1X,'TARGET BANK: ',A4,/,' ----------------------------'/)
  999 RETURN
      END
