      FUNCTION ZBANK_EDIT (RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : hook for editing events written to output stream
C-
C-   Returned value  : TRUE if write event
C-   Inputs  : RCP_BANK
C-   Outputs : none
C-   Controls:
C-
C-   Created   7-AUG-1991   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_BANK
      INTEGER MAXEDIT
      PARAMETER (MAXEDIT=50)
      CHARACTER MSG*80,EDIT_LIST(MAXEDIT)*32,C*8
      INTEGER I,J,K,NTOT,NPASS,NFAIL,NTOT1,NPASS1,NFAIL1
      LOGICAL FIRST,EDIT,CUT,TEST,TESTX
      LOGICAL ZBANK_EDIT,ZBANK_TEST,ZBANK_EDIT_SUM
      INTEGER VALUE(100),TYPE(100),NVAL,IER
      INTEGER NEDIT,LCHAR,NWORD,INTERVAL
C
      DATA FIRST /.TRUE./
C-------------------------------------------------------
      ZBANK_EDIT =.TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        NTOT = 0
        NPASS = 0
        NFAIL = 0
        CALL EZPICK(RCP_BANK)
        CALL EZGET('DO_EDIT',EDIT,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('DO_EDIT MISSING IN RCP',
     &        'ZBANK_EDIT',RCP_BANK,'W')
          EDIT = .FALSE.
          GOTO 999
        END IF
        CALL EZGET('MESSAGE_INTERVAL',INTERVAL,IER)
        IF(INTERVAL.EQ.0) INTERVAL = 100
        CALL EZGET('CUT',CUT,IER)
        IF (EDIT) THEN
          CALL EZGET_VALUE_TYPE('EDIT_LIST',VALUE,TYPE,NVAL,IER)
          I =  1
          NEDIT = 0
          DO WHILE (I.LE.NVAL)
            IF ( TYPE(I).GT.10) THEN  !CHARACTER
              LCHAR = TYPE(I) - 10
              NWORD = (LCHAR+3)/4                   ! NUMBER OF WORDS
              NEDIT = NEDIT + 1
              EDIT_LIST(NEDIT) = ' '                ! Initializing it.
              CALL UHTOC(VALUE(I),4,EDIT_LIST(NEDIT),LCHAR)
              I = I + NWORD - 1
            ELSE
              CALL ERRMSG('NON CHARACTER IN EDIT_LIST',
     &          'ZBANK_EDIT',' SKIP NUMBER','W')
            END IF
            I = I + 1
          END DO
C
C ****  LOOP OVER EDIT_LIST TO FETCH CONDITIONS
C
          DO I =1, NEDIT
            C = EDIT_LIST(I)
            IF(C(1:3).NE.'AND'.AND.C(1:2).NE.'OR'
     &        .AND.C(1:3).NE.'XOR') THEN
              TEST = ZBANK_TEST( EDIT_LIST(I),IER)
              IF (IER.NE.0) THEN
                CALL ERRMSG('ZBANK_TEST_FAIL','ZBANK_EDIT',
     &            ' ZBANK_TEST = FALSE ','W')
              END IF
            END IF
          END DO
        END IF
        CALL EZRSET
      ENDIF
C
C ****  LOOP OVER EDIT_LIST TO CONSTRUCT LOGIC IN BINARY ORDER
C ****  - MUST ALTERNATE ZBANK_TEST AND BOOLEAN BINARY OPERATIONS BEGINING AND
C ****  - ENDING WITH ZBANK_TEST
C
      IF (EDIT) THEN
        I = 1
        TEST = ZBANK_TEST( EDIT_LIST(I),IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('ZBANK_TEST_FAIL','ZBANK_EDIT',
     &        ' ZBANK_TEST = FALSE ','W')
          GOTO 999
        END IF
  100   C = EDIT_LIST(I+1)
        IF (C(1:3).EQ.'AND') THEN
          TEST = TEST .AND.  ZBANK_TEST( EDIT_LIST(I+2),IER)
        ELSE IF(C(1:2).EQ.'OR') THEN
          TEST = TEST .OR.   ZBANK_TEST( EDIT_LIST(I+2),IER)
        ELSE IF(C(1:3).EQ.'XOR') THEN
          TESTX = TEST 
          TEST = ( (TESTX.OR.ZBANK_TEST(EDIT_LIST(I+2),IER))
     &    .AND.(.NOT.(TESTX.AND.ZBANK_TEST(EDIT_LIST(I+2),IER))) )
        END IF
        I = I + 2
        IF(I.LT.NEDIT) GOTO 100
        IF (IER.NE.0) THEN
          CALL ERRMSG('ZBANK_TEST_FAIL','ZBANK_EDIT',
     &        ' ZBANK_TEST = FALSE ','W')
          GOTO 999
        END IF
        ZBANK_EDIT =  (CUT.OR.TEST).and.(.not.(CUT.AND.TEST))
        IF(ZBANK_EDIT) THEN
           NPASS = NPASS + 1
        ELSE
           NFAIL = NFAIL + 1
         END IF
      END IF
  999 NTOT = NTOT + 1
      IF(EDIT.AND.(MOD(NTOT,INTERVAL).EQ.0)) THEN
        WRITE(MSG,1009)NTOT,NPASS,FLOAT(NPASS)/FLOAT(NTOT)
 1009   FORMAT(' ZB_EDIT  TOT ',I9,' PASS ',I9,' RATE ',F12.7)
        CALL INTMSG(MSG)
      ELSE IF(.NOT.EDIT) THEN
        NPASS = NTOT
      END IF
      RETURN
C#######################################################################
      ENTRY ZBANK_EDIT_SUM (NPASS1,NFAIL1,NTOT1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-APR-1993   Chip Stewart
C-
C----------------------------------------------------------------------
      IF(EDIT) ZBANK_EDIT_SUM = .TRUE.
      NPASS1 = NPASS
      NFAIL1 = NFAIL
      NTOT1  = NTOT
C----------------------------------------------------------------------
      END
