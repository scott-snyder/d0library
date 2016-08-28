      FUNCTION TRIGGER_BIT_CHECK (RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURN TRUE IF TRIGGER BIT FIELD HAS
C-                         AT LEAST ONE OF THE REQUIRED BITS ON.
C-                         WORD 11 OF HEAD BANK IS THE TRIGGER BIT FIELD.
C-                         THE TRIGGER MASK IS BUILT FROM GIVEN RCP FILE
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JUL-1990   Chip Stewart
C-   Updated  25-Feb-1992   Herbert Greenlee
C-      Fixed for UNIX (concatenation)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TRIGGER_BIT_CHECK
      CHARACTER*(*) RCP_BANK
      INTEGER MAX_RCP,NUM_RCP,I_BANK
      PARAMETER(MAX_RCP=20)
      CHARACTER*32 RCP_BANK_LIST(MAX_RCP),LAST_RCP_BANK
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FLGVAL,FIRST,TRIGGER(MAX_RCP),PRINT_MESSAGE(MAX_RCP),NEW
      INTEGER IOS,MASK(MAX_RCP),BITS(32),TRIGGER_BITS,RUN,EVENT
      INTEGER EVONUM,RUNNO,IER,I,N,SIZE,J,K,L,J1,K1
      CHARACTER*80 MSG
      INTEGER TRULEN
      DATA FIRST,NEW /2*.TRUE./
C---------------------------------------------------------------------
      TRIGGER_BIT_CHECK = .TRUE.
      NEW = .TRUE.
C if first call
      IF (FIRST) THEN
        FIRST = .FALSE.
        NUM_RCP = 1
        GOTO 10
      END IF
C search through previous calls 
      CALL WORD(RCP_BANK,J,K,L)
      DO I_BANK = 1, NUM_RCP
        CALL WORD(RCP_BANK_LIST(I_BANK),J1,K1,L)
        IF( RCP_BANK(J:K) .EQ. RCP_BANK_LIST(I_BANK)(J1:K1) )THEN
          NEW = .FALSE.
          GOTO 11
        END IF
      END DO
C add another to list
      NUM_RCP = NUM_RCP + 1
      IF(NUM_RCP.GT.MAX_RCP) THEN
        CALL ERRMSG('TRIG_RCP_GT_MAX','TRIGGER_BIT_CHECK',
     &      'number of RCP banks used .gt. MAX_RCP ','W')
        NEW = .TRUE.
        NUM_RCP = MAX_RCP 
        I_BANK = NUM_RCP
      END IF
   10 CONTINUE
      IF(NEW) THEN
        CALL EZPICK(RCP_BANK)
        CALL EZGET('SELECT_TRIGGERS',TRIGGER(NUM_RCP),IER)
        IF( IER.NE.0) THEN
          MSG = 'NO TRIGGER SWITCH IN '//RCP_BANK
          CALL ERRMSG('NOTRIGSWTCH','TRIGGER_BIT_CHECK',
     &      MSG(1:TRULEN(MSG)),'W')
          TRIGGER(NUM_RCP) = .FALSE.
        END IF
        RCP_BANK_LIST(NUM_RCP) = RCP_BANK
        IF (TRIGGER(NUM_RCP)) THEN
          CALL EZGET('TRIGGER_BITS',BITS,IER)
          CALL EZGETA ('TRIGGER_BITS',0,0,0,N,IER)
          IF( (IER.EQ.0) .OR. (N.GT.0) ) THEN
            MASK(NUM_RCP) = 0
            DO I = 1, N
              MASK(NUM_RCP) = MASK(NUM_RCP) + 2**BITS(I)
            END DO
            WRITE(MSG,20)MASK(NUM_RCP),(BITS(I),I=1,N)
   20       FORMAT(' TRIGGER MASK ',Z10.9,' BITS ',10I3)
            CALL STAMSG(MSG,.FALSE.)
            CALL EZGET
     &        ('PRINT_TRIGGER_MESSAGE',PRINT_MESSAGE(NUM_RCP),IER)
          ELSE
            MASK(NUM_RCP) = 16777215
            CALL ERRMSG('NOTRIGBITS','TRIGGER_BIT_CHECK',
     &        'NO TRIGGER BITS IN RCP - ALL SET ON!','W')
          END IF
        ELSE
          MASK(NUM_RCP) = 16777215
        END IF
        CALL EZRSET
        I_BANK = NUM_RCP
      END IF
C
   11 IF ( TRIGGER (I_BANK) ) THEN
        TRIGGER_BITS=iand(MASK(I_BANK),IQ(LHEAD+11))
        IF(TRIGGER_BITS.EQ.0) THEN
          IF ( PRINT_MESSAGE(I_BANK )) THEN
            WRITE(MSG,40)EVONUM(),RUNNO(),IQ(LHEAD+11),RCP_BANK
   40       FORMAT('SKIP EVENT ',I7,' RUN',I9,
     &        ' TRIGGER BITS',Z10.9,A25)
            CALL INTMSG(MSG)
          ENDIF
          TRIGGER_BIT_CHECK = .FALSE.
        ENDIF
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
