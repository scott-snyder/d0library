      SUBROUTINE SET_CAPH(ALGORITHM,TEMPLATE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the CAPH path according to the calorimeter
C-   algorithm name and optionally according to the value of a word at the
C-   specified offset within the bank. Use RESET_CAPH to reset the path.
C-
C-   Inputs  : ALGORITHM        [C*]    Name of algorithm:
C-
C-                                              ELECTRON
C-                                              CONE_JET
C-                                              NN_JET
C-
C-             TEMPLATE(*)      [R]     Specify offset/value of word(s)
C-                                      to be checked.
C-
C-                                      Template format:
C-
C-                                      TEMPLATE(1)     Number of words
C-                                                      to check in CAPH
C-                                      TEMPLATE(2)     Offset of word
C-                                                      in CAPH bank
C-                                      TEMPLATE(3)     Value of word
C-                                                      in CAPH bank
C-
C-                                      If TEMPLATE(1) = 0.0 the template
C-                                      is NOT used.
C-                                      Words 2,3 are repeated TEMPLATE(1)
C-                                      times.
C-
C-   Outputs : IER              [I]     Error code; > = 0 if OK.
C-
C-      ENTRY SHOW_CAPH (ALGORITHM,VERSION,IER)
C-      ENTRY RESET_CAPH
C-      ENTRY CHECK_CAPH_STACK(IER)
C-
C-   Created  10-SEP-1990   Harrison B. Prosper
C-   Updated  18-SEP-1990   K. Wyatt Merritt
C-      Added call to create link area if necessary
C-   Updated   4-JAN-1991   Harrison B. Prosper
C-      Set IER = 1 for stack overflow
C-   Updated  13-AUG-1991   Andrew J. Milder  Modified for MicroDST (MDST)
C-   Updated  15-JUL-1993   N. Graf, R. Astur : Add error message for stack
C-                          overflow
C-   Updated   8-AUG-1993   H. Greenlee : Add error message for stack 
C-                          underflow.
C-   Updated  11-AUG-1993   R. Astur : Unless specified otherwise, cone
C-                          and nearest neigbor algorithms get the default
C-                          parameters. Also, update for microdst use.
C-   Updated   1-SEP-1993   K. Wyatt Merritt   
C-        (1)  Change stack operation so RESET_CAPH *DOES* remove an item 
C-             from the stack if there is only one item on the stack;  
C-             therefore, the stack pointer should go to zero after every
C-             correct set of nested calls (e.g., should be zero at the 
C-             end of any PBD packsge)
C-        (2)  Change stack overflow and underflow to FATAL errors
C-        (3)  Add an entry point CHECK_CAPH_STACK(IER) which returns
C-             IER .NE. 0 if the the stack pointer ICAPH .NE. 0
C-
C-  Updated  14-NOV-1994  R. Astur : Change so that the STACK pointer
C-             is ALWAYS incremented on a call to SET_CAPH even on an
C-             ERROR (no CAPH, no algorithm match). This allows the
C-             user to be able to call RESET_CAPH properly without fear
C-             of causing a stack underflow.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ALGORITHM
      REAL    TEMPLATE(*)
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
C----------------------------------------------------------------------
      CHARACTER*4  PATH, PATH2
      INTEGER OFFSET,I,J,K,GZMDST,LMDST,COUNT_DOWN,CAPH_REP
      INTEGER VERSION,NUMBER,COUNT,LOOP_COUNT
      INTEGER GZPROC,LZLAST,JBANK,GZZTRH,JZTRH
      REAL    LOWER,UPPER,LOWER_SCALE,UPPER_SCALE
      LOGICAL MATCHED,FOUND,MICRO_DST
C
      INTEGER NTEMP, IOFF, ITEMP
C----------------------------------------------------------------------
C
C ****  Create a link area for CAPH if not already in existence
C
      CALL CAPH_LINK
C
      IER = 0
C
C ****  Check algorithm name
C
      MATCHED = .FALSE.
      I = 0
      DO WHILE ( .NOT. MATCHED .AND. (I.LT.A_MAX) )
        I = I + 1
        IF ( ALGORITHM(1:1) .EQ. A_NAME(I)(1:1) ) THEN
          MATCHED = .TRUE.
          NUMBER  = I
        ENDIF
      ENDDO
      IF ( .NOT. MATCHED ) THEN
        IER = -3
        ICAPH = ICAPH + 1
        KCAPH(ICAPH)  = -99     ! Bogus address on stack
        GOTO 999
      ENDIF


C
C ****  Standard or Micro DST format
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST = GZMDST()
        IF (LMDST.LE.0) THEN
          CALL ERRMSG('CALOR','SET_CAPH',
     &    ' MDST BANK DOES NOT EXIST ' ,'W')
          IER = -4
          ICAPH = ICAPH + 1
          KCAPH(ICAPH)  = -99     ! Bogus address on stack
          GOTO 999
        ENDIF
        JBANK = IQ(LMDST+4) + LMDST
        IF (IQ(LMDST+3).EQ.0) GOTO 900
        IQ(JBANK+K_ALGORITHM) = INT(Q(JBANK+K_ALGORITHM))
        COUNT_DOWN = IQ(LMDST+3)
        CAPH_REP = IQ(LMDST+2)
        MICRO_DST = .TRUE.
      ELSE
        JBANK = 0
        JPROC = GZPROC()
        IF ( JPROC .GT. 0 ) JBANK = LQ(JPROC-IZCAPH) 
        COUNT_DOWN = 10000
        MICRO_DST = .FALSE.
      ENDIF
      IF ( JBANK .LE. 0 ) THEN
        IER =-2
        CALL ERRMSG('CALORIMETER','SET_CAPH','LCAPH is zero','W')
        ICAPH = ICAPH + 1
        KCAPH(ICAPH)  = -99     ! Bogus address on stack
        GOTO 999
      ENDIF
C
C ****  Setup template
C
      IF ( TEMPLATE(1) .GE. 1.0 ) THEN
        LOOP_COUNT = TEMPLATE(1)
        LOOP_COUNT = 1 + 2*LOOP_COUNT
      ELSE
        LOOP_COUNT = 0
      ENDIF
C
      DO WHILE ( JBANK .GT. 0 .AND. COUNT_DOWN.GT.0)
C
C ****  Check algorithm number
C
        IF ( IQ(JBANK+K_ALGORITHM) .EQ. NUMBER ) THEN
          FOUND = .TRUE.
C
C ****  Check CAPH words according to template
C
          IF ( LOOP_COUNT .GT. 0 ) THEN
            COUNT = 1
            DO WHILE ( FOUND .AND. (COUNT .LT. LOOP_COUNT) )
C
              OFFSET = TEMPLATE(COUNT+1)
              IF (MICRO_DST .AND. OFFSET.LT.1) THEN
                FOUND = .TRUE.
                COUNT = COUNT + 2
              ELSE
                IF ( TEMPLATE(COUNT+2) .GT. 0.0 ) THEN
                  UPPER_SCALE = (1.0+SET_CAPH_PRECISION)
                  LOWER_SCALE = (1.0-SET_CAPH_PRECISION)
                ELSE
                  UPPER_SCALE = (1.0-SET_CAPH_PRECISION)
                  LOWER_SCALE = (1.0+SET_CAPH_PRECISION)
                ENDIF
                UPPER = TEMPLATE(COUNT+2)*UPPER_SCALE
                LOWER = TEMPLATE(COUNT+2)*LOWER_SCALE
                COUNT = COUNT + 2
C
                IF ( (LOWER .LE. Q(JBANK+OFFSET))  .AND.
     &               (UPPER .GE. Q(JBANK+OFFSET)) ) THEN
                  FOUND = .TRUE.
                ELSE
                  FOUND = .FALSE.
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C
C ****  FOUND bank
C
          IF ( FOUND ) THEN
            IF ( ICAPH .LT. MXCAPH ) THEN
              ICAPH = ICAPH + 1           ! Increment stack pointer
            ELSE
              IER = 1                     ! Stack overflow
              CALL ERRMSG('Stack overflow','SET_CAPH',
     &            'CAPH Stack overflow','F')
            ENDIF
            KCAPH(ICAPH) = JBANK          ! Put new address on stack
            IF (MICRO_DST) THEN
              Q(JBANK+K_ALGORITHM) = FLOAT(IQ(JBANK+K_ALGORITHM))
            ENDIF
            JCAPH = KCAPH(ICAPH)
            GOTO 999                      ! Exit loop
          ENDIF
        ENDIF
        IF (MICRO_DST) THEN
          COUNT_DOWN = COUNT_DOWN - 1
          Q(JBANK+K_ALGORITHM) = FLOAT(IQ(JBANK+K_ALGORITHM))
          JBANK = JBANK + CAPH_REP 
     &       + INT(Q(JBANK-1))*INT(Q(JBANK+3))
          IF (COUNT_DOWN.GT.0) THEN
            IQ(JBANK+K_ALGORITHM) = INT(Q(JBANK+K_ALGORITHM))
          ENDIF
        ELSE
          JBANK = LQ(JBANK)                 ! Next address
        ENDIF
      ENDDO
C
C   not found
C
  900 CONTINUE
      IER = -4
      IF ( ICAPH .LT. MXCAPH ) THEN
        ICAPH = ICAPH + 1           ! Increment stack pointer
      ELSE
        IER = 1                     ! Stack overflow
        CALL ERRMSG('Stack overflow','SET_CAPH',
     &          'CAPH Stack overflow','F')
      ENDIF
      KCAPH(ICAPH) = -99          ! Put bogus address on stack
      JCAPH = KCAPH(ICAPH)
  999 RETURN
C
      ENTRY SHOW_CAPH (ALGORITHM,VERSION,IER)
C----------------------------------------------------------------------
C-   Purpose and Methods : Show path name of currently selected bank.
C----------------------------------------------------------------------
      IF ( JCAPH .GT. 0 ) THEN
C
C ****  Return bank attributes
C
        NUMBER    = IQ(JCAPH+K_ALGORITHM)
        ALGORITHM = A_NAME(NUMBER)
        VERSION   = IQ(JCAPH+K_VERSION)
        CALL PATHGT(PATH2)
        IF ( PATH2.EQ.'MDST' ) VERSION = INT(Q(JCAPH+K_VERSION))
        IER = 0
      ELSE
        NUMBER = 0
        ALGORITHM = 'ERR '
        VERSION = 0
        IER =-1                         ! Bank not found
        CALL ERRMSG('CALORIMETER','SET_CAPH','LCAPH is zero','W')
      ENDIF
 1999 RETURN
C
C ****  Reset path to that of previously selected CAPH path
C
      ENTRY RESET_CAPH
C----------------------------------------------------------------------
C-   Purpose and Methods : Reset CAPH path.
C----------------------------------------------------------------------
      IF ( ICAPH .GE. 1 ) THEN
        ICAPH = ICAPH - 1               ! Decrement stack pointer
      ELSE
        CALL ERRMSG('Stack underflow','RESET_CAPH',
     &    'CAPH Stack underflow','F')
      ENDIF
      IF ( ICAPH .GE. 1 ) THEN
        JCAPH = KCAPH(ICAPH)
      ENDIF
 2999 RETURN
C
C ****  Clear CAPH stack
C
      ENTRY CHECK_CAPH_STACK(IER)
C----------------------------------------------------------------------
C-   Purpose and Methods : return error code if stack is not empty
C----------------------------------------------------------------------  
C
      IER = ICAPH
C
 3999 RETURN
      END

