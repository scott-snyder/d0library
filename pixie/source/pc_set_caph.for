      SUBROUTINE PC_SET_CAPH(NAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get algorithm parameters from current
C-   RCP bank and set the CAPH bank path. NAME can be either
C-   ELECTRON or JETS.
C-
C-   Inputs  : NAME     [C*]    ELECTRON or JETS
C-   Outputs : IER      [I]     0 - Ok
C-   Controls:
C-
C-   Created  28-JAN-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IER
C
      CHARACTER*10 A_TYPE,ALGORITHM
      CHARACTER*80 MESS
      REAL    TEMPLATE(5)
      INTEGER I,J,K
C----------------------------------------------------------------------
C
C ****  Check which set of parameters to get
C
      IER = 0
      IF ( NAME(1:1) .EQ. 'E' ) THEN
        A_TYPE = 'ELEC'
      ELSEIF ( NAME(1:1) .EQ. 'J' ) THEN
        A_TYPE = 'JETS'
      ELSE
        IER = -1
        MESS = 'Wrong algorithm type : '//NAME(1:LEN(NAME))
        CALL ERRMSG('PIXIE','PC_SET_CAPH',MESS,'W')
        GOTO 999
      ENDIF
C
C ****  Get actual algorithm name from RCP bank
C
      MESS = A_TYPE(1:4)//'ALGORITHM'
      CALL WORD(MESS,I,J,K)
      CALL PUGETA(MESS(1:K),ALGORITHM)
C
C ****  Get template
C
      MESS = A_TYPE(1:4)//'WORDS'
      CALL WORD(MESS,I,J,K)
      CALL PUGETV(MESS(1:K),TEMPLATE(1))
      MESS = A_TYPE(1:4)//'WORD1'
      CALL WORD(MESS,I,J,K)
      CALL PUGETV(MESS(1:K),TEMPLATE(2))
      MESS = A_TYPE(1:4)//'VALUE1'
      CALL WORD(MESS,I,J,K)
      CALL PUGETV(MESS(1:K),TEMPLATE(3))
      MESS = A_TYPE(1:4)//'WORD2'
      CALL WORD(MESS,I,J,K)
      CALL PUGETV(MESS(1:K),TEMPLATE(4))
      MESS = A_TYPE(1:4)//'VALUE2'
      CALL WORD(MESS,I,J,K)
      CALL PUGETV(MESS(1:K),TEMPLATE(5))
C
C ****  Check number of words to check: should be 0,1 or 2
C
      IF ( (TEMPLATE(1) .LT. 0.0) .OR. (TEMPLATE(1) .GT. 2.0) ) THEN
        CALL ERRMSG('PIXIE','PC_SET_CAPH',
     &    'Check template: invalid count','W')
        GOTO 999
      ENDIF
C
C ****  Now set the CAPH path
C
      CALL SET_CAPH(ALGORITHM,TEMPLATE,IER)
      IF ( IER .LT. 0 ) THEN
        CALL ERRMSG('PIXIE','PC_SET_CAPH',
     &    'Cannot find specified algorithm in CAPH','W')
        GOTO 999
      ENDIF
      RETURN
C
      ENTRY PC_RESET_CAPH
      CALL RESET_CAPH
  999 RETURN
      END
