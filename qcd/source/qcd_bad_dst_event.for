      SUBROUTINE QCD_BAD_DST_EVENT( BAD_FLAG, NUM_BAD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a BAD_FLAG for the event
C-
C-   Inputs  :
C-   Outputs :  [I] BAD_FLAG  :Bit mask flag (see QCD_BAD_DST) 0=OK
C-              [I] NUM_BAD   :Number of bad conditions found
C-   Controls:
C-
C-   Created  17-DEC-1992   Richard V. Astur
C-   Modified  5-JAN-1993   Andrew G. Brandt use GTJETS_TOTAL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IJET, BAD_FLAG, BAD_FLAG_TEMP, NUM_BAD, NUM_JETS, IER
      REAL TEMPLATE(20)
C----------------------------------------------------------------------
C: Set flag to OK
      BAD_FLAG = 0
      NUM_BAD  = 0

C: Pick  nearest neighbor algorithm
      TEMPLATE(1) = 0     ! No preference
      CALL SET_CAPH('NN_JET',TEMPLATE,IER)

C: Skip if cant do
      IF ( IER .NE. 0 ) THEN
        BAD_FLAG = -1
        GOTO 999
      END IF

C: Loop over jets
      CALL GTJETS_TOTAL(NUM_JETS,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('QCD_BAD_DST_EVENT','QCD_BAD_DST_EVENT',
     &          'No jets bank found','W')
        BAD_FLAG = -1
        GOTO 999
      END IF
      DO IJET=1,NUM_JETS
        CALL QCD_BAD_DST( IJET, BAD_FLAG_TEMP )
        IF ( BAD_FLAG_TEMP .GT. 0 ) THEN
          NUM_BAD = NUM_BAD + 1
          BAD_FLAG = IOR( BAD_FLAG, BAD_FLAG_TEMP )
        ENDIF
      ENDDO

  999 CALL RESET_CAPH
      RETURN
      END
