      SUBROUTINE TB90_CPD8_CORRECT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct the double digitized DBL3 *8 pedestals 
C-                         for the "0.5 ADC count off-set". The pedestals 
C-                         in CPD8 are overwritten with the corrected 
C-                         pedestals. The correction values are read in 
C-                         from TB90_PEDCOR_RCP.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-DEC-1990   Marcel Demarteau 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INTEGER  CARD,LINKH,LINK,LZFIND
      INTEGER  CRATE,I,ICHAN,IER
      REAL     PED_OFFSET(384,12)
      LOGICAL  FIRST
      CHARACTER*40 STRING
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CRATE = 7
        CALL INRCP('TB90_PEDCOR_RCP',IER)
        CALL EZPICK('TB90_PEDCOR_RCP')
        CALL EZGET('PED_OFFSET',PED_OFFSET,IER)
        IF (IER.NE. 0) THEN
          CALL ERRMSG('PROBLEM FETCHING PED CORRECTIONS',
     &      'TB90_CPD8_CORRECT',' NO CORRECTIONS MADE','W')
          GOTO 999
        END IF
        CALL EZRSET
C
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        IF (LCPDH.EQ.0) THEN
          WRITE(STRING,10) CRATE
   10     FORMAT(' ERROR IN CRATE ',I2)
          CALL ERRMSG('NO PEDESTAL BANK','TB90_CPD8_CORRECT',
     &      STRING,'W')
          GO TO 999
        ENDIF
C
        CALL INTMSG(' Correcting *8 Pedestals with TB90_PEDCOR_RCP') 
C
        LINKH = LC(LCPDH-IZCPD8)
        DO CARD = 0, NADCC-1
          LINK  = LZFIND(IDVSTP,LINKH,CARD,11)   !Finds Bank with Card
          IF ( LINK.LE.0 ) THEN
            WRITE(STRING,12) CRATE,CARD
   12       FORMAT(' ERROR IN CRATE ',I2,'  CARD ',I2)
            CALL ERRMSG('NO PEDESTAL BANK','TB90_CPD8_CORRECT',
     &        STRING,'W')
            GOTO 999
          ENDIF
          DO I = 1, 384
            C(LINK+NHEAD+2*I-1) = 
     &        C(LINK+NHEAD+2*I-1) + PED_OFFSET(I,CARD+1)
          END DO
        END DO
C
      END IF
  999 RETURN
      END
