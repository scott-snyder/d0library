      LOGICAL FUNCTION CAL_TB_CHK ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks ADC header for TB90 flag 
C-                      NOTE - until TB flag is implemented,
C-                      CAL_TB_CHK will just check number of 
C-                      words in header.
C-
C-   Returned value  :  TRUE IF TB DATA
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   1-FEB-1990   WELATHANTRI G. DHARMARATNA, CHIP STEWART
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INTEGER LCAD1,HEDLEN
C----------------------------------------------------------------------
C
      CAL_TB_CHK = .FALSE.          
      LCAD1=LQ(LHEAD-IZCAD1)
      IF(LCAD1.NE.0) THEN
        HEDLEN=IQ(LCAD1+1)
        IF (HEDLEN.GT.3) THEN 
C
C ****   TB FLAG NOT IN ADC HEADER YET 
C ****   JUST CHECK NUMBER OF WORDS IN HEADER IF>3 THEN TB DATA
C
          CAL_TB_CHK = .TRUE.          
          GOTO 999
        END IF
      END IF
  999 RETURN
      END
