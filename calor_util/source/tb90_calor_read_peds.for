      SUBROUTINE TB90_CALOR_READ_PEDS(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO READ IN TB90 CALORIMETER PEDESTALS
C-
C-   Inputs  : RCP_BANK [C] - RCP BANK CONTROLS PEDESTAL SUBTRACTION
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  11-JUN-1990   Chip Stewart, W.G. Dharmaratna
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_BANK
      CHARACTER*80 FILENAME,STRING
      INTEGER I,J,K,M,N,L,IER,LSTRING
      LOGICAL STATUS,TRNLNM,OK
C----------------------------------------------------------------------
      CALL EZPICK(RCP_BANK)
      CALL EZ_GET_CHARS('PEDESTAL_FILE',N,FILENAME,IER) 
      CALL EZRSET
      IF (IER.NE.0) GOTO 999
      IF (N.EQ.0) GOTO 999
      IF (INDEX(FILENAME,'NO').GT.0) GOTO 999
      IF (INDEX(FILENAME,'no').GT.0) GOTO 999
C
C ****  Translate filename name; could be a logical
C
      CALL WORD (FILENAME,I,J,L)
      STATUS = TRNLNM(FILENAME(I:J),STRING,LSTRING)
      IF ( STATUS ) THEN
        FILENAME = STRING
      ELSE
        LSTRING  = L
      ENDIF
      CALL INTMSG(' Pedestal file to be read is: ')
      CALL INTMSG('      ---> '//FILENAME(1:LSTRING))
C
      CALL READ_PED_FILE(FILENAME,OK)
      CALL FLGSET('PED_READ',OK)

  999 RETURN
      END
