      SUBROUTINE PRTRGR_L1_FW_AND_CT_ADC(PRUNIT, LTRGR_LEVEL1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the ADC counts from the Level 1 Datablock
C-     Crate in the TRGR bank.
C-
C-   Inputs  : PRUNIT   The unit number to write to.
C-             LTRGR_LEVEL1   The offset into IQ of the Level 1 Datablock crate
C-   Outputs : file output
C-   Controls: none
C-
C-   Created  24-FEB-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER LTRGR_LEVEL1, PRUNIT
C
      INTEGER LDBLOCK_START
      CHARACTER*132 LINE
      CHARACTER*80 ADC_EM_LOW, ADC_HD_LOW, ADC_EM_HIGH, ADC_HD_HIGH
      INTEGER ISTAT, ADDRESS
      INTEGER EM_WORD, HD_WORD
      INTEGER POSITION
      INTEGER ETA, PHI
C
      INTEGER FIRST_ADC, HD_ADC, ADC_NEG_ETA, WORDS_PER_ETA
      PARAMETER (WORDS_PER_ETA = 8)
      PARAMETER (ADC_NEG_ETA = (ETA_MAX-ETA_MIN+1) * WORDS_PER_ETA)
      PARAMETER (FIRST_ADC = (TT_FADC-1)/2)
      PARAMETER (HD_ADC = 2*ADC_NEG_ETA)
C
      INTEGER TRULEN, JBIT, JBYT
      EXTERNAL TRULEN
      EXTERNAL JBIT, JBYT
C----------------------------------------------------------------------
C
      LDBLOCK_START = LTRGR_LEVEL1 + TRGR_HEADER_LENGTH + 1
C
C
C       Do the ADC count dump, if requested
C
      WRITE(PRUNIT,*) 'Trigger Tower ADC Values in Counts'
      WRITE(PRUNIT,*) '=================================='
C
      IF (LTRGR_LEVEL1 .LE. 0) THEN
        WRITE(PRUNIT,*) 
        WRITE(PRUNIT,*) 'Couldn''t find LEVEL 1 crate' 
        CALL ERRMSG('NO LEVEL 1 CRATE', 'PRTRGR_L1_FW_AND_CT_ADC',
     &    ' Couldn''t find LEVEL 1 crate ', 'W')
        GOTO 999
      ENDIF
C
      DO ETA = -ETA_MAX, ETA_MAX
C       Skip ETA = 0
        IF (ETA .EQ. 0) GOTO 550
C
        ADC_EM_LOW = ' '
        ADC_HD_LOW = ' '
        ADC_EM_HIGH = ' '
        ADC_HD_HIGH = ' '
C
  500   FORMAT(' ETA=', SP, I3, SS, '   HD', A)
  510   FORMAT(' PHI=', I2, ':', I2,  ' EM', A)
  520   FORMAT(' ', I3)
        DO POSITION = 0, 15, 2
          ADDRESS = LDBLOCK_START + FIRST_ADC
          IF (ETA .LT. 0) ADDRESS = ADDRESS + ADC_NEG_ETA
          ADDRESS = ADDRESS + (IABS(ETA)-1)*WORDS_PER_ETA
          ADDRESS = ADDRESS + POSITION/2
          EM_WORD = IQ(ADDRESS)
          HD_WORD = IQ(ADDRESS + HD_ADC)
C
          WRITE(ADC_EM_LOW(POSITION*4+1:POSITION*4+4),520,IOSTAT=ISTAT) 
     &      JBYT(EM_WORD, FIRST_BYTE, BYTE_LENGTH)
          WRITE(ADC_EM_HIGH(POSITION*4+1:POSITION*4+4),520,IOSTAT=ISTAT)
     &      JBYT(EM_WORD, SECOND_BYTE, BYTE_LENGTH)
          WRITE(ADC_EM_LOW(POSITION*4+5:POSITION*4+8),520,IOSTAT=ISTAT)
     &      JBYT(EM_WORD, THIRD_BYTE, BYTE_LENGTH)
          WRITE(ADC_EM_HIGH(POSITION*4+5:POSITION*4+8),520,IOSTAT=ISTAT)
     &      JBYT(EM_WORD, FOURTH_BYTE, BYTE_LENGTH)
C
          WRITE(ADC_HD_LOW(POSITION*4+1:POSITION*4+4),520,IOSTAT=ISTAT)
     &      JBYT(HD_WORD, FIRST_BYTE, BYTE_LENGTH)
          WRITE(ADC_HD_HIGH(POSITION*4+1:POSITION*4+4),520,IOSTAT=ISTAT)
     &      JBYT(HD_WORD, SECOND_BYTE, BYTE_LENGTH)
          WRITE(ADC_HD_LOW(POSITION*4+5:POSITION*4+8),520,IOSTAT=ISTAT)
     &      JBYT(HD_WORD, THIRD_BYTE, BYTE_LENGTH)
          WRITE(ADC_HD_HIGH(POSITION*4+5:POSITION*4+8),520,IOSTAT=ISTAT)
     &      JBYT(HD_WORD, FOURTH_BYTE, BYTE_LENGTH)
C
        END DO
C
        WRITE(PRUNIT,*)
        WRITE(PRUNIT,500,IOSTAT=ISTAT) ETA, 
     &    ADC_HD_LOW(1:TRULEN(ADC_HD_LOW))
        WRITE(PRUNIT,510,IOSTAT=ISTAT) 1, 16,
     &    ADC_EM_LOW(1:TRULEN(ADC_EM_LOW))
        WRITE(PRUNIT,500,IOSTAT=ISTAT) ETA,
     &    ADC_HD_HIGH(1:TRULEN(ADC_HD_HIGH))
        WRITE(PRUNIT,510,IOSTAT=ISTAT) 17, 32,
     &    ADC_EM_HIGH(1:TRULEN(ADC_EM_HIGH))
C
  550   CONTINUE
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
