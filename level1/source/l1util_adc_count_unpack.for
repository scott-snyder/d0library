      SUBROUTINE L1UTIL_ADC_COUNT_UNPACK(DBLOCK_ADC, FADC_BYTE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack ADC bytes from a section of a datablock, and
C-     unpack into an arry. See also D0 Note 967.
C-
C-   Inputs  : DBLOCK_ADC       The ADC portion of a Level 1 Datablock
C-   Outputs : FADC_BYTE        The array of ADC bytes.
C-   Controls: none
C-
C-   Created   2-MAR-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
C
      INTEGER DBLOCK_ADC(1:*)
      INTEGER FADC_BYTE(POS_ETA:NEG_ETA,
     +                  ETA_MIN:ETA_MAX,
     +                  PHI_MIN:PHI_MAX,
     +                  EM_TOWER:HD_TOWER)
C
      INTEGER CHANNEL, ETA_SIGN, ETA, PHI, COUNT, BIT
      INTEGER JBYT
      INTEGER PHI_HALF
      PARAMETER (PHI_HALF = (PHI_MAX - PHI_MIN + 1)/2)
      INTEGER BYTE_LENGTH
      PARAMETER (BYTE_LENGTH = 8)
      INTEGER LONG_WORD_LENGTH
      PARAMETER (LONG_WORD_LENGTH = BYTE_LENGTH * 4)
C
      COUNT = 1
      BIT = 1
C
      DO CHANNEL = EM_TOWER, HD_TOWER
        DO ETA_SIGN = POS_ETA, NEG_ETA
          DO ETA = ETA_MIN, ETA_MAX
            DO PHI = PHI_MIN, PHI_MIN+PHI_HALF-1
              FADC_BYTE(ETA_SIGN,ETA, PHI, CHANNEL)
     &          = JBYT(DBLOCK_ADC(COUNT), BIT, BYTE_LENGTH)
              BIT = BIT + BYTE_LENGTH
              FADC_BYTE(ETA_SIGN,ETA,PHI+PHI_HALF,CHANNEL) 
     &          = JBYT(DBLOCK_ADC(COUNT), BIT, BYTE_LENGTH )
              BIT = BIT + BYTE_LENGTH
              IF(BIT.GT.LONG_WORD_LENGTH) THEN
                COUNT = COUNT + 1
                BIT  = 1
              ENDIF
            END DO
          END DO
        END DO
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
