      SUBROUTINE L1UTIL_TRGR_ADC_UNPACK (EM, HD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read and return the trigger tower ADC values from
C-                         the TRGR bank.
C-                         *** NOTE ***
C-                         For simplicity, the offset (08) and scale (0.25 GeV
C-                         per count) are hardcoded at the moment, so that no
C-                         initialization and no LSM file is necesary.
C-
C-   Inputs  : (Implicit) TRGR bank
C-   
C-   Outputs : EM [REAL] array of EM trigger tower energies in GeV
C-             HD [REAL] array of HD trigger tower energies in GeV
C-             
C-   Controls: None
C-
C-   Created   4-FEB-1992   Philippe Laurens, Steven Klocek
C-   Updated  24-FEB-1992   Philippe Laurens, Steven Klocek  
C-                              use GZFIND_CRATE to find actual data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS/LIST'
C
      REAL EM(-20:20,1:32), HD(-20:20,1:32)
C
      INTEGER   LTRGR_LEVEL1, GZTRGR, GZFIND_CRATE
      INTEGER   LDBLOCK_START, ADDRESS
C
C       Offsets in the TRGR Bank cf. d0 note 967
C       parameters derived from L1DBB_DATA_BLOCK.PARAMS
      INTEGER    FIRST_ADC, HD_ADC_OFFSET, NEG_ETA_OFFSET
      PARAMETER (FIRST_ADC      = ( TT_FADC + 1 ) / 2 )
      PARAMETER (HD_ADC_OFFSET  = TT_FADC_L / 4 ) 
      PARAMETER (NEG_ETA_OFFSET = TT_FADC_L / 8 ) 
C
      INTEGER   ETA, PHI 
      INTEGER   TMP
C
      INTEGER   JBYT
      EXTERNAL  JBYT
C
C     INTEGER LUN, DMPUNI
C----------------------------------------------------------------------
C
      LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', GZTRGR( ), CRATE_ID )
C
      IF ( LTRGR_LEVEL1 .LE. 0 ) THEN 
        DO ETA = 1, 20
          DO PHI = 1, 32
            EM(+ETA,PHI) = 0.0
            EM(-ETA,PHI) = 0.0
          END DO
        END DO
        GOTO 999
      END IF
C
      LDBLOCK_START = LTRGR_LEVEL1 + TRGR_HEADER_LENGTH 
C
      ADDRESS = LDBLOCK_START + FIRST_ADC
C
      DO ETA = 1, 20
C
        DO PHI = 1, 16, 2
C
          TMP = IQ( ADDRESS )
          EM(+ETA,PHI+ 0) = (JBYT(TMP, FIRST_BYTE,BYTE_LENGTH) - 8) / 4.
          EM(+ETA,PHI+16) = (JBYT(TMP,SECOND_BYTE,BYTE_LENGTH) - 8) / 4.
          EM(+ETA,PHI+ 1) = (JBYT(TMP, THIRD_BYTE,BYTE_LENGTH) - 8) / 4.
          EM(+ETA,PHI+17) = (JBYT(TMP,FOURTH_BYTE,BYTE_LENGTH) - 8) / 4.
C
          TMP = IQ( ADDRESS + NEG_ETA_OFFSET )
          EM(-ETA,PHI+ 0) = (JBYT(TMP, FIRST_BYTE,BYTE_LENGTH) - 8) / 4.
          EM(-ETA,PHI+16) = (JBYT(TMP,SECOND_BYTE,BYTE_LENGTH) - 8) / 4.
          EM(-ETA,PHI+ 1) = (JBYT(TMP, THIRD_BYTE,BYTE_LENGTH) - 8) / 4.
          EM(-ETA,PHI+17) = (JBYT(TMP,FOURTH_BYTE,BYTE_LENGTH) - 8) / 4.
C
          TMP = IQ( ADDRESS + HD_ADC_OFFSET )
          HD(+ETA,PHI+ 0) = (JBYT(TMP, FIRST_BYTE,BYTE_LENGTH) - 8) / 4.
          HD(+ETA,PHI+16) = (JBYT(TMP,SECOND_BYTE,BYTE_LENGTH) - 8) / 4.
          HD(+ETA,PHI+ 1) = (JBYT(TMP, THIRD_BYTE,BYTE_LENGTH) - 8) / 4.
          HD(+ETA,PHI+17) = (JBYT(TMP,FOURTH_BYTE,BYTE_LENGTH) - 8) / 4.
C
          TMP = IQ( ADDRESS + HD_ADC_OFFSET + NEG_ETA_OFFSET )
          HD(-ETA,PHI+ 0) = (JBYT(TMP, FIRST_BYTE,BYTE_LENGTH) - 8) / 4.
          HD(-ETA,PHI+16) = (JBYT(TMP,SECOND_BYTE,BYTE_LENGTH) - 8) / 4.
          HD(-ETA,PHI+ 1) = (JBYT(TMP, THIRD_BYTE,BYTE_LENGTH) - 8) / 4.
          HD(-ETA,PHI+17) = (JBYT(TMP,FOURTH_BYTE,BYTE_LENGTH) - 8) / 4.
C
          ADDRESS = ADDRESS + 1
C
        END DO
C
      END DO
CC
CC      display result in same format as PRTRGR for cross-checking
CC
C      LUN = DMPUNI()
C      DO ETA = -20, 20
CC
C        IF ( ETA .NE. 0 ) THEN 
C          WRITE(LUN,500) ETA,    ((NINT(4.*HD(ETA,PHI)) +8), PHI=  1,16)
C          WRITE(LUN,510)  1, 16, ((NINT(4.*EM(ETA,PHI)) +8), PHI=  1,16)
C          WRITE(LUN,500) ETA,    ((NINT(4.*HD(ETA,PHI)) +8), PHI= 17,32)
C          WRITE(LUN,510) 17, 32, ((NINT(4.*EM(ETA,PHI)) +8), PHI= 17,32)
C          WRITE(LUN,*) 
C        ENDIF
CC
C  500   FORMAT(' ETA=', SP, I3, SS, '   HD', 16I4)
C  510   FORMAT(' PHI=', I2, ':', I2,  ' EM', 16I4)
CC
C      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
