      SUBROUTINE L1EXTRACT_TRGTWR_ADC_ENERGY(L1_BLOCK,
     &                                       LVL1_ETA, LVL1_PHI,
     &                                       EM_GEV, TOT_GEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the values of ADC bytes for one Trigger
C-      Tower in GeV from the Level 1 Crate of the TRGR Bank.
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +-------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY      |
C-           | and NOT A COPY of it.                                       |
C-           |-------------------------------------------------------------|
C-           | YES :   L1EXTRACT_TRGTWR_ADC_ENERGY ( IQ(LTRGR_LEVEL1),... )|
C-           |-------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                       |
C-           |         L1EXTRACT_TRGTWR_ADC_ENERGY ( L1_BLOCK, ... )       |
C-           +-------------------------------------------------------------+
C- 
C-             LVL1_ETA [I]
C-
C-                The Eta coordinate of the desired Trigger Tower. This should
C-                be in the range [-20, 20] excluding 0.
C-                
C-             LVL1_PHI [I]
C-             
C-                The Phi coordinate of the desired Trigger Tower. This should
C-                be in the range [1, 32].
C-
C-   Outputs : EM_GEV   [R]
C-   
C-                The value of the EM ADC byte, in GeV.
C-                
C-             TOT_GEV  [R]
C-             
C-                The value of EM ADC byte + HD ADC byte, in GeV.
C-   
C-   Controls: none
C-
C-   Created  11-JUN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER L1_BLOCK(0:*)
C
      INTEGER LVL1_ETA, LVL1_PHI
      REAL    EM_GEV, TOT_GEV
C
      INTEGER MAGN_ETA, PHI
      INTEGER ADDRESS, OFFSET
      INTEGER EM_BYTE, HD_BYTE
C
C       The following parameters were taken from PRTRGR_L1_FW_AND_CT_ADC
C
      INTEGER FIRST_ADC, HD_ADC, ADC_NEG_ETA, WORDS_PER_ETA
      PARAMETER (WORDS_PER_ETA = 8)
      PARAMETER (ADC_NEG_ETA = (ETA_MAX-ETA_MIN+1) * WORDS_PER_ETA)
      PARAMETER (FIRST_ADC = (TT_FADC-1)/2)
      PARAMETER (HD_ADC = 2*ADC_NEG_ETA)
C
C       The following parameters will have to be changed if the scale or offset
C       of the ADC bytes ever change
C
      REAL HARDCODED_SCALE_ADC
      PARAMETER (HARDCODED_SCALE_ADC = 0.25)
      INTEGER HARDCODED_OFFSET_ADC
      PARAMETER (HARDCODED_OFFSET_ADC = 8)
C
      ADDRESS = FIRST_ADC
      MAGN_ETA = ABS(LVL1_ETA)
      ADDRESS = ADDRESS + (MAGN_ETA-1)*WORDS_PER_ETA
      IF (LVL1_PHI .GT. 16) THEN
        ADDRESS = ADDRESS + (LVL1_PHI-17) / 2
        OFFSET = 8 + 16 * MOD(LVL1_PHI-1, 2)
      ELSE
        ADDRESS = ADDRESS + (LVL1_PHI-1)  / 2
        OFFSET = 16 * MOD(LVL1_PHI-1, 2)
      ENDIF
C
      IF (LVL1_ETA .LT. 0) ADDRESS = ADDRESS + ADC_NEG_ETA
C
      EM_BYTE = IBITS(
     &  L1_BLOCK(TRGR_HEADER_LENGTH+1+ADDRESS), OFFSET, 8 )
C
      ADDRESS = ADDRESS + HD_ADC
      HD_BYTE = IBITS(
     &  L1_BLOCK(TRGR_HEADER_LENGTH+1+ADDRESS), OFFSET, 8 )
C
      EM_GEV = (EM_BYTE - HARDCODED_OFFSET_ADC) * HARDCODED_SCALE_ADC
C
      TOT_GEV = (EM_BYTE + HD_BYTE - 2 * HARDCODED_OFFSET_ADC)
     &          * HARDCODED_SCALE_ADC
C
C      
C----------------------------------------------------------------------
  999 RETURN
      END
