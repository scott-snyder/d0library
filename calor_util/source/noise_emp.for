      REAL FUNCTION NOISE_EMP(IETA,IPHI,ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns noise based on empirical information
C-                         specifically, pedestal distributions for the
C-                         central calorimeter.  Massless Gap and ICD
C-                         are treated as gaussian as is EC for now.
C-
C-   Inputs  : IETA, IPHI, ILAYER
C-   Outputs :
C-   Controls:
C-
C-   Created  23-OCT-1991   Amber Boehnlein
C-   modified 27-APR-1993   Ian Adam
C-                          use noise pedestal data for all channels
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA, IPHI, ILAYER
      REAL    NOISY_HIS
      REAL    GAIN, CAD_GAIN
      LOGICAL CEXIST
C----------------------------------------------------------------------
      NOISE_EMP = 0.0
      IF (CEXIST(IETA,IPHI,ILAYER)) THEN
        GAIN = CAD_GAIN (IETA,ILAYER)  !Convert from ADC counts to energy
        NOISE_EMP = GAIN*NOISY_HIS(IETA,IPHI,ILAYER)
      ENDIF
  999 RETURN
      END
