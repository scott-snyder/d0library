C----------------------------------------------------------------------
C- MTC_CSFICD.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   7-JUL-1993   Elizabeth Gallas
C-
C- store the conversion factors from adc to total energy 
C- tile to tile in GeV for the ICD ...
C- CSF_ICD_*(1:384) = numbers from CSF_ICD
C- AICD_TYPE(1:768) = char*4 icd pmt type stored in /MTC_ACSFICD/
C----------------------------------------------------------------------
      COMMON /MTC_CSFICD/ csf_icd_n(384), csf_icd_s(384)
      REAL csf_icd_n, csf_icd_s
