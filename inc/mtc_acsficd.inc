C----------------------------------------------------------------------
C- MTC_ACSFICD.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   7-JUL-1993   Elizabeth Gallas
C-
C- store the tube types in each channel of the ICD 
C- AICD_TYPE(1:768) = character*4 identifying icd pmt type
C- CSF_ICD_*(1:384) = numbers from CSF_ICD is stored in /MTC_CSFICD/
C----------------------------------------------------------------------
      COMMON /MTC_ACSFICD/ aicd_type(768)
      CHARACTER*4 aicd_type
