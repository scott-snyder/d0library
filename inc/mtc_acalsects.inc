C----------------------------------------------------------------------
C- MTC_ACALSECTS.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   8-JUL-1993   Elizabeth Gallas
C-
C-This block should accompany the information in /MTC_CALSECTS/
C- There are 10 types of calorimeter sections (defined by IWHERE:
C-      ical=1-10, correspond to the following sections respectively
C-      CCEM  ECEM  CCMG  ICD   ECMG    CCFH  ECIH  ECMH  CCCH  ECOH
C-      (this ordering is the SAME as in SCF_RCP)
C- ACALTYPE(10)  == 4*character expression as above
C----------------------------------------------------------------------
      COMMON /MTC_ACALSECTS/ ACALTYPE(10)
      CHARACTER*4 ACALTYPE
