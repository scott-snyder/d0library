C----------------------------------------------------------------------
C- MTC_CALSECTS.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   8-JUL-1993   Elizabeth Gallas
C-
C- There are 10 types of calorimeter sections (defined by IWHERE:
C-      ical=1-10, correspond to the following sections respectively
C-      CCEM  ECEM  CCMG  ICD   ECMG    CCFH  ECIH  ECMH  CCCH  ECOH
C-      (this ordering is the SAME as in SCF_RCP)
C- ACALTYPE(10)  == 4*character expression as above in /MTC_ACALSECTS/
C- ISUBLAYER(10) == number of sublayers in each of 10 calorimeter sections
C- Each of these sublayers have ISLNUM(ICAL,ISUB) sections in eta's
C- with ISLETA(ICAL,ISUB) starting ieta values ...
C- Values are set to 0,-999. for sublayers that do not exist
C----------------------------------------------------------------------
      COMMON /MTC_CALSECTS/ ISUBLAYER(10), ISLNUM(10,5), ISLETA(10,5)
      INTEGER ISUBLAYER
      INTEGER ISLNUM, ISLETA, I_LAYER
