      SUBROUTINE MTC_FILL_SUBLYR()
C----------------------------------------------------------------------
C- MTC_FILL_SUBLYR: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Call this routine only once, before any events are processed.
C-
C-   Purpose and Methods : - FAST VERSION - no energy chi2 calculated
C-      This routine fills the number of cal module sublayers
C-      array in /MTC_CALSECTS/ and the number of sub-sub-layers
C-      in /issubcal/
C-      for the fast version of the muon cal tracker to run
C-
C-   Created   7-JUL-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers
C- and eta ranges
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'
C----------------------------------------------------------------------
C- ISSLNUM* are cal sub-sub-layer layers numbers by module type, sublayer #.
      INCLUDE 'D0$INC:MTC_ISSUBCAL.INC'
C----------------------------------------------------------------------
C- initialization
C- the calorimeter module types
      ACALTYPE(1) = 'CCEM'
      ACALTYPE(2) = 'ECEM'
      ACALTYPE(3) = 'CCMG'
      ACALTYPE(4) = 'ICD '
      ACALTYPE(5) = 'ECMG'
      ACALTYPE(6) = 'CCFH'
      ACALTYPE(7) = 'ECIH'
      ACALTYPE(8) = 'ECMH'
      ACALTYPE(9) = 'CCCH'
      ACALTYPE(10) = 'ECOH'
C- the number of sublayers in each calorimeter module type
      ISUBLAYER(1) =  4
      ISUBLAYER(2) =  4
      ISUBLAYER(3) =  1
      ISUBLAYER(4) =  1
      ISUBLAYER(5) =  1
      ISUBLAYER(6) =  3
      ISUBLAYER(7) =  5
      ISUBLAYER(8) =  5
      ISUBLAYER(9) =  1
      ISUBLAYER(10) =  3
C----------------------------------------------------------------------
C- COMMON /MTC_ISSUBCAL/ ISSLNUM,ISSLNUM_7,ISSLNUM_8,ISSLNUM_10
C-  ISSLNUM are cal sub-sub-layer layers numbers by module type, sublayer #.
C-  Then in some (ical,isub,ieta), the number of sub-sub-layers is not the norm
C-  The # of sub-sub-layers in these special cal cells are entered in the
C-  other arrays
C-  by 1st column=sublayer #, 2nd column = ieta, 3rd col = # of sub-sub-layers

      ISSLNUM(1,1) = 1
      ISSLNUM(1,2) = 1
      ISSLNUM(1,3) = 1
      ISSLNUM(1,4) = 1
      ISSLNUM(1,5) = 0

      ISSLNUM(2,1) = 1
      ISSLNUM(2,2) = 1
      ISSLNUM(2,3) = 3
      ISSLNUM(2,4) = 2
      ISSLNUM(2,5) = 0

      ISSLNUM(3,1) = 1
      ISSLNUM(3,2) = 0
      ISSLNUM(3,3) = 0
      ISSLNUM(3,4) = 0
      ISSLNUM(3,5) = 0

      ISSLNUM(4,1) = 1
      ISSLNUM(4,2) = 0
      ISSLNUM(4,3) = 0
      ISSLNUM(4,4) = 0
      ISSLNUM(4,5) = 0

      ISSLNUM(5,1) = 1
      ISSLNUM(5,2) = 0
      ISSLNUM(5,3) = 0
      ISSLNUM(5,4) = 0
      ISSLNUM(5,5) = 0

      ISSLNUM(6,1) = 1
      ISSLNUM(6,2) = 1
      ISSLNUM(6,3) = 1
      ISSLNUM(6,4) = 0
      ISSLNUM(6,5) = 0

      ISSLNUM(7,1) = 2
      ISSLNUM(7,2) = 2
      ISSLNUM(7,3) = 2
      ISSLNUM(7,4) = 2
      ISSLNUM(7,5) = 3

      ISSLNUM(8,1) = 2
      ISSLNUM(8,2) = 2
      ISSLNUM(8,3) = 2
      ISSLNUM(8,4) = 2
      ISSLNUM(8,5) = 3

      ISSLNUM(9,1) = 1
      ISSLNUM(9,2) = 0
      ISSLNUM(9,3) = 0
      ISSLNUM(9,4) = 0
      ISSLNUM(9,5) = 0

      ISSLNUM(10,1) = 4
      ISSLNUM(10,2) = 4
      ISSLNUM(10,3) = 4
      ISSLNUM(10,4) = 0
      ISSLNUM(10,5) = 0

C- In some (ical,isub,ieta), the number of sub-sub-layers is not the norm
C- The # of sub-sub-layers in these special cal cells are entered in the
C- following arrays:
C-   1st column=sublayer #, 2nd column = ieta, 3rd col = # of sub-sub-layers
C- in ECIH (ical=7)
      ISSLNUM_7(1,1) = 1
      ISSLNUM_7(1,2) = 17
      ISSLNUM_7(1,3) = 1

      ISSLNUM_7(2,1) = 2
      ISSLNUM_7(2,2) = 18
      ISSLNUM_7(2,3) = 1

      ISSLNUM_7(3,1) = 3
      ISSLNUM_7(3,2) = 19
      ISSLNUM_7(3,3) = 1

      ISSLNUM_7(4,1) = 5
      ISSLNUM_7(4,2) = 21
      ISSLNUM_7(4,3) = 1

! in the ECMH (ical=8)
      ISSLNUM_8(1,1) = 1
      ISSLNUM_8(1,2) = 11
      ISSLNUM_8(1,3) = 1

      ISSLNUM_8(2,1) = 4
      ISSLNUM_8(2,2) = 13
      ISSLNUM_8(2,3) = 1

      ISSLNUM_8(3,1) = 5
      ISSLNUM_8(3,2) = 14
      ISSLNUM_8(3,3) = 1

      ISSLNUM_8(4,1) = 5
      ISSLNUM_8(4,2) = 15
      ISSLNUM_8(4,3) = 2

      ISSLNUM_8(5,1) = 5
      ISSLNUM_8(5,2) = 20
      ISSLNUM_8(5,3) = 2

! in the ECOH (ical=10)
      ISSLNUM_10(1,1) = 1
      ISSLNUM_10(1,2) = 8
      ISSLNUM_10(1,3) = 3

      ISSLNUM_10(2,1) = 1
      ISSLNUM_10(2,2) = 12
      ISSLNUM_10(2,3) = 2

      ISSLNUM_10(3,1) = 2
      ISSLNUM_10(3,2) = 9
      ISSLNUM_10(3,3) = 1

      ISSLNUM_10(4,1) = 3
      ISSLNUM_10(4,2) = 11
      ISSLNUM_10(4,3) = 3

      ISSLNUM_10(5,1) = 3
      ISSLNUM_10(5,2) = 15
      ISSLNUM_10(5,3) = 2
C----------------------------------------------------------------------
  999 RETURN
      END
