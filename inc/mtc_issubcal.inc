C----------------------------------------------------------------------
C- MTC_ISSUBCAL: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created  14-FEB-1994   Elizabeth Gallas
C-
C- List the number of cal sub-sub-layer in each cal cell by 
C-      module type and sublayer # (ISSLNUM).
C- In some (ical,isub,ieta), the number of sub-sub-layers is not the norm.
C- The # of sub-sub-layers in these special cal cells are entered in the
C- arrays (ISSLNUM_7, ISSLNUM_8, ISSLNUM_10) by 
C- 1st column=sublayer #, 2nd column = ieta, 3rd col = # of sub-sub-layers
C----------------------------------------------------------------------
      COMMON /MTC_ISSUBCAL/ ISSLNUM, ISSLNUM_7,ISSLNUM_8,ISSLNUM_10
C- calorimeter sub-sub-layer layers numbers (module type, sublayer #)
      INTEGER ISSLNUM(10,5)
C- in ECIH (ical=7)
      INTEGER ISSLNUM_7(4,3)
C- in the ECMH (ical=8)
      INTEGER ISSLNUM_8(5,3)
C- in the ECOH (ical=10)
      INTEGER ISSLNUM_10(5,3)
