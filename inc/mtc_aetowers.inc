C----------------------------------------------------------------------
C- MTC_AETOWERS: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   6-OCT-1993   Elizabeth Gallas
C-
C-   This block contains the character variable array 
C-   that should accompany block /mtc_etowers/.
C-
C----------------------------------------------------------------------
      COMMON /MTC_AETOWERS/ ATOWER(ITTLO:ITTHI,ITTLO:ITTHI,18)
      CHARACTER*8 ATOWER
C----------------------------------------------------------------------
C- /MTC_AETOWERS/ contents:
C- ATOWER(-2:+2,-2:+2,18)   - position of cell in calorimeter
C-                            (MMMMIIEEE = module - ISUBLAYER - IETA)
C- This array has an element for each cell in the 5x5x18 cal cell array.
C- The central eta,phi tower is at (ieta2=0,iphi2=0,ilyr=1:17), where
C- ieta2 and iphi2 are the ieta,iphi indices relative to the cental tower.  
C----------------------------------------------------------------------
