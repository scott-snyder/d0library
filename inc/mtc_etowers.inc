C----------------------------------------------------------------------
C- MTC_ETOWERS: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   6-OCT-1993   Elizabeth Gallas
C-
C-   This block contains calorimeter information in the
C-   physics projective tower about the current ieta,iphi
C-   projected from the vertex.
C-
C----------------------------------------------------------------------
      INTEGER ITLO,ITHI, ITTLO,ITTHI
      PARAMETER (ITLO=-1,ITHI=1)
      PARAMETER (ITTLO=-2,ITTHI=2)
      COMMON /MTC_ETOWERS/ VTXTWR(3),DVTXTWR(3),
     &            ENTOWER(ITTLO:ITTHI,ITTLO:ITTHI,18), ESUM3(18),
     &            CHITOWER(ITTLO:ITTHI,ITTLO:ITTHI,18), CHIT3(18),
     &            EXPE(ITTLO:ITTHI,ITTLO:ITTHI,18), EXPE3(18),
     &            FCNTCHI(ITTLO:ITTHI,ITTLO:ITTHI),FCNTCHI3,
     &            ICNTCHI(ITTLO:ITTHI,ITTLO:ITTHI),ICNTCHI3,
     &            ITOTCHI(ITTLO:ITTHI,ITTLO:ITTHI),ITOTCHI3,
     &            IETATOWER(18),IPHITOWER(18)

      REAL        VTXTWR,DVTXTWR,
     &            ENTOWER, ESUM3,
     &            CHITOWER, CHIT3,
     &            EXPE, EXPE3,
     &            FCNTCHI,FCNTCHI3
      INTEGER     ITOTCHI,ITOTCHI3,
     &            ICNTCHI,ICNTCHI3,
     &            IETATOWER,IPHITOWER
C----------------------------------------------------------------------
C- /MTC_ETOWERS/ contents:
C- VTXTWR(3) - x,y,z vertex position in last call to mtc_mucaltrack
C- DVTXTWR(3) - uncertainty in vertex coordinates
C- The cal tower looked at by the MTC program is a 5x5 array of cells
C- in 17 possible cal layers defined by the input eta,phi and vertex.
C- Index 18 is for the sum over all layers.
C- The location of the central tower of the array is 
C- IETATOWER(18) - central ieta of tower in each layer
C- IPHITOWER(18) - central iphi of tower in each layer
C- 
C- These arrays have an element for each cell in the 5x5x18 cal cell array.
C- The central eta,phi tower is at (ieta2=0,iphi2=0,ilyr=1:17), where
C- ieta2 and iphi2 are the ieta,iphi indices relative to the cental tower.  
C- ENTOWER(-2:+2,-2:+2,18)  - calorimeter energy
C- CHITOWER(-2:+2,-2:+2,18) - cell energy chi square term
C- EXPE(-2:+2,-2:+2,18)     - expected energy in this cell
C- These arrays contain info for each tower in the 5x5 cal cell array
C- FCNTCHI(-2:+2,-2:+2) - fraction of layers hit
C- ICNTCHI(-2:+2,-2:+2) - number of layers hit
C- ITOTCHI(-2:+2,-2:+2) - total number of layers possible
C-
C- These arrays contain info for sum of the 3x3 cells in each of 17 layers
C- ESUM3(18) - 3x3 energy sum in each of 17 layers (18 is sum over layers)
C- CHIT3(18) - 3x3 chi2 element for each of 17 lyrs (18 for sum over lyrs)
C- EXPE3(18) - 3x3 expected energy in each of 17 lyrs (18 is sum over lyrs)
C- FCNTCHI3  - fraction of layers hit in 3x3 tower
C- ICNTCHI3  - number of layers hit in 3x3 tower
C- ITOTCHI3  - total number of layers in 3x3 tower
