C----------------------------------------------------------------------
C- MTC_E5TOWERS: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   21-DEC-1993  Elizabeth Gallas
C-
C-   This block contains calorimeter information in the 
C-   outer cells of the 5x5 projective tower about the current ieta,iphi
C-   projected from the vertex.
C-
C----------------------------------------------------------------------
      COMMON /MTC_E5TOWERS/
     &            ESUMT5, ESUMTH5, ESUMH5,
     &            CHITH5,
     &            ESUM5(18),CHIT5(18),
     &            FCNTCHI5,ICNTCHI5,ITOTCHI5,
     &            FCNTCH5,ICNTCH5,ITOTCH5

      REAL        ESUMT5, ESUMTH5, ESUMH5,
     &            CHITH5,
     &            ESUM5, CHIT5,
     &            FCNTCHI5, FCNTCH5
      INTEGER     ITOTCHI5, ICNTCHI5,
     &            ITOTCH5, ICNTCH5
C----------------------------------------------------------------------
C- /MTC_E5TOWERS/ contains info similar to /MTC_ETOWERS/ and /MTC_EHTOWERS/
C- ESUMT5    -  energy sum in outer cells of 5x5X17 calorimeter tower
C- ESUMTH5   -  hadronic energy sum outer cells of 5x5X(11:17) cal tower
C- ESUMH5    -  total energy sum in all cells of 5x5X17 calorimeter tower
C- CHITH5    -  total energy chi2 corresponding to this energy sum
C- ESUM5(18) -  energy sum in each layer (18=total 5x5x17 tower energy)
C- CHIT5(18) -  5x5 chi2 element for each of 17 lyrs (18 for sum over lyrs)
C- FCNTCHI5  -  fraction of layers hit in 5x5 tower    
C- ICNTCHI5  -  number of layers hit in 5x5 tower      
C- ITOTCHI5  -  number of possible layers in 5x5 tower 
C- FCNTCH5   -  hadronic fraction of layers hit in 5x5 tower    
C- ICNTCH5   -  number of hadronic layers hit in 5x5 tower      
C- ITOTCH5   -  number of possible hadronic layers in 5x5 tower 
C----------------------------------------------------------------------
