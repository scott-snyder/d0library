C----------------------------------------------------------------------
C- MTC_EHTOWERS: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   10-NOV-1993   Elizabeth Gallas
C-
C-   This block contains HADRONIC calorimeter information in the
C-   physics projective tower about the current ieta,iphi
C-   projected from the vertex.
C-
C----------------------------------------------------------------------
      COMMON /MTC_EHTOWERS/
     &            ENTH(ITTLO:ITTHI,ITTLO:ITTHI), ESUMH3,
     &            CHITH(ITTLO:ITTHI,ITTLO:ITTHI), CHITH3,
     &            EXPEH(ITTLO:ITTHI,ITTLO:ITTHI), EXPEH3,
     &            FCNTCH(ITTLO:ITTHI,ITTLO:ITTHI),FCNTCH3,
     &            ICNTCH(ITTLO:ITTHI,ITTLO:ITTHI),ICNTCH3,
     &            ITOTCH(ITTLO:ITTHI,ITTLO:ITTHI),ITOTCH3

      REAL        ENTH, ESUMH3,
     &            CHITH, CHITH3,
     &            EXPEH, EXPEH3,
     &            FCNTCH,FCNTCH3
      INTEGER     ITOTCH,ITOTCH3, ICNTCH,ICNTCH3
C----------------------------------------------------------------------
C- /MTC_EHTOWER/ is similar to /MTC_ETOWERS/ in structure.
C- Its purpose is to store energies, chi2, fractions and counts for
C- hadronic towers in the MTC package.
C- ENTH(-2:+2,-2:+2)   - energy in each of 5x5 hadronic towers
C- CHITH(-2:+2,-2:+2)  - energy chi2 in each of 5x5 hadronic towers
C- EXPEH(-2:+2,-2:+2)  - expected energy in each of 5x5 hadronic towers
C- FCNTCH(-2:+2,-2:+2) - fraction of layers hit in each of 5x5 had towers
C- ICNTCH(-2:+2,-2:+2) - number of hit layers in each of 5x5 had towers
C- ITOTCH(-2:+2,-2:+2) - number of possible layers in each of 5x5 had towers
C- ESUMH3  - energy sum in all cells within the 3x3 hadronic tower
C- CHITH3  - energy chi2 corresponding to this energy sum 
C- EXPEH3  - expected energy deposited by MIP in this 3x3 hadronic tower
C- FCNTCH3 - fraction of layers hit in the 3x3 hadronic tower
C- ICNTCH3 - number of layers hit in 3x3 hadronic tower
C- ITOTCH3 - number of possible layers in 3x3 hadronic tower
C----------------------------------------------------------------------
