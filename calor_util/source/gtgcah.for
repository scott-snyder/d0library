      SUBROUTINE GTGCAH(ITRAK,ITRA,VERT,P,IDATA,TOTAL,NPOINT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds GCAH bank for first track number .GT. ITRAK
C-                              returns track data
C-
C-   Inputs  : ITRAK     = Lowest Geant track number to look for
C-   Outputs : ITRA      = Track actually found (-1) if no more available
C-             VERT(3,3)
C-                 (i,1) = X,Y,Z of track vertex
C-                 (i,2) = X,Y,Z at entry into Calorimeter
C-                 (i,3) = X,Y,Z at 1st interaction in Calorimeter
C-             P(4,2)
C-                 (i,1) = ISAJET track 4-momentum
C-                 (i,2) = track 4-momentum at entry into Calorimeter
C-             IDATA(1)  = Geant track type
C-                  (2)  = Geant origin vertex #
C-                  (3)  = Geant parent vertex #
C-                  (4)  = Geant parent track #
C-                  (5)  = Production interaction type
C-                            1   -> ISAJET track                - -
C-                            11  -> decay product              'DCAY'
C-                            12  -> hadronic interaction       'HADR'
C-                            13  -> Muon-nuclear interaction   'MUNU'
C-                            14  -> Photo fission              'PFIS'
C-                            15  -> Pair production            'PAIR'
C-                            16  -> Compton scattering         'COMP'
C-                            17  -> Photo production           'PHOT'
C-                            18  -> Annihilation               'ANNI'
C-                            21  -> Punchthrough                - -
C-                            22  -> Bremstrahlung              'BREM'
C-                            23  -> Delta ray electron         'DRAY'
C-                            999 -> Stopping end vertex         - -
C-                  (6)  = Isajet parent vertex #
C-                  (7)  = Isajet track #
C-                  (8)  = 1st Calorimeter interaction type (same as above +)
C-                            2   -> stopping track              - -
C-             TOTAL(1)  = Calorimeter energy UCAL + ECAL
C-                  (2)  = Energy in Massless Gaps
C-                  (3)  = Number of Scintillator Hits
C-                  (4)  = Energy in cracks + cryostat + end plates
C-             NPOINT    = Number of Two word data entries following
C- **** The Address and Energy data are NOT returned by this routine. ****
C-    For V0.10 and earlier versions of D0Geant:
C-              CALL GCELLE(ECELL,IZ,IE,IP,IL,EOD)
C-    to retrieve the energies - cycle through until EOD = .TRUE.
C-              ECELL is an INTEGER energy in KeV
C-
C-     For V0.11 to V1.7 use:
C-              CALL GTENXT(ITRAK,IPOINT,ECELL,IZ,IETA,IPHI,LAYER,EOD)
C-     to retrieve the IPOINT'th entry
C-              1 <= IPOINT <= NPOINT
C-              ECELL is an INTEGER energy in KeV
C-
C-      For V2.0 and later versions use:
C-              CALL GEGCAH(ITRAK,IPOINT,CELLE,IETA,IPHI,LAYER)
C-      to retrieve the IPOINT'th entry
C-              1 <= IPOINT <= NPOINT
C-              CELLE is an REAL energy in GeV
C-
C-      For V2.3 and later versionsof D0GEANT the dimensionality of VERT, P and
C-      IDATA have been increased to return the position and momentum at entry
C-      into the Calorimeter and the position and interaction type of the first
C-      interaction in the Calorimeter.
C-
C-   Created  12-MAY-1987   Alan M. Jonckheere
C-   Updated  16-FEB-1989   Alan M. Jonckheere
C-   Updated   1-OCT-1989   Chip Stewart  - hadronic part - entry/interaction
C-   Updated  11-MAR-1991   Alan M. Jonckheere  Create and call GDGCAH to
C-                              actually return data. Change is due to the
C-                              showerlibrary's desire for speed.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C INPUT VARIABLES
      INTEGER ITRAK
C OUTPUT VARIABLES
      INTEGER ITRA
      REAL VERT(3,3),P(4,2),TOTAL(4)
      INTEGER IDATA(8),NPOINT
C Zebra variables
      INTEGER LGCAH,GZGCAH
C----------------------------------------------------------------------
C
C ****  GET CORRECT GCAH
C
      ITRA = ITRAK - 1
  100 ITRA = ITRA + 1
      LGCAH = GZGCAH(ITRA)
      IF ( LGCAH.EQ.0 ) GOTO 100
      IF ( LGCAH.LT.0 ) THEN
        ITRA = -1
        GOTO 999
      ENDIF
C
C ****  Now call lower level routine to actually return the data
      CALL GHGCAH(LGCAH,ITRA,VERT,P,IDATA,TOTAL,NPOINT)
  999 RETURN
      END
