      SUBROUTINE GHGCAH(LGCAH,ITRA,VERT,P,IDATA,TOTAL,NPOINT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds GCAH bank for next track in GCAH bank
C-
C-   Inputs  : LGCAH     = Link to GCAH bank
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
C-   Created  30-JAN-1990   Rajendran Raja  Changed ITRAK meaning (mod GTGCAH)
C-   Updated  11-MAR-1991   Alan M. Jonckheere  Changed name to maintain the
C-                              original function of GTGCAH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C INPUT VARIABLE
      INTEGER LGCAH
C OUTPUT VARIABLES
      INTEGER ITRA
      REAL VERT(3,3),P(4,2),TOTAL(4)
      INTEGER IDATA(8),NPOINT
C Zebra variables
C LOCAL VARIABLES
      INTEGER I,NDATA,VERSION,NHEADER,NREPEAT,NSKIP
C----------------------------------------------------------------------
      IF ( LGCAH.EQ.0 ) THEN
        ITRA = -1                       ! NO MORE TRACKS
        GO TO 999
      ENDIF
C
C ****  determine version of GCAH (2 or above have differnet format from 1)
C
      ITRA = IQ(LGCAH-5)                ! TRACK NUMBER
      VERSION = IQ(LGCAH+1)
      IF ( VERSION.EQ.2 ) THEN
        NSKIP = 3
        NHEADER = IQ(LGCAH+2)
        NREPEAT = IQ(LGCAH+3)
      ELSE
        NSKIP = 0
        NHEADER = 19
        NREPEAT = 2
        CALL VZERO(VERT,3*3)
        CALL VZERO(P,4*2)
        IDATA(8) = 0
      END IF
C
      DO I = 1,3                    ! Vertex position
        VERT(I,1) = Q(LGCAH+I+NSKIP)
      ENDDO
      DO I = 1,4                    ! Track momentum
        P(I,1) = Q(LGCAH+I+3+NSKIP)
      ENDDO
      DO I = 1,7                    ! Geant information
        IDATA(I) = IQ(LGCAH+I+7+NSKIP)
      ENDDO
      IF ( VERSION.EQ.2 ) THEN
        DO I = 1,4
          TOTAL(I) = Q(LGCAH+I+17)      ! Totals
        ENDDO
        NPOINT = IQ(LGCAH+NHEADER)    ! Number of cells filled (.GE. V0.11)
        DO I =  1, 4
          P(I,2) = Q(LGCAH+I+21)        ! Momentum at CAL entry
        ENDDO
        DO I =  1, 3
          VERT(I,2) = Q(LGCAH+I+25)     ! Position at CAL entry
        ENDDO
        DO I =  1, 3
          VERT(I,3) = Q(LGCAH+I+28)     ! Position at 1st CAL interaction
        ENDDO
        IDATA(8) = IQ(LGCAH+32)
      ELSE
        NDATA = IQ(LGCAH - 1)             ! Length of data
        IF ( NDATA.GT.19) THEN            ! Probably returns garbage for
C                                         ! versions of D0Geant before V0.11
          DO I = 1,4
            TOTAL(I) = Q(LGCAH+I+14+NSKIP)
          ENDDO
          NPOINT = IQ(LGCAH+NHEADER)    ! Number of cells filled (.GE. V0.11)
        ELSE
          CALL VZERO(TOTAL,4)
          NPOINT = 0
        ENDIF
      ENDIF
  999 RETURN
      END
