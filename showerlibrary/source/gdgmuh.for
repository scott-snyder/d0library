      SUBROUTINE GDGMUH(LGMUH,ITRA,VERT,PP,HTDATA,NIDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds GMUH bank for next track in GMUH bank
C-
C-   Inputs  : LGMUH     = Link to GMUH bank
C-   Outputs : ITRA      = Track actually found (-1) if no more available
C-             VERT(3,3)
C-                 (i,1) = X,Y,Z of track vertex
C-                 (i,2) = X,Y,Z at EXIT FROM Calorimeter
C-             PP(4,2)
C-                 (i,1) = ISAJET track 4-momentum
C-                 (i,2) = track 4-momentum at EXIT FROM Calorimeter
C-            HTDATA(1)  = Geant track type
C-                  (2)  = ISAJET particel momentum
C-                  (3)  = ISAJET vertex type
C-                  (4)  = ISAJET Vertex Z
C-                  (5)  = Geant track number of parent
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
C-                  (8)  = Total No. of Hits
C-                  (9- Nhits*24)
C-             NIDATA    = Number of  data entries
C-    CREATED 4-MAY-1993   Jasbir Singh
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:MULDAT.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C INPUT VARIABLE
      INTEGER LGMUH
C OUTPUT VARIABLES
      INTEGER ITRA
      REAL VERT(3,3)
      REAL PP(4,2),F,RNDM,X
      REAL HTDATA(NMULIB)
      INTEGER SSUNIT,MOD_NHITS
C Zebra variables
C LOCAL VARIABLES
      INTEGER I,NIDATA,VERSION,NHEADER,NREPEAT,NSKIP,KSKIP
C----------------------------------------------------------------------
      IF ( LGMUH.EQ.0 ) THEN
        ITRA = -1                       ! NO MORE TRACKS
        GO TO 999
      ENDIF
      ITRA = IQ(LGMUH-5)                ! TRACK NUMBER
      NHEADER = IQ(LGMUH+2)
      NREPEAT = IQ(LGMUH+3)
      CALL VZERO(VERT,3*3)
      CALL VZERO(PP,4*2)
      HTDATA(8) = 0
      MOD_NHITS = 0
      NSKIP = 3
C
      DO I = 1,3                    ! Vertex position
        VERT(I,1) = Q(LGMUH+I+NSKIP)
      ENDDO
      DO I = 1,4                    ! Track momentum
        PP(I,1) = Q(LGMUH+I+3+NSKIP)
      ENDDO
      DO I = 1,7                    ! Geant information
        HTDATA(I) = IQ(LGMUH+I+7+NSKIP)
      ENDDO

      DO I =  1, 3
        VERT(I,2) = Q(LGMUH+I+17)     ! Position at CAL EXIT
        HTDATA(7+I) = Q(LGMUH+I+17)
      ENDDO

      DO I =  1, 4
        PP(I,2) = Q(LGMUH+I+20)        ! Momentum at CAL EXIT
        HTDATA(10+I) = Q(LGMUH+I+20)
      ENDDO
      NHITS = IQ(LGMUH+25)    ! Number of HITS
      IF(NHITS .LE. 0)THEN
        NIDATA = 0
        GO TO 999
      ENDIF
      NIDATA =  15
      DO I=1,NHITS
        IF(NHITS.GT.MAX_HITS) THEN ! START SQUEEZING HITS IF RUNNING OUT OF SPACE
          F = RNDM(0)
          X = FLOAT(MAX_HITS)/FLOAT(NHITS)
          IF ( F.GT.X) GO TO 990
        ENDIF
        MOD_NHITS = MOD_NHITS + 1
        CALL UCOPY(IQ(LGMUH+NIDATA+11),HTDATA(NIDATA+1),NREPEAT)
        NIDATA = NIDATA+NREPEAT
  990   CONTINUE
      ENDDO
  999 CONTINUE
      HTDATA(15) = MOD_NHITS
      RETURN
      END
