      SUBROUTINE GTSATR(JDIR,JTRK,IFLG,PT,THT,PHI,XYZI,XYZO,NHA,NHBC,
     & CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE,NTA,NTBC,L2F1,L2F2,BDL,
     A NHIT,IHIT,IGEO,IDIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get information for one track from SATN or SATS
C-
C-   Inputs :  JDIR  - Direction (1=north, 2=south)
C-             JTRK  - Track number in bank
C-
C-   Outputs : IFLG  - Track flag (-999 = track not found)
C-             PT    - Momentum
C-             THT   - Theta from vertex
C-             PHI   - Phi from vertex
C-             XYZI(6) - 6-vector inside magnet
C-             XYZO(6) - 6-vector outside magnet
C-             NHA   - Number of hits inside magnet
C-             NHBC  - Number of hits outside magnet
C-             CHI   - Chi 2 inside magnet
C-             CHO   - Chi 2 outside magnet
C-             DTRK  - Distance between tracks at magnet
C-             PLAN  - Planarity
C-             DVTX  - Distance to vertex
C-             ECAL  - Energy loss in calorimeter
C-             EFE   - Energy loss in magnet
C-             NTA   - Number of tubes on track in a-layer
C-             NTBC  - Number of tubes on track in b/c-layer
C-             L2F1  - L2 Flag word 1
C-             L2F2  - L2 Flag word 2
C-             BDL   - Magnetic field
C-             NHIT  - Total number of hits
C-             IHIT(40) - Tube address of hit
C-             IGEO(40) - Geometry address of hit
C-             IDIS(40) - Drift distance of hit (*10E5)
C-
C-   Created : 2-MAY-1994   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LSAMT,GZSAMT,LSATR,ISATR,JSATR,I
      INTEGER JDIR,JTRK,IFLG,NHA,NHBC,NTA,NTBC,L2F1,L2F2
      REAL PT,THT,PHI,XYZI(6),XYZO(6),CHI,CHO,DTRK,PLAN,DVTX
      REAL ECAL,EFE,BDL
      INTEGER NHIT,IHIT(40),IGEO(40),IDIS(40)
      EXTERNAL GZSAMT
C
      IFLG = -999
      LSAMT = GZSAMT()
      IF (LSAMT.LE.0) RETURN
      LSATR = LQ(LSAMT-JDIR)
      IF (LSATR.LE.0) RETURN
      IF (JTRK.GT.IQ(LSATR+1)) RETURN
      ISATR = LSATR + 150*(JTRK-1) + 1
C
      IFLG = IQ(ISATR+1)
      PT   = Q(ISATR+2)
      THT  = Q(ISATR+3)
      PHI  = Q(ISATR+4)
      DO I = 1,6
        XYZI(I) = Q(ISATR+4+I)
        XYZO(I) = Q(ISATR+10+I)
      END DO
      NHA  = IQ(ISATR+17)
      NHBC = IQ(ISATR+18)
      CHI  = Q(ISATR+19)
      CHO  = Q(ISATR+20)
      DTRK = Q(ISATR+21)
      PLAN = Q(ISATR+22)
      DVTX = Q(ISATR+23)
      ECAL = Q(ISATR+24)
      EFE  = Q(ISATR+25)
      NTA  = IQ(ISATR+26)
      NTBC = IQ(ISATR+27)
      L2F1 = IQ(ISATR+28)
      L2F2 = IQ(ISATR+29)
      BDL  = Q(ISATR+149)
      NHIT = IQ(ISATR+30)
      DO I = 1,NHIT
        JSATR = ISATR+30+(I-1)*3
        IHIT(I) = IQ(JSATR+1)
        IGEO(I) = IQ(JSATR+2)
        IDIS(I) = IQ(JSATR+3)
      END DO
C
      RETURN
      END
