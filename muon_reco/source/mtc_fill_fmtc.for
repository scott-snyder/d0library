      SUBROUTINE MTC_FILL_FMTC
C----------------------------------------------------------------------
C- MTC_FILL_FMTC: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Fill the MTC.INC common block with best
C-      calorimeter track/pattern recognition information
C-      - FAST VERSION contains energy sums rather than energy chi2 -
C-
C-   Inputs  : many blocks
C-   Outputs : /mtc/
C-
C-   Created   1-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input blocks
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_E5TOWERS.INC'
      INCLUDE 'D0$INC:MTC_BESTRKS.INC'
C- output block
      INCLUDE 'D0$INC:MTC.INC'
C- used for track fitting
      REAL XPNTS(18),YPNTS(18),ZPNTS(18),EZPNTS(18)
      REAL POINT(3),COSDIR(3),CHICALIN
C- local
      INTEGER IXYZ,ILAYERS,NLAYERS, ICOUNT, ILYR
      REAL ESUM
C----------------------------------------------------------------------
      IF(NMTC_TRACKS.NE.0) THEN
C- best track in /mtc_bestrks/ is indexed number 1
        XMTC_ECHI   = CHIEBEST(1)
        XMTC_ETRACK = ENETBEST(1)
C- total fraction, cell count
        IMTC_MAX    = ICNTBEST(1)
        XMTC_FRACT  = FCNTBEST(1)
C- hadronic fraction, cell count
        IMTC_HMAX  = ICNTHBEST(1)
        XMTC_HFRACT = FCNTHBEST(1)
C- massless gap/icd/hadronic fraction, cell count
        IMTC_GHMAX  = ICNTGHBEST(1)
        XMTC_GHFRACT = FCNTGHBEST(1)
C- fit results including the vertex
        DO 10 IXYZ=1,3
          XMTC_DIRCOS_V(IXYZ) = COSBEST(1,IXYZ)
          XMTC_POINT_V(IXYZ)  = PNTBEST(1,IXYZ)
   10   CONTINUE
        XMTC_TRES_V   = CHITBEST(1)
C- get best track fit NOT including the vertex
C- through final NLAYERS-1 points (skip the vertex at ilayer=1) ...
        NLAYERS = IHITBEST(1)+1
        DO 20 ILAYERS=2,NLAYERS
          XPNTS(ILAYERS-1) = XPBEST(1,ILAYERS)
          YPNTS(ILAYERS-1) = YPBEST(1,ILAYERS)
          ZPNTS(ILAYERS-1) = ZPBEST(1,ILAYERS)
          EZPNTS(ILAYERS-1) = EZBEST(1,ILAYERS)
   20   CONTINUE
        NLAYERS = NLAYERS -1
        CALL MTC_LINEPNTDIR(NLAYERS,XPNTS,YPNTS,ZPNTS,EZPNTS,
     &                      point,cosdir,chicalin)
        XMTC_TRES = CHICALIN
        DO 902 IXYZ=1,3
          XMTC_DIRCOS(IXYZ) = COSDIR(IXYZ)
          XMTC_POINT(IXYZ)  = POINT(IXYZ)
  902   CONTINUE

c- no energy chi2's available when running this fast version
        IMTC_LYRMU = -50
        XMTC_ECHI2 = -50.

      ELSE

C- no track found ...
        XMTC_ECHI    = -50.
        XMTC_FRACT   = -50.
        XMTC_HFRACT  = -50.
        XMTC_GHFRACT = -50.
        IMTC_MAX     = -50
        IMTC_HMAX    = -50
        IMTC_GHMAX   = -50
        DO 100 IXYZ=1,3
          XMTC_DIRCOS_V(IXYZ) = -50.
          XMTC_POINT_V(IXYZ)  = -50.
  100   CONTINUE
        XMTC_TRES_V = -50.
        XMTC_TRES   = -50.
        DO 904 IXYZ=1,3
          XMTC_DIRCOS(IXYZ) = -50.
          XMTC_POINT(IXYZ)  = -50.
  904   CONTINUE
        IMTC_LYRMU = -50
        XMTC_ECHI2 = -50.
      END IF

  777 CONTINUE

C- calorimetric measure of total energy, punchthrough ...
      XMTC_EN5         = ESUM5(18)
      XMTC_EN3         = ESUM3(18)
      XMTC_EFRACT_H(1) = 0.
      XMTC_EFRACT_H(2) = 0.
      XMTC_EFRACT_H(3) = 0.
      ICOUNT = 0
      ESUM = 0.
      DO 200 ILYR=17,11,-1
        IF(ESUM3(ILYR).GE.0.) THEN
          ICOUNT = ICOUNT + 1
          IF(icount.GE.4) go to 201
          ESUM = ESUM + ESUM3(ILYR)
          IF(ESUM3(18).NE.0.) THEN
            XMTC_EFRACT_H(ICOUNT) = ESUM / ESUM3(18)
          ELSE
            XMTC_EFRACT_H(ICOUNT) = 0.
          END IF
        END IF
  200 CONTINUE
  201 CONTINUE

C- calorimetric measure of MIP-like track isolation ...
C- no energy chi2's, fractions available when running this fast version
      IF(XMTC_ECHI_33.NE.-50.) then
        XMTC_ECHI_33  = -50.
        XMTC_FRACT_33 = -50.
        XMTC_FRACH_33 = -50.
        XMTC_ECHI_55  = -50.
        XMTC_FRACT_55 = -50.
        XMTC_FRACH_55 = -50.
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
