      SUBROUTINE MTC_FILL_MTC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the MTC.INC common block with best
C-      calorimeter track/pattern recognition information
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
      INCLUDE 'D0$INC:MTC_EHTOWERS.INC'
      INCLUDE 'D0$INC:MTC_BESTRKS.INC'
C- output block
      INCLUDE 'D0$INC:MTC.INC'
C- used for track fitting
      REAL XPNTS(18),YPNTS(18),ZPNTS(18),EZPNTS(18)
      REAL POINT(3),COSDIR(3),CHICALIN
C- local
      INTEGER IXYZ,ILAYERS,NLAYERS
      INTEGER ICOUNT
      REAL ESUM
      INTEGER ILYR,IE2,IP2
      REAL CHITOT,CHIAVG,CHILAST
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

C- Step toward the vertex along the track,
C- find the layer where the avg energy chi2 rises above 5.0
        CHITOT = 0.
        CHIAVG = 0.
        CHILAST = 0.
        NLAYERS = IHITBEST(1) + 1
        DO 30 ILAYERS=2,NLAYERS
          ILYR = ILBEST(1,ILAYERS)
          IE2  = IEBEST(1,ILAYERS)
          IP2  = IPBEST(1,ILAYERS)
          CHITOT = CHITOT + CHITOWER(IE2,IP2,ILYR)
          CHIAVG = CHITOT / FLOAT(ILAYERS-1)
          IF(CHIAVG.GT.5.0) THEN
            IF(ILAYERS.GE.3) THEN
              IMTC_LYRMU = ILBEST(1,ILAYERS-1)
              XMTC_ECHI2  = CHILAST
            ELSE
              IMTC_LYRMU = 18
              XMTC_ECHI2  = CHITOWER(IE2,IP2,ILYR)
            END IF
            GO TO 777
          ELSE
            CHILAST = CHIAVG
            IMTC_LYRMU = ILYR
            XMTC_ECHI2 = CHILAST
          END IF
C- if we find the last layer, then set IMTC_LYRMU = 0
          IF(IMTC_LYRMU.EQ.ILBEST(1,NLAYERS)) IMTC_LYRMU = 0
   30   CONTINUE

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
          IF(icount.ge.4) go to 201
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
      XMTC_ECHI_33  = CHIT3(18)
      XMTC_FRACT_33 = FCNTCHI3
      XMTC_FRACH_33 = FCNTCH3
      XMTC_ECHI_55  = CHIT5(18)
      XMTC_FRACT_55 = FCNTCHI5
      XMTC_FRACH_55 = FCNTCH5
C----------------------------------------------------------------------
  999 RETURN
      END
