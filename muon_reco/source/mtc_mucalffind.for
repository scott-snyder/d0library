      SUBROUTINE MTC_MUCALFFIND()
C----------------------------------------------------------------------
C- MTC_MUCALFFIND: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods :  Find tracklike objects in the calorimeter.
C-   This routine is called by MTCEVT(), which stores the needed input
C-   parameters in MTC_FIND.
C-
C-   Inputs  : some /MTC_FIND/ parameters:
C-             XMTC_VTXFND(3)  - vertex position
C-             XMTC_DVTXFND(3) - uncertainty in event vertex position
C-             XMTC_ETAMIN,
C-             XMTC_ETAMAX - eta range in which to look for tracks
C-                           (etamin.le.abs(eta).le.etamax)
C-             XMTC_HFRACMIN - minimum fraction of hadronic layers
C-                           required for scanned tracks
C-             IMTC_ISTATUS  - 0 if you want status messages turned on
C-   Outputs : other /MTC_FIND/ parameters namely the
C-             IMTC_ISTATUS = 0 if routine was successful
C-                          = -1 if routine failed
C-                          = +1 if too many tracks were found
C-             ETA,PHI etc of tracklike objects found by this program
C-
C-   Created  20-FEB-1994   Elizabeth Gallas
C-   Modified 17-APR-1995   Elizabeth Gallas - 
C-      If caep does not exist, then create it from caeq.
C-      This change will enable MTC to be run on run 1b dsts.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c- input, output
      INCLUDE 'd0$inc:mtc_find.inc'
C- blocks used
      INCLUDE 'd0$inc:mtc.inc'
      INCLUDE 'd0$inc:mtc_scan.inc'
C- 5 best tracks from mtc_mucalftrack (for energy sum or energy chi2)
      INCLUDE 'd0$inc:mtc_bestrks.inc'
C- local
      INTEGER itracks,imtc,itfnd, ixyz
      REAL    eta,phi,vtx(3),dvtx(3)
      INTEGER lcaep, gzcaep, lcaeq, gzcaeq
C----------------------------------------------------------------------
C- initialize the error flag, it will be reset if something goes wrong
      IMTC_IERROR = 0
C----------------------------------------------------------------------
C- if caep exists, use it;  if caeq but no caep, create caep from caeq.
      lcaeq = gzcaeq()
      lcaep = gzcaep()
      IF(  lcaeq.LE. 0 ) THEN
        IF(lcaep.LE.0) THEN
          WRITE(6,*) ' MTC_MUCALFFIND: error - no CAEP or CAEQ bank'
          imtc_nscan  =  0
          imtc_nfnd   =  0
          imtc_ierror = -1
          RETURN
        ENDIF
      ELSE
        IF(lcaep .LE. 0 ) CALL caeq_to_caep
        lcaep = gzcaep()
      ENDIF
C----------------------------------------------------------------------
C- Scan for muons - fill the /MTC_SCAN/ block
      CALL mtc_mucalscan
C----------------------------------------------------------------------
C- initialize /MTC_FIND/
      imtc_nfnd = 0
C----------------------------------------------------------------------
C- put the vertex into a local array
      DO 9 ixyz=1,3
        vtx(ixyz)  = xmtc_vtxfnd(ixyz)
        dvtx(ixyz) = xmtc_dvtxfnd(ixyz)
    9 CONTINUE
C----------------------------------------------------------------------
C- Sort muons found in scan ... get better tracks, eliminate duplicates
C- see if no tracks found
      IF(imtc_nscan.EQ.0) THEN
        imtc_nfnd   = 0
        RETURN
      END IF

C- call mucaltrack for tracks found in scan array
C- if no track was found, go to next track in scan array
      DO 10 itracks=1,imtc_nscan
        eta = xmtc_etascan(itracks)
        phi = xmtc_phiscan(itracks)
        CALL mtc_mucalftrack(vtx,dvtx,eta,phi)
        IF(imtc_hmax.LT.0) go to 10
        CALL mtc_cosetaphi(xmtc_dircos_v,eta,phi)

CC        IF(abs(eta).LT.3.0) THEN
CCC- call mucaltrack again if le 2 hadronic layers were utilized for best track
CC          IF(imtc_hmax.LE.2 .AND. imtc_hmax.GE.0) THEN
CC            CALL mtc_mucalftrack(vtx,dvtx,eta,phi)
CC            IF(imtc_hmax.LT.0) go to 10
CC            CALL mtc_cosetaphi(xmtc_dircos_v,eta,phi)
CC          END IF
CCC- get best track info by calling mucaltrack one more time
CC          CALL mtc_mucalftrack(vtx,dvtx,eta,phi)
CC          IF(imtc_hmax.LT.0) go to 10
CC          CALL mtc_cosetaphi(xmtc_dircos_v,eta,phi)
CC        END IF

C- if no tracks was found, go to next track in scan array
        IF(imtc_hmax.LT.0) go to 10
C- these are the good mucaltrack requirements
        IF(xmtc_hfract.GE.xmtc_hfracfnd .AND. 
     &     xmtc_fract.GE.xmtc_fracfnd .AND. imtc_hmax.GT.1 ) THEN
C- Cal track passes cuts, add it to /MTC_FIND/ with better eta,phi
          imtc_nfnd = imtc_nfnd + 1
          xmtc_frafnd(imtc_nfnd) = xmtc_fract
          xmtc_hfrafnd(imtc_nfnd) = xmtc_hfract
          xmtc_tresvfnd(imtc_nfnd) = xmtc_tres_v
          xmtc_etafnd(imtc_nfnd) = eta
          xmtc_phifnd(imtc_nfnd) = phi
          xmtc_enefnd(imtc_nfnd) = chiebest(1)
c- compare the eta,phi of current track to previously found tracks,
C- if delta(eta) and delta(phi) within .25, then only add better track
          IF(imtc_nfnd.EQ.1) go to 72
          DO 75 itfnd=imtc_nfnd-1,1,-1
            imtc = imtc_nfnd
            IF(abs(xmtc_etafnd(imtc)-xmtc_etafnd(itfnd)).LT.0.25 .AND.
     &        (abs(xmtc_phifnd(imtc)-xmtc_phifnd(itfnd)).LT.0.25 .OR.
     &        abs(xmtc_phifnd(imtc)-xmtc_phifnd(itfnd)).GT.6.15 ) )
     &        THEN
C- best track has higher had fraction, higher fraction, lower track res
              IF(xmtc_hfrafnd(imtc) .GT.xmtc_hfrafnd(itfnd).OR.
     &          (xmtc_hfrafnd(imtc) .EQ.xmtc_hfrafnd(itfnd).AND.
     &          xmtc_frafnd(imtc)  .GT.xmtc_frafnd(itfnd)) .OR.
     &          (xmtc_hfrafnd(imtc) .EQ.xmtc_hfrafnd(itfnd).AND.
     &          xmtc_frafnd(imtc)  .EQ.xmtc_frafnd(itfnd) .AND.
     &          xmtc_tresvfnd(imtc).LT.xmtc_tresvfnd(itfnd)))THEN

                xmtc_hfrafnd(itfnd)  = xmtc_hfrafnd(imtc)
                xmtc_frafnd(itfnd)   = xmtc_frafnd(imtc)
                xmtc_tresvfnd(itfnd) = xmtc_tresvfnd(imtc)
                xmtc_etafnd(itfnd)   = xmtc_etafnd(imtc)
                xmtc_phifnd(itfnd)   = xmtc_phifnd(imtc)
                xmtc_enefnd(itfnd)   = xmtc_enefnd(imtc)
                imtc_nfnd = imtc_nfnd - 1
              ELSE
                imtc_nfnd = imtc_nfnd - 1
              END IF
            END IF
   75     CONTINUE
   72     CONTINUE
        ELSE
C- Cal track failed MTC criteria

        END IF
   10 CONTINUE                          ! loop over imtc_nscan cal tracks
C----------------------------------------------------------------------
  999 RETURN
      END
