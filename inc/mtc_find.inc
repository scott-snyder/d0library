C----------------------------------------------------------------------
C- MTC_FIND: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created  20-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      INTEGER imtc_tracks
      PARAMETER (imtc_tracks=200)
      COMMON /MTC_FIND/ imtc_full, 
     &        imtc_nfnd, imtc_ipstatus, imtc_ierror,
     &        xmtc_vtxfnd(3), xmtc_dvtxfnd(3),
     &        xmtc_etamin, xmtc_etamax, 
     &        xmtc_hfracscan,xmtc_hfracfnd,xmtc_fracfnd,
     &        xmtc_tresvfnd(imtc_tracks),
     &        xmtc_frafnd(imtc_tracks),xmtc_hfrafnd(imtc_tracks),
     &        xmtc_etafnd(imtc_tracks),xmtc_phifnd(imtc_tracks),
     &        xmtc_enefnd(imtc_tracks)
      INTEGER imtc_full, imtc_nfnd, imtc_ipstatus, imtc_ierror
      REAL    xmtc_vtxfnd, xmtc_dvtxfnd,
     &        xmtc_etamin, xmtc_etamax, 
     &        xmtc_hfracscan,xmtc_hfracfnd,xmtc_fracfnd,
     &        xmtc_tresvfnd,xmtc_frafnd,xmtc_hfrafnd,
     &        xmtc_etafnd,xmtc_phifnd,
     &        xmtc_enefnd
C----------------------------------------------------------------------
C- block MTC_FIND stores the eta,phi,other track information for
C- IMTC_NFND (max=IMTC_TRACKS=200) tracks found by MTC_MUCALFFIND if
C- IMTC_FULL is -1 (FULL_MTC=.TRUE. in MTC.RCP)
C-
C- cal track finding input information read from MTC.RCP
C- XMTC_ETAMIN    - min and max specify eta range
C- XMTC_ETAMAX      in which to look for cal tracks
C- XMTC_HFRACSCAN - minimum fraction of hadronic layers hit required in
C-                  initial mtc track scanning
C- XMTC_HFRACFND  - min frac of had layers hit required in mtc track finding
C- XMTC_FRACFND   - min frac of layers hit required in mtc track finding
C- IMTC_IPSTATUS  = 0 to print status messages
C-                = non0 not to print
C-
C- XMTC_VTXFND(3)  - event vertex position in x,y,z
C- XMTC_DVTXFND(3) - uncertainty in event vertex position
C-
C- cal track output information
C- IMTC_IERROR  =  0 if mtc track finding successful
C-              = -1 if track finding unsuccessful
C-              = +1 if too many tracks were found
C- IMTC_NFND - number of tracks found
C- XMTC_ETAFND(i) - eta of cal track fit (using the vertex in the fit)
C- XMTC_PHIFND(i) - phi of cal track fit (using the vertex in the fit)
C- XMTC_ENEFND(i) - energy associated with the ith track
C- XMTC_FRAFND(i) - fraction of layers utilized with non0 energy
C- XMTC_HFRAFND(i) - fract of hadronic layers used with non0 energy
C- XMTC_TRESVFND(i) - track residual of ith track
C----------------------------------------------------------------------
