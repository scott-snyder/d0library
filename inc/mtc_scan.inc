C----------------------------------------------------------------------
C- MTC_SCAN: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created  20-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      INTEGER imtc_nstmax
      PARAMETER (imtc_nstmax=200)
      COMMON /MTC_SCAN/ imtc_nscan,
     &        imtc_nctscan(imtc_nstmax),
     &        xmtc_frascan(imtc_nstmax),
     &        xmtc_etascan(imtc_nstmax),xmtc_phiscan(imtc_nstmax)
      INTEGER imtc_nscan,imtc_nctscan
      REAL    xmtc_frascan,xmtc_etascan,xmtc_phiscan
C----------------------------------------------------------------------
C- block MTC_SCAN contains the eta,phi,etc for tracks found in the
C- initial scan of the calorimeter for track-like objects by routine
C- MTC_MUCALSCAN.  Up to IMTC_NSTMAX tracks can be stored.
C-
C- IMTC_NSCAN - the number of tracks found in the scan
C- XMTC_ETASCAN(i) - eta of ith track
C- XMTC_PHISCAN(i) - phi of ith track
C- XMTC_FRASCAN(i) - fraction of hadronic layers hit of ith track
C- IMTC_NCTSCAN(i) - num of had lyrs possible to hit along ith track
C----------------------------------------------------------------------
