C----------------------------------------------------------------------
C- MTC_BESTRKS.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   1-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      COMMON /MTC_BESTRKS/ nmtc_tracks,
     &  chitbest(5), chiebest(5), fcntbest(5), icntbest(5), ihitbest(5),
     &  fcnthbest(5), icnthbest(5), ihithbest(5),
     &  fcntghbest(5), icntghbest(5), ihitghbest(5),
     &  ilbest(5,18), iebest(5,18), ipbest(5,18),
     &  xpbest(5,18), ypbest(5,18), zpbest(5,18), ezbest(5,18),
     &  pntbest(5,3), cosbest(5,3),
     &  etabest(5), phibest(5),
     &  enetbest(5)

      INTEGER nmtc_tracks
      REAL    chitbest, chiebest, fcntbest, fcnthbest, fcntghbest
      INTEGER icntbest, ihitbest, 
     &        icnthbest, ihithbest, icntghbest, ihitghbest, 
     &        ilbest, iebest, ipbest
      REAL    xpbest, ypbest, zpbest, ezbest,
     &        pntbest, cosbest,
     &        etabest, phibest
      REAL    enetbest
C----------------------------------------------------------------------
C- COMMON /MTC_BESTRKS/ contains info for the best mu tracks found in the cal
C-
C- NMTC_TRACKS  - number of best tracks found (range I=1:5)
C- CHITBEST(I)  - track residual of Ith best track
C- CHIEBEST(I)  - energy chi2 of Ith best track
C- ENETBEST(I)  - cal energy associated with Ith best track
C- 
C- FCNTBEST(I)  - fraction of layers hit of Ith best track
C- ICNTBEST(I)  - max number of layers traversed of Ith track
C- IHITBEST(I)  - number of layers hit by Ith track
C- 
C- FCNTHBEST(I)  - hadronic fraction of layers hit of Ith best track
C- ICNTHBEST(I)  - hadronic max number of layers traversed of Ith track
C- IHITHBEST(I)  - hadronic number of layers hit by Ith track
C- 
C- FCNTGHBEST(I)  - gap/hadronic fraction of layers hit of Ith best track
C- ICNTGHBEST(I)  - gap/hadronic max number of layers traversed of Ith track
C- IHITGHBEST(I)  - gap/hadronic number of layers hit by Ith track
C-
C- track fit points (cells) utilized to get Ith track
C-   ILBEST(I,J=1:IHITBEST(I)+1) - layer number of J th hit of Ith track
C-   IEBEST(I,J) - relative ieta of Jth hit of Ith track
C-   IPBEST(I,J) - relative iphi of Jth hit of Ith track
C-   XPBEST(I,J) - x position of Jth hit of Ith track
C-   YPBEST(I,J) - y position of Jth hit of Ith track
C-   ZPBEST(I,J) - z position of Jth hit of Ith track
C-   EZBEST(I,J) - uncertainty in z position of Jth hit of Ith track
C-
C- fit results:
C-   PNTBEST(I,3) - x,y,z coord of point on Ith fit line
C-   COSBEST(I,3) - direction cosines of Ith fit line
C-   ETABEST(I)   - eta of ith fit line
C-   PHIBEST(I)   - phi of ith fit line
C----------------------------------------------------------------------
