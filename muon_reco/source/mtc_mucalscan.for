      SUBROUTINE MTC_MUCALSCAN
C----------------------------------------------------------------------
C- MTC_MUCALSCAN: part of the MTC package
C-
C-   Purpose and Methods : Scan the calorimeter for tracklike objects.
C-
C-   Inputs  : /MTC_FIND/ input cuts/requirements.
C-   Outputs : /MTC_SCAN/ - a block containing preliminary candidate
C-      muon tracks found in the calorimeter in the abs(eta) range
C-      from XMTC_ETAMIN to XMTC_ETAMAX.  The tracks must have a
C-      hadronic fraction of layers hit gt fracmin.
C-      Fracmin is set initially to XMTC_HFRACSCAN.
C-      A maximum of IMTC_NSCAN tracks can be stored -
C-      if neccessary, fracmin increases for events with a lot of
C-      tracks found so that only the best tracks are stored.
C-
C-   Created  20-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      INCLUDE 'd0$inc:mtc_find.inc'
C- output
      INCLUDE 'd0$inc:mtc_scan.inc'
C- local
      INTEGER ieta,iphi,ilyr, ihere,ier_caep, ok,num_trav,itracks,itlast
      REAL    point1(3), point2(3), cosdir(3), frac_hit,eta,phi, x,y,z,
     &        energy
      LOGICAL lfound
C- fnctions
      INTEGER MTC_IWHERE
C- keeping track of layers hit
      INTEGER imtc
C- frac of lyrs hit 4/7,3/5,5/8,2/3,5/7, 3/4,4/5,5/6,6/7,7/8, all
      INTEGER nfracs,ifracs
      REAL    fracmin,fracs(11)
      DATA    fracs/.57,.60,.62,.66,.71, .75,.80,.83,.85,.87, 1.0/
C----------------------------------------------------------------------
      imtc_nscan = 0
C----------------------------------------------------------------------
C- set the min had scan frac and set ifracs appropriately so that it knows
C- what the next fracmin is so that if too many tracks are found, fracmin
C- can be increased to the next level and only the best tracks are kept
      ifracs = 11
      fracmin = xmtc_hfracscan
      DO 8 nfracs=10,0,-1
        IF(xmtc_hfracscan.GE.fracs(nfracs+1)) go to 9
        ifracs = nfracs
    8 CONTINUE
    9 CONTINUE
C----------------------------------------------------------------------
      point1(1) = xmtc_vtxfnd(1)
      point1(2) = xmtc_vtxfnd(2)
      point1(3) = xmtc_vtxfnd(3)
C- Loop over phi
      DO 10 iphi=1,64
C- Loop over eta
        DO 20 ieta=-37,37
          lfound = .false.
c- Loop over layers
          DO 30 ilyr=17,11,-1
            IF(lfound) go to 20
            ihere = MTC_IWHERE(IETA,IPHI,ILYR)
            IF(ihere.EQ.0) go to 30

            CALL GTCAEP_ADDR(IETA,IPHI,ILYR, energy,ier_caep)
            IF(energy.EQ.0.) ier_caep = -5
            IF(ier_caep.NE.0 .AND. ier_caep.NE.-5) go to 30
C- Increment number of layers found
            IF(energy.EQ.0) go to 30
C- Seed cell found at this eta,phi
            lfound = .true.
C- Get the x,y,z coord of cell center
            CALL CELXYZ(IETA, IPHI, ILYR, x, y, z, ok)
            IF(ok.NE.0) WRITE(6,89) ieta,iphi,ilyr
C- Get the direction cosines of a line from the vtx to the seed cell ...
            point2(1) = x
            point2(2) = y
            point2(3) = z
            CALL MTC_PNTTOCOS(POINT1,POINT2, cosdir)
C- See if this track is within the eta range specified
            CALL MTC_COSETAPHI(COSDIR, eta,phi)
            IF(abs(eta).LT.XMTC_ETAMIN .OR.
     &         abs(eta).GT.XMTC_ETAMAX ) go to 30

C- Get the fraction of hadronic layers hit along this line
            CALL MTC_FRACHIT(POINT1,COSDIR, frac_hit, num_trav)

C- add another track to the /MTC_SCAN/ block
            IF(frac_hit.GE.fracmin) THEN

              imtc_nscan = imtc_nscan + 1
              xmtc_frascan(imtc_nscan) = frac_hit
              imtc_nctscan(imtc_nscan) = num_trav
              xmtc_etascan(imtc_nscan) = eta
              xmtc_phiscan(imtc_nscan) = phi

C- compare the eta,phi of current track to previously found tracks,
C- if delta(eta) and delta(phi) within .25, then only add better track
              IF(imtc_nscan.EQ.1) go to 72
              DO 75 itracks=imtc_nscan-1,1,-1
                imtc = imtc_nscan
                IF(abs(xmtc_etascan(imtc)-xmtc_etascan(itracks))
     &            .LT.0.25 .AND. (
     &            abs(xmtc_phiscan(imtc)-xmtc_phiscan(itracks))
     &            .LT.0.25 .OR.
     &            abs(xmtc_phiscan(imtc)-xmtc_phiscan(itracks))
     &            .GT.6.15 ) ) THEN
                  IF(xmtc_frascan(imtc).GT.xmtc_frascan(itracks).OR.
     &              (xmtc_frascan(imtc).EQ.xmtc_frascan(itracks).AND.
     &              imtc_nctscan(imtc).GT.imtc_nctscan(itracks)))THEN
                    xmtc_frascan(itracks) = xmtc_frascan(imtc)
                    imtc_nctscan(itracks) = imtc_nctscan(imtc)
                    xmtc_etascan(itracks) = xmtc_etascan(imtc)
                    xmtc_phiscan(itracks) = xmtc_phiscan(imtc)
                    imtc_nscan = imtc_nscan - 1
                  ELSE
                    imtc_nscan = imtc_nscan - 1
                  END IF
                END IF
   75         CONTINUE

C- if too many tracks, increase fracmin and write over worst tracks
   72         CONTINUE
              IF(imtc_nscan.EQ.imtc_nstmax) THEN
                IF(ifracs.EQ.11) THEN
                  IF(imtc_ipstatus.EQ.0) WRITE(6,*)
     &              ' MTC_MUCALSCAN: OVER ',IMTC_NSTMAX,' TRACKS !!! '
                  IMTC_IERROR = +1
                  RETURN
                END IF
                ifracs = ifracs + 1
                fracmin = fracs(ifracs)
                IF(imtc_ipstatus.EQ.0) WRITE(6,*)
     &              ' MTC_MUCALSCAN: Increase fracmin to ',fracmin
                itlast = 0
                DO 70 itracks=1,imtc_nscan
                  IF(xmtc_frascan(itracks).GE.fracmin) THEN
                    itlast = itlast + 1
                    xmtc_frascan(itlast) = xmtc_frascan(itracks)
                    imtc_nctscan(itlast) = imtc_nctscan(itracks)
                    xmtc_etascan(itlast) = xmtc_etascan(itracks)
                    xmtc_phiscan(itlast) = xmtc_phiscan(itracks)
                  END IF
   70           CONTINUE
                imtc_nscan = itlast
                go to 72
              END IF                  ! end of fracmin track elimination
            END IF                      ! finished adding this track
   30     CONTINUE                      ! loop over layer
   20   CONTINUE                      ! loop over eta
   10 CONTINUE                      ! loop over phi
C----------------------------------------------------------------------
   87 FORMAT(' MTC_MUCALSCAN: error calling gtcaep for ie,ip,il=',3i5)
   88 FORMAT(' MTC_MUCALSCAN: clinph failure !!!',2i10)
   89 FORMAT(' MTC_MUCALSCAN: error w/x,y,z ie,ip,il',3I4)
  999 RETURN
      END
