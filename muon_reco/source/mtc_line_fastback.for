      SUBROUTINE MTC_LINE_FASTBACK(
     &  point, cosdir, chicalin, tenergy, flyrhit, icntall)
C----------------------------------------------------------------------
C- MTC_LINE_FASTBACK: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : - FAST VERSION of MTC_LINE_STEPBACK -
C-      Find the best track using the following method:
C-      Designate as a seed cell a hit calorimeter cell in one of two
C-      layers furthest from the input vertex.  Step down in layer number
C-      (down to ILYR=1), looking for a hit cell at/adjacent
C-      to the current eta,phi which minimizes the track chisquared for
C-      a line fit through that cell, the vertex, and the seed cell.
C-      Step down through remaining layers one layer at a time,
C-      adding hit cells minimizing the track chisquared in each layer
C-      to the set of track fit points.
C-      Skip layers only if there isn't an adjacent hit cell in the layer
C-      (within 1 nearest neighbor) to the hit used in the
C-      previous layer.
C-      The linear fit is weighted according to the uncertainties in the
C-      points used:  for the vertex, the standard uncertainty is used,
C-      for the cal points, uncertainty = half the extent of the cell in z
C-
C-   Inputs  : /MTC_ETOWERS/
C-   Outputs : POINT(3), COSDIR(3) - The output fitted line is defined by
C-              direction cosines COSDIR and an arbitrary  point
C-              POINT(3) through which the line passes.
C-      CHICALIN - the chisquared of the fit - equal to the sqrt
C-              of the sum of the squares of the perpendicular
C-              distance from the fitted line to the calorimeter
C-              cell points
C-      ENERGY   - the sum of the cell energies associated with the track
C-      FLYRHIT - the fraction of possible layers utilized to get the track
C-       If the input arguments are inappropriate:
C-              If there is no energy in the given tower, then
C-              POINT(IXYZ) = 0., COSDIR(IXYZ) = 0., CHICALIN = -50.
C-
C-   Created   18-JAN-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input ...
      INCLUDE 'D0$INC:mtc_etowers.inc'
      INCLUDE 'D0$INC:mtc_e5towers.inc'
C- output arguments contain track info for best track
      REAL POINT(3), COSDIR(3), CHICALIN, TENERGY, FLYRHIT
      INTEGER ICNTALL
C- output block contains info for 5 best tracks
      INCLUDE 'D0$INC:MTC_BESTRKS.INC'
C- local ...
      INTEGER ixyz, nlayers,ilayers, iemark,ipmark,
     &  iseed,nseed,mseed, ilseed1,ilseed2
      REAL chitemp, x,y,z, xpoints(18),ypoints(18),zpoints(18),
     &  xsave,ysave,zsave,esave
      INTEGER iepoint(18),ippoint(18),ilpoint(18), icnthall, icntghall
      REAL eta,phi
      INTEGER maxlayers
C- uncertainties ...
      REAL zlong, ezpoints(18)
C- save best seed track coord, ilyr ,ie,ip ...
      REAL xseed(25,18),yseed(25,18),zseed(25,18),ezseed(25,18)
      INTEGER ilseed(25,18),ieseed(25,18),ipseed(25,18)
      INTEGER ibest
C- save best seed track frac layers hit, track chi2, energy chi2, tot # lyrs
      REAL    chitseed(25), chieseed(25), enetseed(25), fcntseed(25)
      REAL    pntseed(25,3),cosseed(25,3),
     &  etaseed(25),phiseed(25)
      INTEGER ihitseed(25),icntseed(25)
C- hadronic sums, fractions of layers hit
      REAL    fcnthseed(25)
      INTEGER ihithseed(25),icnthseed(25)
      REAL    fcntghseed(25)
      INTEGER ihitghseed(25),icntghseed(25)

C- do loops and indexes ...
      INTEGER ilyr, jlyr, ie2,ip2, ilyr2, ie3,ip3,ie23,ip23
      LOGICAL lenergy
      INTEGER ok
C- functns
      LOGICAL mtc_lexist
C----------------------------------------------------------------------
C- initialize the # of seeds and number of found tracks ...
      iseed   = 0
      NMTC_TRACKS = 0
      lenergy = .FALSE.
C- Check for no energy in tower ...
      IF(esum5(18).LE.0.) go to 665

C- find the two uppermost hadronic layers with non0 energy
C- these are the possible seed layers ...
      ilseed1=0
      ilseed2=0
      DO 669 ilyr=17,11,-1
        IF(esum5(ilyr).GT.0.) THEN
          ilseed1 = ilyr
          IF(ilyr.EQ.11) go to 664
          DO 668 jlyr=ilseed1-1,11,-1
            IF(esum5(jlyr).GT.0.) THEN
              ilseed2 = jlyr
              go to 667
            END IF
  668     CONTINUE
          go to 664
        END IF
  669 CONTINUE
  664 CONTINUE
      IF(ilseed2.EQ.0) ilseed2 = ilseed1
      IF(ilseed1.EQ.0) go to 665
  667 CONTINUE

C- make the first point the vertex position ...
      nlayers = 1
      xpoints(nlayers) = vtxtwr(1)
      ypoints(nlayers) = vtxtwr(2)
      zpoints(nlayers) = vtxtwr(3)
      ezpoints(nlayers) = dvtxtwr(3)
C- loop over all possible seed cells ...
      DO 770 ilyr=17,ilseed2,-1
C- does the layers exist or if so, does it have zero energy ?
        IF(esum5(ilyr).LT.0.) go to 770
        IF(esum5(ilyr).EQ.0.) go to 770
C- it exists and it has energy, look for the seed cells ...
        DO 771 ie2=ittlo,itthi
          DO 772 ip2=ittlo,itthi
            IF(entower(ie2,ip2,ilyr).gt.0.) then
C- seed layer found, make sure it's not directly below another seed ...
              IF(ilyr.EQ.ilseed2 .AND. ilseed1.NE.ilseed2 .AND.
     &           entower(ie2,ip2,ilseed1).ne.0.) go to 772
C- seed layer found
              iseed = iseed + 1
C- hadronic sum
              icnthseed(iseed) = 0
              ihithseed(iseed) = 0
C- gap/hadronic sum
              icntghseed(iseed) = 0
              ihitghseed(iseed) = 0
C- how many cells with zero energy are above it?
              icntall = 0
              icnthall = 0
              icntghall = 0
              IF(ilyr.LT.17) THEN
                DO 773 ilyr2=ilyr+1,17
                  IF(entower(ie2,ip2,ilyr2).ge.0.) then
                    icntall = icntall + 1
                    IF(ilyr2.GE.11) icnthall = icnthall + 1
                    IF(ilyr2.GE.8) icntghall = icntghall + 1
                  END IF
  773           CONTINUE
              END IF
              icntseed(iseed) = icntall + 1
C- hadronic sum
              IF(ilyr.GE.11) THEN
                icnthseed(iseed) = icnthall + 1
                ihithseed(iseed) = ihithseed(iseed) + 1
              END IF
C- gap/hadronic sum
              IF(ilyr.GE.8) THEN
                icntghseed(iseed) = icntghall + 1
                ihitghseed(iseed) = ihitghseed(iseed) + 1
              END IF
C- initialize track chi2 for this seed ...
              chitseed(iseed)  = 1.e8
C- if no other hit cells are found for this seed, then CHITEMP will
C- still be equal to this value
              chitemp = 1.e7
C- get seed x,y,z position, uncertainty
              CALL mtc_get_xyz(ie2,ip2,ilyr, x,y,z, ok)
              IF(ok.NE.0)
     &          WRITE(6,89) ie2,ip2,ilyr,ietatower(ilyr),iphitower(ilyr)
              CALL mtc_get_longz(ie2,ip2,ilyr, zlong, ok)
              IF(ok.NE.0)
     &          WRITE(6,88) ie2,ip2,ilyr,ietatower(ilyr),iphitower(ilyr)

              nlayers = 2
              xpoints(nlayers) = x
              ypoints(nlayers) = y
              zpoints(nlayers) = z
              ezpoints(nlayers) = zlong/2.
              iepoint(nlayers) = ie2
              ippoint(nlayers) = ip2
              ilpoint(nlayers) = ilyr
              iemark = ie2
              ipmark = ip2

              IF((ilyr-1).lt.1) go to 780
              DO 774 ilyr2=ilyr-1,1,-1
C- does the layers exist?
                IF(esum5(ilyr2).LT.0.) go to 774

C- do adjacent cells under this seed exist in this layer ?
                DO 775 ie3=itlo,ithi
                  DO 776 ip3=itlo,ithi
                    ie23 = iemark + ie3
                    ip23 = ipmark + ip3
                    IF(abs(ie23).GT.itthi .OR. abs(ip23).GT.itthi)
     &                go to 776
                    IF(entower(ie23,ip23,ilyr2).ge.0.) then
                      icntseed(iseed) = icntseed(iseed) + 1
                      IF(ilyr2.GE.11)
     &                  icnthseed(iseed) = icnthseed(iseed) + 1
                      IF(ilyr2.GE.8)
     &                  icntghseed(iseed) = icntghseed(iseed) + 1
                      go to 777
                    END IF
  776             CONTINUE
  775           CONTINUE
                go to 774
  777           CONTINUE

                IF(esum5(ilyr2).EQ.0.) go to 774
C- set this to false until energy is found in an adjacent cell in this layer
                lenergy = .false.
                DO 778 ie3=itlo,ithi
                  DO 779 ip3=itlo,ithi
                    ie23 = iemark + ie3
                    ip23 = ipmark + ip3
C- make sure this cell is within the 5x5 defined in /MTC_ETOWERS/
                    IF(abs(ie23).GT.itthi .OR. abs(ip23).GT.itthi)
     &                go to 779
                    IF(.NOT.mtc_lexist(ie23,ip23,ilyr2)) go to 779
C- is there energy in this cell ?
                    IF(entower(ie23,ip23,ilyr2).gt.0.) then
C- if this is the first non0 energy cell in this layer found
                      IF(.NOT.lenergy) THEN
                        lenergy = .true.
                        nlayers = nlayers + 1
                        IF(ilyr2.GE.11)
     &                    ihithseed(iseed) = ihithseed(iseed) + 1
                        IF(ilyr2.GE.8)
     &                    ihitghseed(iseed) = ihitghseed(iseed) + 1
                        chitemp = 1.e8
                      END IF
C- adjacent non0 energy cell found ... get its x,y,z position
                      CALL mtc_get_xyz(ie23,ip23,ilyr2, x,y,z, ok)
                      IF(ok.NE.0) WRITE(6,89) ie23,ip23,ilyr2,
     &                            ietatower(ilyr),iphitower(ilyr)
                      CALL mtc_get_longz(ie23,ip23,ilyr2, zlong, ok)
                      IF(ok.NE.0) WRITE(6,88) ie23,ip23,ilyr2,
     &                            ietatower(ilyr),iphitower(ilyr)

                      xpoints(nlayers) = x
                      ypoints(nlayers) = y
                      zpoints(nlayers) = z
                      ezpoints(nlayers) = zlong/2.
C- fit a track through NLAYERS points ...
                      CALL mtc_linepntdir(
     &                  nlayers,xpoints, ypoints, zpoints,ezpoints,
     &                  point,cosdir,chicalin)
                      IF(chicalin.LT.chitemp) THEN
                        xsave = xpoints(nlayers)
                        ysave = ypoints(nlayers)
                        zsave = zpoints(nlayers)
                        esave = ezpoints(nlayers)
                        chitemp = chicalin
                        iepoint(nlayers) = ie23
                        ippoint(nlayers) = ip23
                        ilpoint(nlayers) = ilyr2
                      END IF
                    END IF
  779             CONTINUE
  778           CONTINUE
C- enter the best non0 energy cell to the track points if one was found ...
                IF(lenergy) THEN
                  xpoints(nlayers) = xsave
                  ypoints(nlayers) = ysave
                  zpoints(nlayers) = zsave
                  ezpoints(nlayers) = esave
                  iemark = iepoint(nlayers)
                  ipmark = ippoint(nlayers)
                END IF
  774         CONTINUE                  ! finished with this seed

  780         CONTINUE

              IF(chitemp.LT.chitseed(iseed) ) THEN
                DO 11 ilayers=1,nlayers
                  xseed(iseed,ilayers) = xpoints(ilayers)
                  yseed(iseed,ilayers) = ypoints(ilayers)
                  zseed(iseed,ilayers) = zpoints(ilayers)
                  ezseed(iseed,ilayers) = ezpoints(ilayers)
                  ilseed(iseed,ilayers) = ilpoint(ilayers)
                  ieseed(iseed,ilayers) = iepoint(ilayers)
                  ipseed(iseed,ilayers) = ippoint(ilayers)
   11           CONTINUE
                ihitseed(iseed) = nlayers-1
                chitseed(iseed) = chitemp
c- total fraction of layers hit ...
                fcntseed(iseed) = float(nlayers-1) /
     &                            float(icntseed(iseed))
C- hadronic fraction of layers hit ...
                IF(icnthseed(iseed).NE.0) THEN
                  fcnthseed(iseed) = float(ihithseed(iseed)) /
     &                               float(icnthseed(iseed))
                ELSE
                  fcnthseed(iseed) = 0.
                END IF
C- massless gap/icd/hadronic fraction of layers hit ...
                IF(icntghseed(iseed).NE.0) THEN
                  fcntghseed(iseed) = float(ihitghseed(iseed)) /
     &                                float(icntghseed(iseed))
                ELSE
                  fcntghseed(iseed) = 0.
                END IF

              END IF
            END IF                              ! end loop for this seed

  772     CONTINUE                              ! loop over iphi2
  771   CONTINUE                                ! loop over ieta2
  770 CONTINUE                                  ! loop over seed layers

      IF(iseed.EQ.0) go to 665
C- get direction cosines, eta,phi of all final seed tracks and energy chi2
      DO 701 nseed=1,iseed
        nlayers = ihitseed(nseed)+1
C- I require that at least 3 cal cells be used
        IF(nlayers.GE.4) THEN
C- fit a track through final NLAYERS points ...
          DO 702 ilayers=2,nlayers
            xpoints(ilayers) = xseed(nseed,ilayers)
            ypoints(ilayers) = yseed(nseed,ilayers)
            zpoints(ilayers) = zseed(nseed,ilayers)
            ezpoints(ilayers) = ezseed(nseed,ilayers)
  702     CONTINUE
          CALL MTC_LINEPNTDIR(NLAYERS,XPOINTS,YPOINTS,ZPOINTS,EZPOINTS,
     &                        point,cosdir,chicalin)
C- store the direction cosines of the seed track
          DO 703 ixyz=1,3
            pntseed(nseed,ixyz) = point(ixyz)
            cosseed(nseed,ixyz) = cosdir(ixyz)
  703     CONTINUE
C- store the eta,phi of the seed track
          CALL MTC_COSETAPHI(COSDIR, eta,phi)
          etaseed(nseed) = eta
          phiseed(nseed) = phi
C- Get the energy sum for this seed's best track
          chieseed(nseed) = 0.
          enetseed(nseed) = 0.
          DO 704 ilayers=nlayers,2,-1
            ilyr = ilseed(nseed,ilayers)
            ie2  = ieseed(nseed,ilayers)
            ip2  = ipseed(nseed,ilayers)
ccccc            chieseed(nseed) = chieseed(nseed) + entower(ie2,ip2,ilyr)
            enetseed(nseed) = enetseed(nseed) + entower(ie2,ip2,ilyr)
  704     CONTINUE
ccccc          chieseed(nseed) = chieseed(nseed) / float(nlayers-1)
        END IF
  701 CONTINUE

C- Order the seed track results by number of hadronic layers hit,
C- number of total layers hit, track chi2, number of possible layers ...
      DO 903 nseed=1,iseed-1
        DO 905 mseed=nseed+1,iseed
          IF((ihithseed(mseed).GT.ihithseed(nseed))
     &      .OR.
     &      (ihithseed(mseed).EQ.ihithseed(nseed) .AND.
     &      ihitseed(mseed).GT.ihitseed(nseed) )
     &      .OR.
     &      (ihithseed(mseed).EQ.ihithseed(nseed) .AND.
     &      ihitseed(mseed).EQ.ihitseed(nseed)    .AND.
     &      chitseed(mseed).LT.chitseed(nseed) )
     &      .OR.
     &      (ihithseed(mseed).EQ.ihithseed(nseed) .AND.
     &      ihitseed(mseed).EQ.ihitseed(nseed)    .AND.
     &      chitseed(mseed).EQ.chitseed(nseed)    .AND.
     &      icntseed(mseed).GT.icntseed(nseed) )) THEN
C- switch the positions of the mth and nth track
            CALL mtc_rswap(chitseed(mseed),  chitseed(nseed))
            CALL mtc_rswap(chieseed(mseed),  chieseed(nseed))
            CALL mtc_rswap(enetseed(mseed),  enetseed(nseed))

            CALL mtc_rswap(fcntseed(mseed),  fcntseed(nseed))
            CALL mtc_rswap(fcnthseed(mseed), fcnthseed(nseed))
            CALL mtc_rswap(fcntghseed(mseed), fcntghseed(nseed))

            CALL mtc_iswap(icntseed(mseed),  icntseed(nseed))
            CALL mtc_iswap(ihitseed(mseed),  ihitseed(nseed))
            CALL mtc_iswap(icnthseed(mseed), icnthseed(nseed))
            CALL mtc_iswap(ihithseed(mseed), ihithseed(nseed))
            CALL mtc_iswap(icntghseed(mseed), icntghseed(nseed))
            CALL mtc_iswap(ihitghseed(mseed), ihitghseed(nseed))
C- track fit points used ...
            maxlayers = max(ihitseed(mseed),ihitseed(nseed))
            DO 906, ilayers=1,maxlayers+1
            CALL mtc_iswap(ilseed(mseed,ilayers),ilseed(nseed,ilayers))
            CALL mtc_iswap(ieseed(mseed,ilayers),ieseed(nseed,ilayers))
            CALL mtc_iswap(ipseed(mseed,ilayers),ipseed(nseed,ilayers))
            CALL mtc_rswap(xseed(mseed,ilayers),xseed(nseed,ilayers))
            CALL mtc_rswap(yseed(mseed,ilayers),yseed(nseed,ilayers))
            CALL mtc_rswap(zseed(mseed,ilayers),zseed(nseed,ilayers))
            CALL mtc_rswap(ezseed(mseed,ilayers),ezseed(nseed,ilayers))
  906       CONTINUE
C- track fit results (with vertex) ...
            DO 907 ixyz=1,3
              CALL mtc_rswap(pntseed(mseed,ixyz), pntseed(nseed,ixyz))
              CALL mtc_rswap(cosseed(mseed,ixyz), cosseed(nseed,ixyz))
  907       CONTINUE
            CALL mtc_rswap(etaseed(mseed), etaseed(nseed))
            CALL mtc_rswap(phiseed(mseed), phiseed(nseed))
          END IF
  905   CONTINUE
  903 CONTINUE

  700 CONTINUE
c- tracks are numbered best to worst ... now eliminate tracks with
C- less than 3 cells used or delta(eta)<.03 or delta(phi)<.03
C- If track fails these criteria, the track with the next numbered track
      NMTC_TRACKS = 0
      IF(iseed.EQ.0) go to 665
      DO 604 nseed=1,iseed
C- make sure there were at least 3 cal hits utilized ...
        IF(ihitseed(nseed).LE.2) go to 600
C- eliminate tracks w/in deta,dphi<.03 of previous good tracks
        IF(nseed.GT.1) THEN
          DO 603 mseed=1,nseed-1
            IF( abs(etaseed(nseed)-etaseed(mseed)).LT.0.03 .AND.
     &          abs(phiseed(nseed)-phiseed(mseed)).LT.0.03 ) go to 600
  603     CONTINUE                      ! loop over prev etaphi tracks
        END IF

C- this is a good track, add to count then go to next seed track
        NMTC_TRACKS = NMTC_TRACKS + 1
        go to 604

  600   CONTINUE
C- if this is the last track, then just reduce iseed by 1
        IF(nseed.EQ.iseed) go to 602
C- replace the NSEEDth seed track with the (NSEED+1)th track, etc
        DO 601 mseed=nseed+1,iseed
C- move the mth track into the m-1th position
          chitseed(mseed-1)  = chitseed(mseed)
          chieseed(mseed-1)  = chieseed(mseed)
          enetseed(mseed-1)  = enetseed(mseed)

          fcntseed(mseed-1)  = fcntseed(mseed)
          fcnthseed(mseed-1) = fcnthseed(mseed)
          fcntghseed(mseed-1) = fcntghseed(mseed)

          icntseed(mseed-1)  = icntseed(mseed)
          ihitseed(mseed-1)  = ihitseed(mseed)
          icnthseed(mseed-1) = icnthseed(mseed)
          ihithseed(mseed-1) = ihithseed(mseed)
          icntghseed(mseed-1) = icntghseed(mseed)
          ihitghseed(mseed-1) = ihitghseed(mseed)
C- track fit points used ...
          maxlayers = max(ihitseed(mseed-1),ihitseed(mseed))
          DO 606 ilayers=1,18
            ilseed(mseed-1,ilayers) = ilseed(mseed,ilayers)
            ieseed(mseed-1,ilayers) = ieseed(mseed,ilayers)
            ipseed(mseed-1,ilayers) = ipseed(mseed,ilayers)

            xseed(mseed-1,ilayers)  = xseed(mseed,ilayers)
            yseed(mseed-1,ilayers)  = yseed(mseed,ilayers)
            zseed(mseed-1,ilayers)  = zseed(mseed,ilayers)
            ezseed(mseed-1,ilayers) = ezseed(mseed,ilayers)
  606     CONTINUE
C- track fit results (with vertex) ...
          DO 607 ixyz=1,3
            pntseed(mseed-1,ixyz) = pntseed(mseed,ixyz)
            cosseed(mseed-1,ixyz) = cosseed(mseed,ixyz)
  607     CONTINUE
          etaseed(mseed-1) = etaseed(mseed)
          phiseed(mseed-1) = phiseed(mseed)
  601   CONTINUE
C- one less number of seed tracks
  602   CONTINUE
        iseed = iseed - 1
        go to 700
  604 CONTINUE                          ! loop over good seed tracks

  665 CONTINUE
C- transfer best 5 seed tracks to the best track common block
      DO 804 ibest=1,5
        IF(iseed.EQ.0) go to 809
C- make sure there was at least ibest tracks found ...
        IF(ibest.GT.iseed) go to 809
C- make sure there were at least 3 cal hits utilized ...
        IF(ihitseed(ibest).LE.2) go to 805
C- list their chi2, fraction of layers hit ...
        CHITBEST(IBEST) = chitseed(ibest)
        CHIEBEST(IBEST) = chieseed(ibest)
        ENETBEST(IBEST) = enetseed(ibest)

        FCNTBEST(IBEST) = fcntseed(ibest)
        FCNTHBEST(IBEST) = fcnthseed(ibest)
        FCNTGHBEST(IBEST) = fcntghseed(ibest)

        ICNTBEST(IBEST) = icntseed(ibest)
        IHITBEST(IBEST) = ihitseed(ibest)
        ICNTHBEST(IBEST) = icnthseed(ibest)
        IHITHBEST(IBEST) = ihithseed(ibest)
        ICNTGHBEST(IBEST) = icntghseed(ibest)
        IHITGHBEST(IBEST) = ihitghseed(ibest)
C- track fit points used ...
        nlayers = ihitbest(ibest) + 1
        DO 806 ilayers=1,nlayers
          ILBEST(IBEST,ILAYERS) = ilseed(ibest,ilayers)
          IEBEST(IBEST,ILAYERS) = ieseed(ibest,ilayers)
          IPBEST(IBEST,ILAYERS) = ipseed(ibest,ilayers)
          XPBEST(IBEST,ILAYERS) = xseed(ibest,ilayers)
          YPBEST(IBEST,ILAYERS) = yseed(ibest,ilayers)
          ZPBEST(IBEST,ILAYERS) = zseed(ibest,ilayers)
          EZBEST(IBEST,ILAYERS) = ezseed(ibest,ilayers)
  806   CONTINUE
C- track fit results (with vertex) ...
        DO 807 ixyz=1,3
          PNTBEST(IBEST,IXYZ) = pntseed(ibest,ixyz)
          COSBEST(IBEST,IXYZ) = cosseed(ibest,ixyz)
  807   CONTINUE
        ETABEST(IBEST) = etaseed(ibest)
        PHIBEST(IBEST) = phiseed(ibest)
        go to 804
  805   CONTINUE
C- less than 5 good tracks found ... fill the remaining track results as blank

  809   CONTINUE
        CHITBEST(IBEST) = -50.
        CHIEBEST(IBEST) = -50.
        ENETBEST(IBEST) = -50.

        FCNTBEST(IBEST) = -50.
        FCNTHBEST(IBEST) = -50.
        FCNTGHBEST(IBEST) = -50.

        ICNTBEST(IBEST) = -50
        IHITBEST(IBEST) = -50
        ICNTHBEST(IBEST) = -50
        IHITHBEST(IBEST) = -50
        ICNTGHBEST(IBEST) = -50
        IHITGHBEST(IBEST) = -50
C- no track fit points used ...
C- track fit results (with vertex) ...
        DO 808 ixyz=1,3
          PNTBEST(IBEST,IXYZ) = -50.
          COSBEST(IBEST,IXYZ) = -50.
  808   CONTINUE
        ETABEST(IBEST) = -50.
        PHIBEST(IBEST) = -50.
  804 CONTINUE
C- return best seed track information
      FLYRHIT = fcntbest(1)
      ICNTALL = icntbest(1)
      CHICALIN = chitbest(1)
      TENERGY  = chiebest(1)
      DO 908 ixyz=1,3
        POINT(IXYZ)  = pntbest(1,ixyz)
        COSDIR(IXYZ) = cosbest(1,ixyz)
  908 CONTINUE
C----------------------------------------------------------------------
   89 FORMAT(' MTC_LINE_FASTBACK: error w/x,y,z ie2,ip2,il,ie,ip',5I4)
   88 FORMAT(' MTC_LINE_FASTBACK: error w/longz ie2,ip2,il,ie,ip',5I4)
  999 RETURN
      END
