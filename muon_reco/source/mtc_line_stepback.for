      SUBROUTINE MTC_LINE_STEPBACK(ILYRMIN,
     &  point, cosdir, chicalin, chiene, flyrhit, icntall)
C----------------------------------------------------------------------
C- MTC_LINE_STEPBACK: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Find the best track using the following method:
C-      Designate as a seed cell a hit calorimeter cell in one of two
C-      layers furthest from the input vertex.  Step down in layer number
C-      (down to ILYRMIN), looking for a hit cell at/adjacent
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
C-             ILYRMIN - the minimum layer number used in the calorimeter
C-              (ilyrmin=1 for all cal layers,
C-               ilyrmin=8 for hadronic plus icd/mg)
C-               ilyrmin=11 for hadronic only)
C-   Outputs : POINT(3), COSDIR(3) - The output fitted line is defined by
C-              direction cosines COSDIR and an arbitrary  point
C-              POINT(3) through which the line passes.
C-      CHICALIN - the chisquared of the fit - equal to the sqrt
C-              of the sum of the squares of the perpendicular
C-              distance from the fitted line to the calorimeter
C-              cell points
C-      CHIENE  - the MIP energy chisquared for the set of chosen points.
C-      FLYRHIT - the fraction of possible layers utilized to get the track
C-       If the input arguments are inappropriate:
C-              If there is no energy in the given tower, then
C-              POINT(IXYZ) = 0., COSDIR(IXYZ) = 0., CHICALIN = -50.
C-
C-   Created   18-JAN-1993   Elizabeth Gallas
C-   Modified   2-FEB-1995   Elizabeth Gallas - see comments in code
C-   Modified  13-APR-1995   Elizabeth Gallas - loop over seed cells
C-    starting from the center of the central tower, use seed cells in 
C-    last 3 hit layers rather than 2, change criteria for best track
C-    when more than 3 hadronic layers are available.
C-    The same routine will work if using all possible layers.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input ...
      INCLUDE 'D0$INC:mtc_etowers.inc'
      INCLUDE 'D0$INC:mtc_e5towers.inc'
      INTEGER ilyrmin
C- output arguments contain track info for best track
      REAL POINT(3), COSDIR(3), CHICALIN, CHIENE, FLYRHIT
      INTEGER ICNTALL
C- output block contains info for 5 best tracks
      INCLUDE 'D0$INC:MTC_BESTRKS.INC'
C- local ...
      INTEGER ixyz, nlayers,ilayers, iemark,ipmark,
     &  iseed,nseed,mseed, nlseed(3),nseedlyr
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
      INTEGER ilyr, jlyr, klyr, ie2,ip2, ilyr2, ie3,ip3,ie23,ip23
      LOGICAL lyrexi,lenergy, lswitch
      INTEGER ok
C- functns
      LOGICAL mtc_lexist
C----------------------------------------------------------------------
      INTEGER ifirst,iloop(itthi*2+1),ielp,iplp
      DATA    ifirst/0/
      IF(ifirst.EQ.0) THEN
        ifirst = 1
        iloop(1) = 0
        DO 9 ie2=1,itthi
          iloop((ie2-1)*2+2) = ie2
          iloop((ie2-1)*2+3) = -ie2
    9   CONTINUE
      ENDIF
C----------------------------------------------------------------------
C- initialize the # of seeds and number of found tracks ...
      iseed   = 0
      NMTC_TRACKS = 0
      lenergy = .FALSE.
C- Check for no energy in tower ...
      IF(esum5(18).LE.0.) go to 665

c- 13-APR-95 find the three uppermost hadronic layers with non0 energy
C- these are the possible seed layers ... (formerly looked at two)
      nlseed(1) = 0
      nlseed(2) = 0
      nlseed(3) = 0
      DO 669 ilyr=17,11,-1
        IF(esum5(ilyr).GT.0.) THEN
          nlseed(1) = ilyr
          IF(ilyr.EQ.11) go to 664
          DO 668 jlyr=nlseed(1)-1,11,-1
            IF(esum5(jlyr).GT.0.) THEN
              nlseed(2) = jlyr
              IF(jlyr.EQ.11) go to 664
              DO 670 klyr=nlseed(2)-1,11,-1
                IF(esum5(klyr).GT.0.) THEN
                  nlseed(3) = klyr
                  go to 667
                ENDIF
  670         CONTINUE
            END IF
  668     CONTINUE
          go to 664
        END IF
  669 CONTINUE
  664 CONTINUE
C- **      IF(nlseed(1).EQ.0 .OR. nlseed(1).EQ.11) go to 665
      IF(nlseed(1).EQ.0) go to 665
  667 CONTINUE
C----------------------------------------------------------------------
c- 13-APR-95 - don't use cells in layer 11 as seeds, loop over
c- nseedlyr seed layers below
      nseedlyr = 3
C- **      IF(nlseed(3).EQ.0 .OR. nlseed(3).EQ.11) nseedlyr  = 2
C- **      IF(nlseed(2).EQ.0 .OR. nlseed(2).EQ.11) nseedlyr  = 1
      IF(nlseed(3).EQ.0) nseedlyr  = 2
      IF(nlseed(2).EQ.0) nseedlyr  = 1
C----------------------------------------------------------------------
C- make the first point the vertex position ...
      nlayers = 1
      xpoints(nlayers) = vtxtwr(1)
      ypoints(nlayers) = vtxtwr(2)
      zpoints(nlayers) = vtxtwr(3)
      ezpoints(nlayers) = dvtxtwr(3)
C- loop over all possible seed cells ...
C- **      DO 770 ilyr=17,ilseed2,-1
      DO 770 jlyr=1,nseedlyr
        ilyr = nlseed(jlyr)
C- does the layer exist and if so, does it have zero energy ?
        IF(esum5(ilyr).LT.0.) go to 770
        IF(esum5(ilyr).EQ.0.) go to 770
C- it exists and it has energy, look for the seed cells ...
C- **   DO 771 ie2=ittlo,itthi
        DO 771 ielp=1,2*itthi+1
          ie2 = iloop(ielp)
C- **     DO 772 ip2=ittlo,itthi
          DO 772 iplp=1,2*itthi+1
            ip2 = iloop(iplp)

            IF(entower(ie2,ip2,ilyr).gt.0.) then
C- seed layer found, make sure it's not directly below another seed ...
C- **              IF(ilyr.EQ.ilseed2 .AND. ilseed1.NE.ilseed2 .AND.
C- **     &           entower(ie2,ip2,ilseed1).ne.0.) go to 772
              IF(jlyr.GE.2) THEN
                DO 781 klyr=jlyr-1,1,-1
                  IF(entower(ie2,ip2,nlseed(klyr)).gt.0.)
     &              go to 772
  781           CONTINUE
              ENDIF

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

              IF((ilyr-1).lt.ilyrmin) go to 780
              DO 774 ilyr2=ilyr-1,ilyrmin,-1
C- does the layer exist?
                IF(esum5(ilyr2).LT.0.) go to 774

C- 2-feb-95 don't count the layer unless a cell exists directly under
C- the seed cell OR an adjacent cell under the seed is hit
                lyrexi = .false.
                IF(entower(iemark,ipmark,ilyr2).ge.0.) then
                  lyrexi = .true.
                  icntseed(iseed) = icntseed(iseed) + 1
                  IF(ilyr2.GE.11)
     &              icnthseed(iseed) = icnthseed(iseed) + 1
                  IF(ilyr2.GE.8)
     &              icntghseed(iseed) = icntghseed(iseed) + 1
                END IF

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

C- 2-feb-95 if a cell dne under the seed cell, but energy is seen in
C- an adjacent cell, then count it now
                      IF(.NOT.lyrexi) THEN
                        lyrexi = .true.
                        icntseed(iseed) = icntseed(iseed) + 1
                        IF(ilyr2.GE.11)
     &                    icnthseed(iseed) = icnthseed(iseed) + 1
                        IF(ilyr2.GE.8)
     &                    icntghseed(iseed) = icntghseed(iseed) + 1
                      ENDIF

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
  774         CONTINUE  ! finished with this layer below the current seed

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
C----------------------------------------------------------------------
              IF(iseed.EQ.0 .OR. nlayers.LE.3) go to 772
C- may-2,1994 - A track has been found for this seed.
C- Reiterate over layers hit to see if any other cell in that layer
C- gives a lower chi^2 than the one found.  At the end of these loops,
C- the best possible track will have been found for this seed.
C- The seed is always ilayer=2 (vtx is ilayer=1).

              DO 750 ilayers=1,nlayers
                xpoints(ilayers)  = xseed(iseed,ilayers)
                ypoints(ilayers)  = yseed(iseed,ilayers)
                zpoints(ilayers)  = zseed(iseed,ilayers)
                ezpoints(ilayers) = ezseed(iseed,ilayers)
  750         CONTINUE

              DO 751 ilayers=3,nlayers
                ilyr2 = ilseed(iseed,ilayers)
                iemark = ieseed(iseed,ilayers-1)
                ipmark = ipseed(iseed,ilayers-1)
                DO 752 ie3=itlo,ithi
                  DO 753 ip3=itlo,ithi
                    ie23 = iemark + ie3
                    ip23 = ipmark + ip3
C- make sure this cell is within the 5x5 defined in /MTC_ETOWERS/
                    IF(abs(ie23).GT.itthi .OR. abs(ip23).GT.itthi)
     &                go to 753
                    IF(.NOT.mtc_lexist(ie23,ip23,ilyr2)) go to 753
C- see if this cell was the one already chosen
                    IF(ie23.EQ.iepoint(ilayers) .AND.
     &                ip23.EQ.ippoint(ilayers)) go to 753
C- is there energy in this cell ?
                    IF(entower(ie23,ip23,ilyr2).gt.0.) then
C- adjacent non0 energy cell found ... get its x,y,z position
                      CALL mtc_get_xyz(ie23,ip23,ilyr2, x,y,z, ok)
                      IF(ok.NE.0) WRITE(6,89) ie23,ip23,ilyr2,
     &                            ietatower(ilyr),iphitower(ilyr)
                      CALL mtc_get_longz(ie23,ip23,ilyr2, zlong, ok)
                      IF(ok.NE.0) WRITE(6,88) ie23,ip23,ilyr2,
     &                            ietatower(ilyr),iphitower(ilyr)
C- save the old point in case I need it back ...
                      xsave = xpoints(ilayers)
                      ysave = ypoints(ilayers)
                      zsave = zpoints(ilayers)
                      esave = ezpoints(ilayers)
C- enter the new point in the array at layer ilayer
                      xpoints(ilayers) = x
                      ypoints(ilayers) = y
                      zpoints(ilayers) = z
                      ezpoints(ilayers) = zlong/2.
C- fit a track through NLAYERS points ...
                      CALL MTC_LINEPNTDIR(
     &                  NLAYERS,XPOINTS, YPOINTS, ZPOINTS,EZPOINTS,
     &                  point,cosdir,chicalin)
C- if a better track residual is found, replace the point by the new point,
                      IF(chicalin.LT.chitseed(iseed)) THEN
                        chitseed(iseed) = chicalin
                        ieseed(iseed,ilayers) = ie23
                        ipseed(iseed,ilayers) = ip23
                      ELSE
C- if not, put the saved point back into the array.
                        xpoints(ilayers) = xsave
                        ypoints(ilayers) = ysave
                        zpoints(ilayers) = zsave
                        ezpoints(ilayers) = esave
                      END IF
                    END IF
  753             CONTINUE                ! loop over relative ieta
  752           CONTINUE                ! loop over relative iphi
  751         CONTINUE                ! loop over ilayers for this seed

              DO 754 ilayers=1,nlayers
                xseed(iseed,ilayers)  = xpoints(ilayers)
                yseed(iseed,ilayers)  = ypoints(ilayers)
                zseed(iseed,ilayers)  = zpoints(ilayers)
                ezseed(iseed,ilayers) = ezpoints(ilayers)
  754         CONTINUE
C----------------------------------------------------------------------
            END IF                              ! end loop for this seed

  772     CONTINUE                              ! loop over iphi2
  771   CONTINUE                                ! loop over ieta2
  770 CONTINUE                                  ! loop over seed layers

C- The best track has been found for all seeds.

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
C- Get the energy chi squared for this seed's best track
          chieseed(nseed) = 0.
          enetseed(nseed) = 0.
          DO 704 ilayers=nlayers,2,-1
            ilyr = ilseed(nseed,ilayers)
            ie2  = ieseed(nseed,ilayers)
            ip2  = ipseed(nseed,ilayers)
            chieseed(nseed) = chieseed(nseed) + chitower(ie2,ip2,ilyr)
            enetseed(nseed) = enetseed(nseed) + entower(ie2,ip2,ilyr)
  704     CONTINUE
          chieseed(nseed) = chieseed(nseed) / float(nlayers-1)
        END IF
  701 CONTINUE

C- order the tracks best to worst
      DO 903 nseed=1,iseed-1
        DO 905 mseed=nseed+1,iseed

C- **C- Order the seed track results by num of hadronic layers hit,
C- **C- num of total layers hit, track chi2, num of total layers ...

          lswitch = .false.
C- do this the old way if the best track currently has less than 3 layers
          IF(icnthseed(nseed).LT.3 .OR. icnthseed(mseed).LT.3) THEN
            IF( (ihithseed(mseed).GT.ihithseed(nseed))
     &        .OR.
     &        (ihithseed(mseed).EQ.ihithseed(nseed) .AND.
     &        ihitseed(mseed).GT.ihitseed(nseed) )
     &        .OR.
     &        (ihithseed(mseed).EQ.ihithseed(nseed) .AND.
     &        ihitseed(mseed).EQ.ihitseed(nseed)    .AND.
     &        chitseed(mseed).LT.chitseed(nseed) )
     &        .OR.
     &        (ihithseed(mseed).EQ.ihithseed(nseed) .AND.
     &        ihitseed(mseed).EQ.ihitseed(nseed)    .AND.
     &        chitseed(mseed).EQ.chitseed(nseed)    .AND.
     &        fcnthseed(mseed).GT.fcnthseed(nseed) ) )
     &        lswitch = .true.

C- if(icntseed).ge.3) fraction hadronic layers, fract all layers,
C- 13-APR-95 - Order the tracks by frac had layers, fract all layers,
C- track chi2 (track residual), number of total layers
          ELSE
            IF((fcnthseed(mseed).GT.fcnthseed(nseed))
     &        .OR.
     &        (fcnthseed(mseed).EQ.fcnthseed(nseed) .AND.
     &        fcntseed(mseed).GT.fcntseed(nseed) )
     &        .OR.
     &        (fcnthseed(mseed).EQ.fcnthseed(nseed) .AND.
     &        fcntseed(mseed).EQ.fcntseed(nseed)    .AND.
     &        chitseed(mseed).LT.chitseed(nseed) )
     &        .OR.
     &        (fcnthseed(mseed).EQ.fcnthseed(nseed) .AND.
     &        fcntseed(mseed).EQ.fcntseed(nseed)    .AND.
     &        chitseed(mseed).EQ.chitseed(nseed)    .AND.
     &        icnthseed(mseed).GT.icnthseed(nseed) ))
     &        lswitch = .true.
          ENDIF
          IF(.NOT.lswitch) go to 905

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
  906     CONTINUE
C- track fit results (with vertex) ...
          DO 907 ixyz=1,3
            CALL mtc_rswap(pntseed(mseed,ixyz), pntseed(nseed,ixyz))
            CALL mtc_rswap(cosseed(mseed,ixyz), cosseed(nseed,ixyz))
  907     CONTINUE
          CALL mtc_rswap(etaseed(mseed), etaseed(nseed))
          CALL mtc_rswap(phiseed(mseed), phiseed(nseed))
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
      CHIENE  = chiebest(1)
      DO 908 ixyz=1,3
        POINT(IXYZ)  = pntbest(1,ixyz)
        COSDIR(IXYZ) = cosbest(1,ixyz)
  908 CONTINUE
C----------------------------------------------------------------------
   89 FORMAT(' MTC_LINE_STEPBACK: error w/x,y,z ie2,ip2,il,ie,ip',5I4)
   88 FORMAT(' MTC_LINE_STEPBACK: error w/longz ie2,ip2,il,ie,ip',5I4)
  999 RETURN
      END
