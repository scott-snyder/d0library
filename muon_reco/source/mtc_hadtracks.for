      SUBROUTINE MTC_HADTRACKS( PNTMU,DIRMU, frac, ntot,
     &  cdist,hfrac,esum,cosmtc,pntmtc,tresid,nhad,np,iflg)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modified version of mtc_hadtracks
C-    to find a track for a muot candidate in the calorimeter.
C-    This version includes all cal cells in the fit plus the muot
C-    input point using the same fitting routine as offline MTC.
C-    Cells are chosen which are closest to a line through the
C-    muot input point and the average position of cells found so far.
C-    Two output arguments are added:  FRAC and NTOT.
C-    This version uses the extent of each cell in z as the error
C-    in the fit - previous versions used 1 or 2 cm uniformly.
C-
C-   Inputs  : PNTMU(3) - x,y,z coord of track inside magnet
C-                        (should correspond to MUOT words 8,9,10)
C-             DIRMU(3) - x,y,z direction cosines of track
C-                        (should correspond to MUOT words 14,15,16)
C-   Outputs :
C-             frac      - the fraction of cal layers hit associated
C-                         with the best track
C-             ntot      - the number of total layers traversed by
C-                         the best track
C-
C-             cdist     - closest dist from input track to beamline
C-             hfrac     - fraction of hadronic layers hit for
C-                         longest set of contiguously hit cells
C-             esum      - sum of cal energy in cells assoc with track
C-             cosmtc(3) - direction cosine of best found crude cal track
C-                         from the calorimeter cells hit and the muot pnt
C-             pntmtc(3) - the average x,y,z of all cal cells associated
C-                         with the best track
C-             tresid    - track residual - sqrt of sum of squares of
C-                         the distance from the cells to the fit line
C-             nhad      - number of hadronic cells traversed by best trk
C-                         (number of cells hit = hfrac * nhad)
C-             np        - abs(number of phi projective towers crossed)
C-                         if(np.ge.2) --> cosmic ray !
C-             iflg = -3 - input line has a dca outside of VTX chamber in z
C-                  = -2 - line does not enter vol w/in CC cal outer radius
C-                  = -1 - input line parallel to z-axis
C-                         if(iflg<0) nominal vertex position will be used
C-                  =  0 - special case - line || to x or y axis
C-                  = +1 - line passes inside vertex chamber
C-                  = +2 - line passes inside TRD (outside VTX)
C-                  = +3 - line passes inside central drift chamber
C-                  = +4 - line passes inside calorimeter (outside CD)
C-
C-   Created  17-APR-1995   Elizabeth Gallas
C-   Modified 14-AUG-1995   Elizabeth Gallas - to use all layers
C-                          (not just hadronic)
C-   Modified 14-AUG-1995   Elizabeth Gallas - add the muot point
C-                          to the final fit
C-   Modified 6-SEP-1995    Elizabeth Gallas - use extent of the cell
C-                          in z for cell width
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- replace CALTRACK with D0$INC when running in L2 environment !!!!!!!!!
      INCLUDE 'D0$INC:ZEBCOM.INC'
C- input
      REAL    pntmu(3),dirmu(3)
C- output
      REAL    cdist,hfrac,esum,cosmtc(3),pntmtc(3),tresid
      REAL    frac
      INTEGER nhad,np,iflg
      INTEGER ntot
C- program variables for that can be modified - had frac threshold for
C- best track to pass, half width of tower to look for muons in eta,phi.
C- I require that IEMAX.LE.IPMAX
      INTEGER iemax,ipmax
      PARAMETER( iemax = 2, ipmax = 2 )
C- store tower energies(Et), num of layers and their lyr#,eta,phi index
      REAL    energy(-iemax:iemax,-ipmax:ipmax,1:17)
      INTEGER nlayer,nlyr(1:17),neta(1:17),nphi(-ipmax:ipmax),
     &        meta(-iemax:iemax,1:17)
C- store min,max _eta,_phi of tower and lo,hi eta,phi of cal road
      INTEGER max_eta,min_eta,max_phi,min_phi
C- for calling clinpl
      REAL    vtx(3)
      DATA    vtx/0.,0.,0./
      INTEGER ncell,argsok,ietac(20),iphic(20),layerc(20)
      REAL    pathc(20), dircin(3)
C- for calling mtcl2_zimpact
      REAL    cpnt(3),zvtx,rdist
      INTEGER ierr
C- store pnt in muon sys, seed position, avg cell posn,
C- candidate cell posn, current best candidate cell position
      REAL    pntmu2(3), pntseed(3), pnt2(3), pnt3(3), pntsave(3)
c- do loops, eta, phi and lyr indices
      INTEGER ncl,ilayer, il2, mlayer, ie,ip, ilseed(3),
     &        iemark,ipmark, ienext,ipnext
      INTEGER ieta,iphi,ieta2,iphi2,jphi, ie2,ip2,ie3
      LOGICAL lyrexi,lyrhit
C- counts, array for dp
      INTEGER nhcells,nhhits,ncells,nhits, ihits,ipp(15),ipptemp(15)
C-local
      INTEGER mtc_iwhere, ihere
      REAL    enrg, x,y,z,
     &        esave, etseed, distmin,dist, hfseed, cosdir(3)
C----------------------------------------------------------------------
C- save the positions of the cells in the best track found
C- A max of 16 points since lyrs 4-6 are = lyr3 (plus 1pnt for muot pnt)
      REAL    linesave(3,16),line(3,16), ezsave(16),longz,
     &        xpnts(16),ypnts(16),zpnts(16),ezpnts(16), chicalin
C----------------------------------------------------------------------
      INTEGER ifirst,iloop(iemax*2+1),ielp,iplp
      DATA    ifirst/0/
C- To run correctly, IEMAX.LE.IPMAX
      IF(ifirst.EQ.0) THEN
        ifirst = 1
        iloop(1) = 0
        DO 9 ie=1,iemax
          iloop((ie-1)*2+2) = ie
          iloop((ie-1)*2+3) = -ie
    9   CONTINUE
        CALL MTC_FILL_SUBLYR()
      ENDIF
C----------------------------------------------------------------------
C- initialize
      cdist  = -1.
      HFRAC  = 0.
      ESUM   = 0.
      nhad   = 0
      np     = 0
      COSMTC(1) = 0.
      COSMTC(2) = 0.
      COSMTC(3) = 0.
      PNTMTC(1) = 0.
      PNTMTC(2) = 0.
      PNTMTC(3) = 0.
C----------------------------------------------------------------------
C- See if point PNTMU is in the muon system or is the vertex point
C- Central cal is 261.cm long and has an outer radius of 219.cm
      rdist = sqrt(pntmu(1)**2 + pntmu(2)**2)
      IF(abs(pntmu(3)).LT.130. .AND. rdist.LT.200.) THEN
C- PNTMU is in the cd or cal volume, move it outside
        pntmu2(1) = pntmu(1) + 300.*dirmu(1)
        pntmu2(2) = pntmu(2) + 300.*dirmu(2)
        pntmu2(3) = pntmu(3) + 300.*dirmu(3)
      ELSE
        pntmu2(1) = pntmu(1)
        pntmu2(2) = pntmu(2)
        pntmu2(3) = pntmu(3)
      ENDIF
C----------------------------------------------------------------------
C- Find the point on the input line which comes closest to to z-axis
      CALL MTCL2_ZIMPACT( PNTMU2,DIRMU, dist,cpnt,ierr)
C- IERR=-1 (line || to x or y axis), =0 (line parallel to z-axis),
C- =1,2,3,4,5 (line passes inside VTX, TRD, CD, CAL or outside CAL vol)
      cdist = dist
      iflg  = ierr
      zvtx  = cpnt(3)
C- a nominal vertex will be used if (-1) input line is || to z-axis,
C- (-2) line does not enter vol w/in cal outer rad, or DCA is outside
C- out CDC in z direction.  The CDC length is 179.4cm.
      IF(ierr.LT.0) zvtx = 0.
      IF(abs(zvtx).GT.89.7) THEN
        zvtx = 0.
        iflg = -3
      ENDIF

      vtx(3) = zvtx
C----------------------------------------------------------------------
C- initialize central tower layer numbers
      DO 10 ilayer=1,17
        nlyr(ilayer) = 0
   10 CONTINUE
C----------------------------------------------------------------------
C- find the central tower
      CALL MTC_PNTTOCOS(VTX,PNTMU2, dircin)
      CALL CLINPL(VTX,DIRCIN,20,ncell,ietac,iphic,layerc,pathc,argsok)
C- Look for multiple cells hit in same layer,
C- if there are 2 then choose the ieta with the highest abs(ieta),
C- if there are 3 then set the first occurance to the second eta and phi.
C- there should never be 4 cells at same layer (I hope)
      nlayer = 0
C- also get min_eta,max_eta, min_phi,max_phi
      min_phi =  100
      max_phi = -100
      min_eta =  100
      max_eta = -100
      IF(ncell.GE.2) THEN
        DO 11 ncl=1,ncell
cc          IF(layerc(ncl).LE.10) go to 11
          IF((ncl.NE.ncell) .AND. layerc(ncl).EQ.layerc(ncl+1)) then
            IF((ncl+1.NE.ncell) .AND. layerc(ncl).EQ.layerc(ncl+2)) then
              ietac(ncl) = ietac(ncl+1)
              iphic(ncl) = iphic(ncl+1)
              layerc(ncl)= layerc(ncl+1)
            ELSE IF(abs(ietac(ncl+1)).gt.abs(ietac(ncl))) then
              ietac(ncl) = ietac(ncl+1)
              iphic(ncl) = iphic(ncl+1)
              layerc(ncl)= layerc(ncl+1)
            ENDIF
          ENDIF
C- enter hit layers and their corresponding eta,phi into arrays nlyr,neta,nphi
          IF(nlayer.NE.0 .AND. layerc(ncl).EQ.nlyr(layerc(ncl)))
     &                                                  go to 11
          nlayer = nlayer + 1
          nlyr(layerc(ncl)) = layerc(ncl)
          nphi(0) = iphic(ncl)

          ieta = ietac(ncl)
          neta(layerc(ncl)) = ieta
          IF(IETA.GT.MAX_ETA) MAX_ETA = IETA
          IF(IETA.LT.MIN_ETA) MIN_ETA = IETA
   11   CONTINUE
        max_phi = nphi(0)
        min_phi = nphi(0)
      ENDIF
      IF(nlayer.EQ.0) RETURN
C----------------------------------------------------------------------
C- fill in eta,phi of missing layers of central tower
      DO 13 ilayer=1,17
        IF(nlyr(ilayer).EQ.0) THEN
          DO 14 il2=1,16
            mlayer = ilayer+il2
            IF(mlayer.LE.17.AND.nlyr(mlayer).NE.0) go to 15
            mlayer = ilayer-il2
            IF(mlayer.GE.1.AND.nlyr(mlayer).NE.0) go to 15
   14     CONTINUE
          PRINT *, ' MTC_HADTRACKS: error finding layer number'
   15     nlyr(ilayer) = ilayer
          neta(ilayer) = neta(mlayer)
        ENDIF
   13 CONTINUE
C----------------------------------------------------------------------
C- fill the meta() array
      DO 16 ilayer=1,17
        ie2 = 0
        ie3 = 0
        meta(0,ilayer) = neta(ilayer)
        DO 17 ie=1,iemax
          ieta = neta(ilayer) + ie + ie2
          IF(ieta.EQ.0) THEN
            ie2 = 1
            ieta = neta(ilayer) + ie + ie2
          END IF
          meta(ie,ilayer) = ieta

          ieta = neta(ilayer) - ie - ie3
          IF(ieta.EQ.0) THEN
            ie3 = 1
            ieta = neta(ilayer) - ie - ie3
          END IF
          meta(-ie,ilayer) = ieta
   17   CONTINUE
   16 CONTINUE
C----------------------------------------------------------------------
C- fill the array relating iphi to the ip index for all layers
      DO 42 ip=-ipmax,ipmax
        IPHI  = nphi(0) + IP
        JPHI = IPHI
        IF (IPHI .GE.65) JPHI = MOD(IPHI ,64)
        IF (IPHI .LE.0)  JPHI = 64 + IPHI
        nphi(ip) = jphi
   42 CONTINUE
C----------------------------------------------------------------------
C- Fill the energy array
C- initialize the seed layers
      ilseed(1) = 0
      ilseed(2) = 0
      ilseed(3) = 0
      DO 31 ilayer=17,1,-1
C-      skip last 3 em3 sublayers (included when ilayer=3)
        IF(ilayer.GE.4.AND.ilayer.LE.6) go to 31
        DO 33 ie=-iemax,iemax
          ieta = meta(ie,ilayer)

          DO 34 ip=-ipmax,ipmax
            iphi = nphi(ip)

            energy(ie,ip,ilayer) = -2.
C- does this cell exist ?
            IHERE = MTC_IWHERE(IETA,IPHI,ILAYER)
C- EM3 includes EM4-EM6 so reset IHERE if neccessary
            IF(ABS(IETA).LE.26 .AND. ILAYER.EQ.3) THEN
              IF(IHERE.EQ.0 .AND.
     &          ABS(IETA).NE.12 .AND. ABS(IETA).NE.14) GO TO 34
              IF(ABS(IETA).EQ.14) IHERE = 2       ! ECEM
              IF(ABS(IETA).EQ.12) IHERE = 1       ! CCEM
            END IF

            IF(IHERE.EQ.0) GO TO 34
            energy(ie,ip,ilayer) = 0.

C- If we are in the .05x.05 section of EM3,
C- sum energy over 4 pads (get ENERGY) ...
            ENRG = 0.
            IF(ABS(IETA).LE.26 .AND. ILAYER.EQ.3) THEN
              CALL MTC_EN_EM3(IETA,IPHI,enrg,ierr)
            ELSE
C- else (not in EM3) get enrg for this .1x.1 pad ...
              CALL GTCAEP_ADDR(IETA,IPHI,ILAYER,enrg,ierr)
            END IF
            IF(ENRG.LT.0.) ENRG = 0.

            IF(ENRG.GT.0.) THEN
              energy(ie,ip,ilayer) = enrg
C- look for seed layers in the hadronic section only
              IF(ilayer.GE.11 .AND. ilseed(3).EQ.0) THEN
                IF(ilseed(2).EQ.0) THEN
                  IF(ilseed(1).EQ.0) THEN
                    ilseed(1)=ilayer
                  ELSE IF(ilayer.NE.ilseed(1)) THEN
                    ilseed(2)=ilayer
                  ENDIF
                ELSE IF(ilayer.NE.ilseed(2)) THEN
                  ilseed(3)=ilayer
                ENDIF
              ENDIF

            ENDIF
   34     CONTINUE
   33   CONTINUE
   31 CONTINUE
C- return if no seed cell found
      IF(ilseed(1).EQ.0) RETURN
C----------------------------------------------------------------------
C- how many layers (except layer 11) were found with seed cells
      nlayer = 3
      IF(ilseed(3).EQ.0 .OR. ilseed(3).EQ.11) nlayer  = 2
      IF(ilseed(2).EQ.0 .OR. ilseed(2).EQ.11) nlayer  = 1
C----------------------------------------------------------------------
C- loop over the number of seed layers
      DO 51 ncl=1,nlayer
        ilayer = ilseed(ncl)
C- loop over iphi cells, starting from the central tower, working outward
        DO 52 iplp=1,2*ipmax+1
          ip = iloop(iplp)
C- loop over ieta cells, starting from the central tower, working outward
          DO 53 ielp=1,2*iemax+1
            ie = iloop(ielp)
            ieta = meta(ie,ilayer)

            IF(energy(ie,ip,ilayer).le.0.) go to 53
c- seed cell found
            iemark = ie
            ipmark = ip
C- count the number of cells above this seed (always hadronic)
            nhcells = 0
            ncells  = 0
            IF(ilayer.LT.17) THEN
              DO 54 mlayer=ilayer+1,17
                IF(energy(ie,ip,mlayer).gt.0.) go to 53
                IF(energy(ie,ip,mlayer).eq.0.) then
                  nhcells = nhcells + 1
                  ncells  = ncells + 1
                ENDIF
   54         CONTINUE
            ENDIF
            nhhits  = 1
            nhits   = 1
            nhcells = nhcells + 1
            ncells  = ncells + 1
C- get the x,y,z position of the seed cell
            IPHI = NPHI(IP)
            CALL CELXYZ(IETA,IPHI,ILAYER, x,y,z,ierr)
            pnt2(1) = x
            pnt2(2) = y
            pnt2(3) = z
            etseed  = energy(ie,ip,ilayer)
C- store the seed cell location for final cal fit
            pntseed(1) = x
            pntseed(2) = y
            pntseed(3) = z

            linesave(1,1) = x
            linesave(2,1) = y
            linesave(3,1) = z
C- Get the extent of this cell in z
            CALL MTC_GET_CELLEZ(IETA,IPHI,ILAYER, longz,ierr)
            ezsave(1) = longz/2.

C- store the seed location in phi
            ipptemp(1) = iphi

C- loop over cells below this seed
            DO 56 mlayer=ilayer-1,1,-1
C-            skip last 3 em3 sublayers (included when ilayer=3)
              IF(mlayer.GE.4.AND.mlayer.LE.6) go to 56

              lyrexi = .false.
              lyrhit = .false.
              distmin   = 10000.

C- if a cell below this seed exists, count this layer now
              IF(energy(iemark,ipmark,mlayer).ge.0.) then
                lyrexi = .true.
                if(mlayer.ge.11) nhcells = nhcells + 1
                ncells = ncells + 1
              ENDIF

c- loop over adjacent cells in ieta, staying within tower range
              DO 57 ie2=iemark-1,iemark+1
                IF(abs(ie2).GT.iemax) go to 57
c- loop over adjacent cells in iphi, staying within tower range
                DO 58 ip2=ipmark-1,ipmark+1
                  IF(abs(ip2).GT.ipmax) go to 58
C- energy()<0 if the cell dne, energy()=0. if cell has no energy
                  IF(energy(ie2,ip2,mlayer).le.0.) go to 58
C- an adjacent cell in this layer has nonzero energy
C- count the layer if it hasn't been counted yet
                  IF(.NOT.lyrexi) THEN
                    lyrexi = .true.
                    if(mlayer.ge.11) nhcells = nhcells + 1
                    ncells = ncells + 1
                  ENDIF
C- cell has nonzero energy - get ieta2, iphi2 then its x,y,z location
                  IF(.NOT.lyrhit) THEN
                    lyrhit = .true.
                    if(mlayer.ge.11) nhhits  = nhhits + 1
                    nhits  = nhits + 1
                  ENDIF
                  iphi2 = nphi(ip2)
                  ieta2 = meta(ie2,mlayer)

C- If we are in the .05x.05 section of EM3, find the average location
C- over 4 pads ... if not EM3
                  IF(MLAYER.NE.3) THEN
                    CALL CELXYZ(IETA2,IPHI2,MLAYER, X,Y,Z,IERR)
                  ELSE
C- if EM3, then energy weight the position of the energy deposition ...
                    CALL MTC_EXYZEM3(IETA2,IPHI2, X,Y,Z, IERR)
                  END IF
                  IF(IERR.NE.0) THEN
                    PRINT *, ' MTC_HADTRACKS:  problem with xyz',
     &                ieta2,iphi2,mlayer
                  ENDIF
                  pnt3(1) = x
                  pnt3(2) = y
                  pnt3(3) = z
ccC- get the perp dist from cell to a line from the vtx to avg cells so far
cc                  CALL MTC_PERPDIST(PNT3,VTX,PNT2, dist,ierr)
C- get perp dist from cell to a line from the muot pnt to avg cells so far
                  CALL MTC_PERPDIST(PNT3,PNTMU2,PNT2, dist,ierr)
                  IF(dist.LT.distmin) THEN
                    distmin    = dist
                    pntsave(1) = x
                    pntsave(2) = y
                    pntsave(3) = z
                    esave      = energy(ie2,ip2,mlayer)
                    ienext     = ie2
                    ipnext     = ip2
C- store this cell location in phi
                    ipptemp(nhits) = iphi2
C- store the extent of this cell in z
                    CALL MTC_GET_CELLEZ(IETA2,IPHI2,MLAYER, longz,ierr)
                    ezsave(nhits) = longz/2.
                  END IF
   58           CONTINUE                  ! loop over candidate ip2 cells
   57         CONTINUE                  ! loop over candidate ie2 cells
C- if hit cell found in this layer, add its energy and avg in its position
              IF(lyrhit) THEN
                pnt2(1) = (pnt2(1)+pntsave(1) ) / 2.
                pnt2(2) = (pnt2(2)+pntsave(2) ) / 2.
                pnt2(3) = (pnt2(3)+pntsave(3) ) / 2.
                etseed  = etseed + esave
                iemark = ienext
                ipmark = ipnext
                linesave(1,nhits) = pntsave(1)
                linesave(2,nhits) = pntsave(2)
                linesave(3,nhits) = pntsave(3)
              ENDIF
   56       CONTINUE                    ! loop over layers below seed
C- is this the best track found so far?
            hfseed = float(nhhits) / float(nhcells)
            IF(hfseed.GT.hfrac .OR.
     &         (hfseed.EQ.hfrac .AND. nhcells.GT.nhad) ) THEN
              nhad  = nhcells
              hfrac = hfseed
              ntot  = ncells
              frac  = float(nhits) / float(ncells)
CL2- at L2, CAEP stores ET (so convert), offline CAEP is Energy (don't)
CL2-              CALL MTC_PNTTOCOS(VTX,PNT2, cosdir)
CL2-              esum  = etseed / sqrt(1 - cosdir(3)**2)
              esum        = etseed

              pntmtc(1)   = pnt2(1)
              pntmtc(2)   = pnt2(2)
              pntmtc(3)   = pnt2(3)
C- same the x,y,z positions of cells for best track for final fit
              DO 55 ihits=1,nhits
                line(1,ihits) = linesave(1,ihits)
                line(2,ihits) = linesave(2,ihits)
                line(3,ihits) = linesave(3,ihits)
   55         CONTINUE
C- save the array of iphi values for the best track
              DO 60 ihits=1,nhits
                ipp(ihits) = ipptemp(ihits)
   60         CONTINUE
            ENDIF
C???            IF(hfrac.EQ.1.0 .AND. nhad.GE.3) go to 888
   53     CONTINUE                ! loop over ip looking for seed cells
   52   CONTINUE                ! loop over ie looking for seed cells
   51 CONTINUE                ! loop over ncl, the seed layers
C----------------------------------------------------------------------
  888 CONTINUE
      nhits = nint(frac*float(ntot))
C- get the final fit for the best track
C- add the muot point first
      xpnts(1) = pntmu2(1)
      ypnts(1) = pntmu2(2)
      zpnts(1) = pntmu2(3)
      ezpnts(1) = 1.0
      IF(nhits.GE.2) THEN
        DO 59 ihits=1,nhits
          xpnts(ihits+1) = line(1,ihits)
          ypnts(ihits+1) = line(2,ihits)
          zpnts(ihits+1) = line(3,ihits)
          ezpnts(ihits+1) = ezsave(ihits)
   59   CONTINUE
        CALL MTC_LINEPNTDIR(nhits+1,XPNTS,YPNTS,ZPNTS,EZPNTS,
     &                      pnt2,cosdir,chicalin)
        tresid = chicalin
        cosmtc(1) = cosdir(1)
        cosmtc(2) = cosdir(2)
        cosmtc(3) = cosdir(3)
        pntmtc(1) = pnt2(1)
        pntmtc(2) = pnt2(2)
        pntmtc(3) = pnt2(3)
      ENDIF

c- calculate the number of phi projective towers crossed
      IF(nhits.GE.2) THEN
        DO 61 ihits=1,nhits-1
          ip = ipp(ihits+1)-ipp(ihits)
          IF(ip.EQ.+63) ip = -1
          IF(ip.EQ.-63) ip = +1
          np = np + ip
   61   CONTINUE
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
