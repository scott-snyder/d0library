      SUBROUTINE cal_emcluster(selection,select_names,limit,n_em,em_e,
     &        em_eta,em_phi,em_qual,em_ident)

C---------------------------------------------------------------------------
C-   Purpose and Methods :  Obtain Et sorted information about reconstructed
C-        good electromagnetic clusters.
C-
C-   Inputs :
C-      selection(10)     I  cleanem mask determining cleanem bits to select on.
C-                           Switches:
C-                             selection(1) < 0 : use em clusters stored in
C-                               gtslink specified with select_names.
C-                             selection(1) = 0 : accept all pelcs and pphos
C-      select_names(10)  S  names of electron (PELC) and photon (PPHO) types
C-                             to get from gtslink.
C-      limit             I  maximum number of em clusters
C-
C-   Outputs :
C-      n_em              I   total # EM objects passed in event
C-      em_e(50,#)        R   energy quantities for em clusters
C-             (1,#) = Ex
C-             (2,#) = Ey
C-             (3,#) = Ez
C-             (4,#) = E
C-             (5,#) = Et
C-            (10,#) = em E in cluster w/o central tower
C-            (11,#) = tot E in R <= 0.2
C-            (12,#) = em E in R <= 0.2
C-            (13,#) = tot E in R <= 0.4
C-            (14,#) = em E in R <= 0.4
C-            (15,#) = Et in physics isol cone (R = 0.4)
C-            (16,#) = Et in physics isol cone (R = 0.6)
C-            (17,#) = Et in physics isol cone (R = 0.7)
C-              6 thru 9, 18 thru 50 spare
C-      em_eta(10,#)      R   em cluster eta's
C-             (1,#) = physics eta
C-             (2,#) = detector eta
C-             (3,#) = ieta of hottest tower
C-             (4,#) = theta
C-             (5,#) = theta error
C-             (6,#) = theta error with vertex in fit
C-             (7,#) = Z0
C-             (8,#) = Z0 error
C-             (9,#) = Z0 error with vertex in fit
C-      em_phi(10,#)      R   phi
C-             (1,#) = phi of centroid
C-             (2,#) = r-dphi of 2D VTX track to cluster centroid
C-             (3,#) = chi-squared from x-y fit for 2D VTX track
C-      em_qual(50,#)     R   quality of EM clusters
C-             (2,#) = Hmatrix chi-squared
C-             (3,#) = EM fraction
C-             (4,#) = isolation fraction (Energy in R = 0.4)
C-             (5,#) = track match significance
C-             (6,#) = CDC dE/dx for track
C-             (7,#) = VTX dE/dx for track
C-             (8,#) = FDC dE/dx for track
C-             (9,#) = # cells in cluster
C-            (11,#) = TRD number hit anodes in layer 1
C-            (12,#) = TRD number hit anodes in layer 2
C-            (13,#) = TRD number hit anodes in layer 3
C-            (14,#) = TRD energy in layer 1
C-            (15,#) = TRD energy in layer 2
C-            (16,#) = TRD energy in layer 3
C-            (17,#) = TRD truncated sum
C-            (19,#) = TRD efficiency from truncated sum
C-            (21,#) = electron likelihood (TRD only)
C-            (22,#) = efficiency using TRD and CDC likelihood
C-            (23,#) = z-intercept for TRD layer 1
C-            (24,#) = z-intercept for TRD layer 2
C-            (25,#) = z-intercept for TRD layer 3
C-            (26,#) = TRD number hit cathodes in layer 1
C-            (27,#) = TRD number hit cathodes in layer 2
C-            (28,#) = TRD number hit cathodes in layer 3
C-            (31,#) = fraction of hit VTX wires
C-            (32,#) = fraction of hit CDC wires
C-            (33,#) = # VTX x-y hits
C-            (34,#) = # VTX 3d hits
C-            (35,#) = # CDC x-y hits
C-            (36,#) = # CDC 3d hits
C-            (37,#) = # CDC z-segments
C-            (40,#) = 4-parameter electron likelihood
C-                        (EMFR,CHI2,TRKM,DEDX)
C-            (41,#) = 5-parameter electron likelihood
C-                        (EMFR,CHI2,TRKM,DEDX,EPST)
C-              1, 10, 20, 29, 30, 38, 39, and 42 thru 50 spare
C-      em_ident(20,#)    I   id of EM clusters
C-            (1,#) = CLEANEM status
C-            (2,#) = em cluster bit pattern based on selection arrays above
C-            (3,#) = TRD layers in acceptance
C-            (4,#) = indicates PELC (-1) or PPHO (0)
C-            (6,#) = error flag for electron likelihood
C-            (10,#) = link to PELC/PPHO bank
C-              5, 7 - 9, 11 thru 20 spare
C-
C-   Created  AUG-24-1993   Bob Kehoe
C-   Updated  Feb-08-1994   Bob Kehoe -- allow ppho's also
C-   Updated  Mar-27-1994   Bob Kehoe -- use gtslink, vertex_info and det_eta
C-   Updated  May-25-1994   Bob Kehoe -- make gtslink an option, add VTX dE/dx,
C-                                       use zlinka.inc
C-   Updated  Jun-16-1994   Bob Kehoe -- add 4 TRD words to em_qual array
C-   Updated  Oct-06-1994   Bob Kehoe -- added coordinates of hottest tower,
C-                                       added TRD layer words
C-   Updated  Jun-25-1995   Bob Kehoe -- add HITSINFO, TRD geom, PELC/PPHO flag
C-   Updated  Sep-01-1995   Bob Kehoe -- add electron likelihood
C-   Updated  May-01-1996   Bob Kehoe -- add forward TRD, new VTX quantities
C-   Modified Oct-10-1996   Ela Barberis -- modify definition of em_e(15,n_em)
C-                                          through em_e(17,n_em)
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER wanted,ntyp_max,mask_all,mask_trdoff
      REAL fhadhi_cc,fhadhi_ec
      PARAMETER (wanted = 3)
      PARAMETER (ntyp_max = 10)
      PARAMETER (mask_all = '1F'x)
      PARAMETER (mask_trdoff = '0F'x)
      PARAMETER (fhadhi_cc = 0.52)
      PARAMETER (fhadhi_ec = 0.62)
      INTEGER ier,i,k,n,nelc,npho,n_em,limit,k_em(ntyp_max),layer
      INTEGER nz,ncvar,ntvar,n_em_max,ntyp,nelph(ntyp_max)
      INTEGER selection(ntyp_max),status,version,reco_pass
      INTEGER lpelc,gzpelc,lppho,gzppho,lcacl,lzfit
      INTEGER ltana,ltdst,gztdst,lcash,lztrk,ltrdt
      INTEGER lvtxt,em_links(ntyp_max,nslink),links(nslink)
      INTEGER em_ident(20,limit)
      REAL zv(wanted),dz(wanted),info(3,wanted),theta
      REAL em_e(50,limit),em_eta(10,limit),em_phi(10,limit),et_em,et_max
      REAL em_qual(50,limit),cquan(50),tquan(50),phi
      REAL st,vin(6),vout(6),sig1,sig2
      REAL elike,elike_nt,elikehi,dummy,elike_set_mask_cc
      REAL elike_set_mask_ec
      LOGICAL ok,yep,done,first,geometry(3),badtrack(10)
      CHARACTER*4 bank
      CHARACTER*8 select_names(ntyp_max),ename
      DATA first/.true./

C----------------------------------------------------------------------
C-          INITIALIZATION
      n_em = 0
      DO i = 1,limit
        DO k = 1,50
          em_e(k,i) = 0.
          em_qual(k,i) = 0.
        ENDDO
        DO k = 1,10
          em_eta(k,i) = 0.
          em_phi(k,i) = 0.
        ENDDO
        DO k = 1,20
          em_ident(k,i) = 0
        ENDDO
      ENDDO
      CALL gtpelc_total(nelc,ier)
      IF (ier.NE.0) CALL errmsg('gtpelc_total FAILED',
     &      'cal_emcluster','error in # elec','W')
      CALL gtppho_total(npho,ier)
      IF (ier.NE.0) CALL errmsg('gtppho_total FAILED',
     &      'cal_emcluster','error in # phot','W')
      n_em_max = nelc + npho
      IF (n_em_max.EQ.0) THEN
        GOTO 999
      ELSEIF (n_em_max.GT.limit) THEN
        n_em_max = limit
      ENDIF
      DO i = 1,nslink
        DO k = 1,ntyp_max
          em_links(k,i) = 0
        ENDDO
        links(i) = 0
      ENDDO
      DO k = 1,ntyp_max
        nelph(k) = 0
      ENDDO

C-        *** obtain vertex for detector eta calculation ***
      CALL reco_version(version,reco_pass)
      CALL vzero(zv,wanted)
      CALL vzero(dz,wanted)
      CALL vertex_info(wanted,nz,info,yep)
      IF (.NOT.yep) THEN
        CALL errmsg('problem getting vertex','cal_emcluster',
     &          'vertex set to zero','W')
        nz = 1
        zv(1) = 0.
        dz(1) = 100.
      ELSE
        DO k = 1,min(wanted,nz)
          zv(k) = info(1,k)
          dz(k) = info(2,k)
        ENDDO
      ENDIF

C-        *** Et order em cluster banks ***
      lpelc = gzpelc()
      IF(lpelc.NE.0) THEN
        CALL zsort(ixcom,lpelc,7)
        lpelc = gzpelc()
        CALL ztopsy(ixcom,lpelc)
      ENDIF
      lppho = gzppho()
      IF(lppho.NE.0) THEN
        CALL zsort(ixcom,lppho,7)
        lppho = gzppho()
        CALL ztopsy(ixcom,lppho)
      ENDIF

C-        *** obtain lists of links to em clusters of types requested ***
      ntyp = 1
      IF (selection(1).LT.0) THEN
        ename = select_names(ntyp)
        DO WHILE((ename(1:1).NE.' ').AND.(ntyp.LT.ntyp_max))
          ntyp = ntyp + 1
          ename = select_names(ntyp)
        ENDDO
        IF ((ntyp.EQ.ntyp_max).AND.(ename(1:1).NE.' ')) THEN
          CALL errmsg('max em types','cal_emcluster',
     &            'types requested is maximum','W')
        ELSE
          ntyp = ntyp - 1
        ENDIF
        DO k = 1,ntyp
          CALL gtslink(select_names(k),limit,nelph(k),links)
          DO i = 1,nelph(k)
            em_links(k,i) = links(i)
          ENDDO
        ENDDO
      ELSE
        IF (selection(1).NE.0) THEN
          DO WHILE((selection(ntyp).NE.0).AND.(ntyp.LT.ntyp_max))
            ntyp = ntyp + 1
          ENDDO
          IF ((ntyp.EQ.ntyp_max).AND.(selection(ntyp).NE.0)) THEN
            CALL errmsg('max em types','cal_emcluster',
     &        'types requested is maximum','W')
          ELSE
            ntyp = ntyp - 1
          ENDIF
        ENDIF
        DO k = 1,ntyp
          lpelc = gzpelc()
          n = 2*k - 1
          DO i = 1,min(limit,nelc)
            CALL check_em_quality(lpelc,selection(k),ok)
            IF (ok) THEN
              nelph(n) = nelph(n) + 1
              em_links(n,nelph(n)) = lpelc
              lpelc = lq(lpelc)
            ENDIF
          ENDDO
          lppho = gzppho()
          n = 2*k
          DO i = 1,min(limit,npho)
            CALL check_em_quality(lppho,selection(k),ok)
            IF (ok) THEN
              nelph(n) = nelph(n) + 1
              em_links(n,nelph(n)) = lppho
              lppho = lq(lppho)
            ENDIF
          ENDDO
        ENDDO
        ntyp = n
      ENDIF

C-      *** merge arrays for various em types and set type bit patterns ***
      DO i = 1,ntyp
        k_em(i) = 1
      ENDDO
      done = .false.
      n_em = 0
      DO WHILE((n_em.LT.n_em_max).AND..NOT.done)
        n_em = n_em + 1
        et_max = 0.0
        DO k = 1,ntyp
          et_em = q(em_links(k,k_em(k)) + 7)
          IF (et_em.GT.et_max) THEN
            et_max = et_em
            em_ident(10,n_em) = em_links(k,k_em(k))
          ENDIF
        ENDDO
        DO k = 1,ntyp
          IF ((em_links(k,k_em(k)).eq.em_ident(10,n_em)).and.
     &          (em_ident(10,n_em).ne.0)) then
            k_em(k) = k_em(k) + 1
            em_ident(2,n_em) = em_ident(2,n_em) + 2**(k-1)
          ENDIF
        ENDDO
        ok = .false.
        IF (em_ident(10,n_em).ne.0) then    !*** get reco quantities  ***
          ok = .true.                       !*** for good em clusters ***
          em_e(1,n_em) = q(em_ident(10,n_em) + 3)
          em_e(2,n_em) = q(em_ident(10,n_em) + 4)
          em_e(3,n_em) = q(em_ident(10,n_em) + 5)
          em_e(4,n_em) = q(em_ident(10,n_em) + 6)
          em_e(5,n_em) = q(em_ident(10,n_em) + 7)
          em_e(10,n_em) = q(em_ident(10,n_em) + 14)
          lcacl = lq(em_ident(10,n_em) - 2)
          em_e(11,n_em) = q(lcacl + 24)
          em_e(12,n_em) = q(lcacl + 26)
          em_e(13,n_em) = q(lcacl + 25)
          em_e(14,n_em) = q(lcacl + 27)
          IF (version.LE.11) THEN
            em_e(15,n_em) = q(lcacl + 29)
            em_e(17,n_em) = q(lcacl + 31)
          ELSE
            em_e(15,n_em) = q(lcacl + 29)
            em_e(16,n_em) = q(lcacl + 33)
            em_e(17,n_em) = q(lcacl + 31)
          ENDIF
          em_eta(1,n_em) = q(em_ident(10,n_em) + 9)
          theta = 2.*atan(exp(-em_eta(1,n_em)))
          CALL det_eta(zv(1),theta,em_eta(2,n_em))
          em_eta(3,n_em) = q(em_ident(10,n_em) + 19)
          phi = q(em_ident(10,n_em) + 10)
          em_phi(1,n_em) = phi
          CALL vtx_em(em_ident(10,n_em),lvtxt,sig1,sig2)
          em_phi(2,n_em) = sig1
          IF (lvtxt.GT.0) em_phi(3,n_em) = q(lvtxt + 12)
          CALL cleanem(em_ident(10,n_em),1,ok,status)
          em_ident(1,n_em) = status
          CALL cleanem_cquans(ncvar,cquan)
          em_qual(2,n_em) = cquan(4)
          em_qual(3,n_em) = cquan(9)
          em_qual(4,n_em) = cquan(13)
          CALL cleanem_tquans(ntvar,tquan)
          em_qual(5,n_em) = tquan(12)
          em_qual(6,n_em) = tquan(13)
          IF (tquan(15).GT.0) THEN
            em_qual(7,n_em) = tquan(15)
          ELSEIF (lvtxt.GT.0) THEN
            em_qual(7,n_em) = -1.*q(lvtxt + 20)
          ENDIF
          em_qual(8,n_em) = tquan(14)
          lcash = lq(lcacl-2)
          IF (lcash.GT.0) em_qual(9,n_em) = iq(lcash + 2)
          CALL uhtoc(iq(em_ident(10,n_em)-4),4,bank,4)
          lztrk = lq(em_ident(10,n_em) - 3)
          IF (lztrk.GT.0) THEN
            lzfit = lq(lztrk - 1)
            em_eta(4,n_em) = q(lzfit + 13)
            em_eta(5,n_em) = q(lzfit + 18)
            em_eta(7,n_em) = q(lzfit + 15)
            em_eta(8,n_em) = q(lzfit + 19)
            em_eta(10,n_em) = q(lzfit + 34)
            em_phi(4,n_em) = q(lzfit + 11)
            em_phi(5,n_em) = q(lzfit + 17)
            em_phi(6,n_em) = q(lzfit + 12)
            em_phi(7,n_em) = q(lzfit + 19)
            phi = q(lzfit + 10)
            theta = q(lzfit + 13)
            st = sin(theta)
            vin(1) = q(lzfit + 11)
            vin(2) = q(lzfit + 12)
            vin(3) = q(lzfit + 15)
            vin(4) = st*cos(phi)
            vin(5) = st*sin(phi)
            vin(6) = cos(theta)
            CALL extcyl(vin,vout,26.3,ier)
            em_qual(23,n_em) = vout(3)
            CALL extcyl(vin,vout,36.85,ier)
            em_qual(24,n_em) = vout(3)
            CALL extcyl(vin,vout,47.4,ier)
            em_qual(25,n_em) = vout(3)
          ELSEIF (bank.EQ.'PPHO'.and.lcacl.GT.0) THEN
            phi = q(lcacl + 12)
            theta = q(lcacl + 11)
            st = sin(theta)
            vin(1) = q(lcacl + 14)
            vin(2) = q(lcacl + 15)
            vin(3) = q(lcacl + 16)
            vin(4) = st*cos(phi)
            vin(5) = st*sin(phi)
            vin(6) = cos(theta)
            CALL extcyl(vin,vout,26.3,ier)
            em_qual(23,n_em) = vout(3)
            CALL extcyl(vin,vout,36.85,ier)
            em_qual(24,n_em) = vout(3)
            CALL extcyl(vin,vout,47.4,ier)
            em_qual(25,n_em) = vout(3)
          ENDIF
          ltrdt = 0
          IF (bank.EQ.'PPHO') THEN
            em_ident(4,n_em) = 0
            IF (iq(lcacl+1).ge.6) ltrdt = lq(lcacl-5)
          ELSE
            em_ident(4,n_em) = -1
            IF (lztrk.GT.0) THEN
              ltrdt = lq(lztrk - 9)
            ELSE
              em_ident(4,n_em) = -100     ! crazy PELC
              CALL errmsg('no ZTRAK','cal_emcluster','lztrk=0','W')
            ENDIF
          ENDIF
          em_ident(5,n_em) = int(tquan(22))      ! acceptance
          em_qual(19,n_em) = tquan(23)           ! efficiency
          CALL trd_num_layers(em_ident(10,n_em),geometry,badtrack,
     &        em_ident(3,n_em))
          IF (ltrdt.GT.0) THEN
            ltdst = gztdst()
            IF (ltdst.GT.3) THEN
              em_qual(17,n_em) = q(ltdst + 16)     ! truncated sum
              em_qual(21,n_em) = q(ltdst + 11)     ! likelihood from TRD alone
              em_qual(22,n_em) = q(ltdst + 9)      ! likelihood from TRD and CDC
              DO layer = 1,3
                em_qual(13+layer,n_em) = q(ltdst + 11 + layer)
                ltana = lq(ltdst - layer)
                IF (ltana.GT.0) THEN
                  em_qual(10+layer,n_em) = float(iq(ltana + 304))
                  em_qual(25+layer,n_em) = float(iq(ltana + 305))
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          em_qual(30,n_em) = tquan(24)
          em_qual(31,n_em) = tquan(25)
          em_qual(32,n_em) = tquan(26)
          em_qual(33,n_em) = tquan(27)
          em_qual(34,n_em) = tquan(28)
          em_qual(35,n_em) = tquan(29)
          em_qual(36,n_em) = tquan(30)
          em_qual(37,n_em) = tquan(31)
          IF (abs(em_eta(2,n_em)).lt.1.25) then
            dummy = elike_set_mask_cc(mask_trdoff)
            elike_nt = elike(em_ident(10,n_em),fhadhi_cc,ier)
            dummy = elike_set_mask_cc(mask_all)
            elikehi = elike(em_ident(10,n_em),fhadhi_cc,ier)
          ELSE
            dummy = elike_set_mask_ec(mask_trdoff)
            elike_nt = elike(em_ident(10,n_em),fhadhi_ec,ier)
            dummy = elike_set_mask_ec(mask_all)
            elikehi = elike(em_ident(10,n_em),fhadhi_ec,ier)
          ENDIF
          em_ident(6,n_em) = ier
          em_qual(40,n_em) = elike_nt
          em_qual(41,n_em) = elikehi
        ELSE
          done = .true.
          n_em = n_em - 1
        ENDIF
      ENDDO

  999 RETURN
      END
