      SUBROUTINE cal_jets(selection,select_name,limit,njets,jet_e,
     &        jet_eta,jet_phi,jet_qual,jet_ident)

C----------------------------------------------------------------------
C-   Purpose and Methods :  obtain information of reconstructed jets
C-
C-   Inputs:
C-       selection(10)    I  specifies template for jet reconstruction algorithm
C-                            (4 = NN; 3 = 0.3; 2 = 0.5; 1 = 0.7; 5 = 1.0)
C-                            Switch:
C-                              selection(1) < 0 : get jets from gtslink
C-                                specified by select_name
C-       select_name(10)  S  name of good jets type from gtslink
C-       limit            I  maximum number of jets
C-
C-   Outputs:
C-       njets            I  # of jets in event
C-       jet_e(50,20)     R  energy vector of jets
C-                (1,#) = Ex
C-                (2,#) = Ey
C-                (3,#) = Ez
C-                (4,#) = energy
C-                (5,#) = transverse energy of jet
C-                6 thru 50 spare
C-       jet_eta(10,20)    R  physics and detector eta of jets
C-                (1,#) = physics eta
C-                (2,#) = detector eta
C-                 3 thru 10 spare
C-       jet_phi(10,20)    R  phi of jets
C-                 1 thru 10 spare
C-       jet_qual(50,20)   R  quality of jets
C-                (1,#) = EM fraction of Et
C-                (3,#) = rms in eta
C-                (4,#) = rms in phi
C-                (7,#) = ICD fraction of Et
C-                (8,#) = CH fraction of Et
C-                (9,#) = (Et leading cell)/(Et 2nd leading cell)
C-                2, 5, 6, 10 thru 50 spare
C-       jet_ident(20,20)   I  quality and id of jets
C-                (1,#) = merge/split flag
C-                (3,#) = # tracks in road
C-                (4,#) = # cells above threshold
C-                (10,#) = link to JETS bank
C-                2, 5 thru 9, 11 thru 50 spare
C-
C-   Created  May-12-1993  Bob Kehoe
C-   Updated  Mar-27-1994  Bob Kehoe -- use gtslink, set_caph_alg,
C-                                       vertex_info and det_eta
C-   Updated  May-23-1994  Bob Kehoe -- get isajet vertex for v11
C-                                       showerlib, use zlinka.inc
C-   Updated  Oct-07-1994  Bob Kehoe -- allow better descrimination of showerlib
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER wanted,ntyp_max
      PARAMETER (wanted = 3)
      PARAMETER (ntyp_max = 10)
      INTEGER nz,i,j,k,selection(10),njets,iword,ntraks,limit
      INTEGER links(nslink),ljets,lcaph,gzcaph,jet_ident(20,limit)
      REAL zv(wanted),dz(wanted),info(3,wanted)
      REAL jet_eta(10,limit),jet_phi(10,limit)
      REAL jet_e(50,limit),jet_qual(50,limit),theta
      LOGICAL yep
      CHARACTER*8 select_name(10)
      BYTE bvert(4)
      EQUIVALENCE (bvert,iword)

C----------------------------------------------------------------------
C-        *** initialize variables ***
      DO i = 1,limit
        DO k = 1,50
          jet_e(k,i) = 0.
        ENDDO
        DO k = 1,10
          jet_eta(k,i) = 0.
        ENDDO
        DO k = 1,10
          jet_phi(k,i) = 0.
        ENDDO
        DO j = 1,50
          jet_qual(j,i) = 0.
        ENDDO
        DO k = 1,20
          jet_ident(k,i) = 0
        ENDDO
      ENDDO
      njets = 0
      DO i = 1,nslink
        links(i) = 0
      ENDDO
C-        *** get links for Et sorted jets banks ***
      IF (selection(1).LT.0) THEN
        CALL gtslink(select_name(1),limit,njets,links)
      ELSE
        CALL set_caph_alg(selection(1))
        lcaph = gzcaph()
        IF (lcaph.NE.0) THEN
          ljets = lq(lcaph - izjets)
          IF (ljets.GT.0) THEN
            IF (iq(ljets+1).lt.3) call errmsg('cant reject noisy jets',
     &        'cal_jets','ancient jets bank','W')
            CALL zsort(ixcom,ljets,6)
            ljets = lq(lcaph - izjets)
            CALL ztopsy(ixcom,ljets)
            ljets = lq(lcaph - izjets)
            DO WHILE(ljets.NE.0)
              njets = njets + 1
              links(njets) = ljets
              ljets = lq(ljets)
            ENDDO
            IF (njets.NE.iq(lcaph+3)) call errmsg('njets screwed up',
     &        'cal_jets','njets and iq(lcaph+3) dont agree','W')
            IF (njets.GT.limit) THEN
              CALL errmsg('too many jets','cal_jets',' ','W')
              njets = limit
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF (njets.EQ.0) GOTO 999

C-        *** obtain vertex for detector eta calculation ***
      CALL vzero(zv,wanted)
      CALL vzero(dz,wanted)
      CALL vertex_info(wanted,nz,info,yep)
      IF (.NOT.yep) THEN
        CALL errmsg('problem getting vertex','cal_jets',
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

C-        ***  get reconstructed quantities of jets  ***
      DO i = 1,njets
        jet_e(1,i) = q(links(i) + 2)
        jet_e(2,i) = q(links(i) + 3)
        jet_e(3,i) = q(links(i) + 4)
        jet_e(4,i) = q(links(i) + 5)
        jet_e(5,i) = q(links(i) + 6)
        jet_eta(1,i) = q(links(i) + 9)
        theta = 2.*atan(exp(-jet_eta(1,i)))
        CALL det_eta(zv(1),theta,jet_eta(2,i))
        jet_phi(1,i) = q(links(i) + 8)
        jet_qual(1,i) = q(links(i) + 14)
        jet_qual(3,i) = q(links(i) + 12)
        jet_qual(4,i) = q(links(i) + 13)
        jet_qual(7,i) = q(links(i) + 17)
        jet_qual(8,i) = q(links(i) + 18)
        jet_qual(9,i) = q(links(i) + 19)
        jet_ident(1,i) = iq(links(i) + 15)
        iword = iq(links(i) + 27)
        ntraks = bvert(2)
        jet_ident(3,i) = ntraks
        jet_ident(4,i) = iq(links(i) + 16)
        jet_ident(10,i) = links(i)
      ENDDO

  999 IF (selection(1).GT.0) CALL reset_caph

      RETURN
      END
