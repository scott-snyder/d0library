      LOGICAL FUNCTION MU2_B()
!------------------------------------------------------------------------
!-   Purpose: Flags dimuon events for B physics.
!-
!-   Author:  Andrzej Zieminski
!-   Created: 20 Nov 1992
!-   Revised: 25 Jun 1993  Daria Zieminska - use T0 as cosmic check
!-            20 Jul 1993  G. Álvarez & C. R. Murphy - eliminated RCP
!-                         dependence; tested code - passes 6% of data
!-             7 Feb 1994  G. Álvarez - protect against A stubs
!-            18 Jun 1994  G. Álvarez - protect against no PMUO, MUON
!-                         and MUOT banks
!-            18 Oct 1994  G. Álvarez - Cal. confirm 1NN and scint. used;
!-                         for pre-scint. changed opening angle cut from
!-                         160 to 170 degrees
!-            21-DE!-1994  D. Vititoe - All the Same cuts, code was 
!-                         re-structured.
!-            04-MAR-1995  D. Vititoe - Added fract, and points on track
!-                         cut to reduce EF pass rate
!-            29-APR-1995  D. Vititoe - Fixed bug in scintillator cut.
!-                         JBIT begins counting at 1 not 0.
!-            20-OTC-1995  D. Vititoe - Filter Re-Designed for D0FIX'ed Data,
!-                         replace cal conf. with MTC cuts
!-
!------------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'

      INTEGER max
      PARAMETER(max=8) ! max allowed muon + 1

      INTEGER gzparh,lparh,nzbank,gzpmuo,lpmuo
      INTEGER i,nmuons,nmus,ifw4,run,event
      INTEGER k,muon(max),muot(max),ns,stub,nobj(10),id_obj(50)
      INTEGER ifw2,quad,ifw1
      INTEGER ngood_mu,good_pmuo(max),good_muot(max),nsamus

      REAL    etrack,et(50),hfract,pmu,ehad

      LOGICAL biton(50),noscint,mtc_pass,scint_bit16,scint_bit17
!----------------------------------------------------------------------
      MU2_B = .false.

!-- decode reco bits
      CALL unpack_reco_bits(nobj,id_obj,et,biton)
      IF (nobj(3) .LT. 2) RETURN

!-- reject event if more than 7 non-ASTUB reconstructed muons
      nmuons = 0
      lparh  = gzparh()
      lpmuo  = lq(lparh - izpmuo)
      IF (lpmuo .LE. 0.) RETURN
      nmuons = nzbank(ixcom,lpmuo)

      nmus = 0
      DO 10 k = 1, nmuons
        muon(nmus+1) = gzpmuo(k)
        IF (muon(nmus+1) .le. 0.) goto 10
        ns   = iq(muon(nmus+1) - 2)
        muot(nmus+1) = lq(muon(nmus+1) - ns - 1)
        IF (muot(nmus+1) .le. 0.) goto 10
        stub = iq(muot(nmus+1) + 4)
        IF (stub .ne. 5) nmus = nmus + 1
        IF (nmus.GT.7) RETURN
   10 CONTINUE
      IF ( nmus .LT. 2 ) RETURN

      CALL evntid(run,event)

!-- loop over muons
      ngood_mu = 0
      DO 20 i = 1, nmus
        quad = iq(muon(i)+7)

! ***   Make WAMUS and OVERLAP CUTS
        if(quad.lt.13) then
          ifw4 = mod(iq(muon(i) + 9),100)  ! quality of muon (IFW4)
          IF (ifw4 .GT. 1) GOTO 20
C
          etrack = q(muon(i) + 90)
          if(etrack.le.0.5) goto 20
C
          hfract = q(muon(i) + 94)
          if(hfract.le.0.5) goto 20
C
          !-- cosmic rejection
          !-- scintilator - note: 100 ns gate was centered at 50 ns from 74561
          !--                     and 70 ns from 79860
          IF (run .GE. 74561) THEN
            ifw2 = iq(muot(i) + 5)
            scint_bit16 = btest(ifw2,16)
            scint_bit17 = btest(ifw2,17)
            noscint = (scint_bit16 .AND. .not. scint_bit17)
            IF (noscint) GOTO 20
          ENDIF
C
          !-- EF Cuts
          if(quad.gt.4) then
            ifw1 = iq(muot(i) + 4)        ! \_ Require Three 
            if(mod(ifw1,10).ne.0) goto 20 ! /  Layers Hit
            if(etrack.le.0.8) goto 20
            if(hfract.le.0.78) goto 20
          endif

        else if(quad.eq.13 .or. quad.eq.14) then
          PMU    = ABS(Q(muon(i)+13))
          IF(PMU.LE.5.0) GO TO 20
C
          ! Must pass Ehad or MTC cuts
          EHAD   = Q(muon(i)+84)-Q(MUON(I)+79)
          etrack = q(muon(i) + 90)
          hfract = q(muon(i) + 94)
          mtc_pass = etrack.gt.0.8 .and. hfract.gt.0.78
          IF(EHAD.LE.1.5 .and. .not.mtc_pass) GO TO 20
C
          NSAMUS=IQ(MUOT(i)+2)
          IF(NSAMUS.LT.14) GO TO 20
C
        endif

        ngood_mu = ngood_mu + 1   !-- Good Muon so far
        good_pmuo(ngood_mu) = muon(i)
        good_muot(ngood_mu) = muot(i)

   20 CONTINUE
      IF(ngood_mu.LT.2) RETURN

!-- Found Good Muon Pair
      MU2_B = .true.
      RETURN
      END
