      SUBROUTINE correct_jets_reco_version(reco,istat)

C----------------------------------------------------------------------
C-   Purpose and Methods : Determine reco version or use default in
C-        qcd_jet_correction_rcp
C-
C-   Outputs :
C-            reco          I   -- version of reconstruction to use for jets
C-            istat         I   -- = 1 if error
C-
C-   Controls:    QCD_JET_CORRECTION_RCP
C-
C-   Created  Sep-05-1995   Bob Kehoe
C-   Updated  29-NOV-1995   David E. Cullen-Vidal   Will only write 'No Reco
C-                                                  version' message once.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER istat,pass,version,RECO,lrcp,ier,reco_default
      LOGICAL first,ok,once
      DATA first /.true./
      DATA ONCE /.TRUE./
C----------------------------------------------------------------------
C-        *** Initialize ***
      istat = 0
      IF (first) THEN
        CALL ezloc('QCD_JET_CORRECTION_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('QCD_JET_CORRECTION_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('QCD_JET_CORRECTION_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF (ier.NE.0) THEN
            CALL errmsg('RCP not found','correct_jets_reco_version',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN    ! *** read in RCP parameters ***
          CALL ezget('RECO_DEFAULT',reco_default,ier)
          CALL ezrset
          IF (ier.NE.0) THEN
            CALL errmsg('RCP error','correct_jets_reco_version',
     &        'Read error:abort ','F')
            ier = -3
            GOTO 999
          ENDIF
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'correct_jets_reco_version','NO RCP file to work with','F')
          ier = -2
          GOTO 999
        ENDIF
        first = .false.
      ENDIF
      reco = reco_default

C-      *** determine reco version or take default ***
      CALL reco_version(version,pass)
      IF (version .GE. 8) THEN
        reco = version
      ELSE
        IF (ONCE) THEN
          ONCE = .FALSE.
          CALL errmsg('No Reco version',
     &      'CORRECT_JETS_RECO_VERSION','Using RECO_DEFAULT','W')
        ENDIF
      ENDIF

  999 RETURN
      END
