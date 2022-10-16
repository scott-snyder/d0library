      LOGICAL FUNCTION fix_jet_et()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modify word 6 of JETS bank (ET) to use
C-                         an alternate definition of jet ET.  Also, remove CH
C-                         from jets dominated by CH energy to remove main-ring
C-                         backgrounds to jets.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  FIX_JET_ET_INI - Initialization.
C-
C-
C-   JET_ET_DEFINITION parameter:
C-
C-   0 - Do not modify jet ET (default is scalar sum ET).
C-   1 - pt = sqrt(px**2 + py**2)
C-   2 - e * sin(theta) = e * pt / p
C-   3 - sqrt(e**2 - pz**2)
C-
C-   Generally, methods 1-3 give increasing values of ET.  The default
C-   definition of ET (scalar sum ET) is the highest.
C-
C-   Created  27-Dec-1993   Herbert Greenlee
C-   Updated  Feb-12-1995   Bob Kehoe   -- allow removal of CH/OH energy from
C-                                         jets in various mainring states.
C-                                         CH fraction set to -999. as flag.
C-   Updated  Dec-04-1995   Bob Kehoe   -- explicitly calculate EM, ICD
C-                                         fractions, set CH fraction = 0 to
C-                                         avoid blowing up jet (and missing_et)
C-                                         corrections.
C----------------------------------------------------------------------

      IMPLICIT NONE
      LOGICAL fix_jet_et_ini,mrveto,do_fix_jet_et
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INTEGER jet_et_definition
      REAL px,py,pz,e,et,max_ch_fraction,chfrac,min_jet_et,em_et
      real icd_et,ch_et
      INTEGER ier,corr_switch(5),num_mr,nsw,k,jet_cut_switch
      INTEGER lproc, lcaph, ljets
      INTEGER gzproc
      CHARACTER*32 mr_names(5)
      LOGICAL first,mainring_veto
      DATA first/.true./

C----------------------------------------------------------------------
C- Always return .TRUE. (unless rejecting event if bad CH jet. see below)
C-
      fix_jet_et = .true.
C-
C- Want to modify jet ET's?
C-
      IF (.NOT.do_fix_jet_et) GOTO 999
C-
C- Yes, loop over all CAPH and JETS banks.  Don't use SET_CAPH etc.
C-
      lproc = gzproc()
      IF(lproc.EQ.0)go to 999
      lcaph = lq(lproc-izcaph)
      DO WHILE (lcaph.GT.0)
        ljets = lq(lcaph-izjets)
        DO WHILE (ljets.GT.0)
          px = q(ljets+2)
          py = q(ljets+3)
          pz = q(ljets+4)
          e = q(ljets+5)
          et = q(ljets+6)
          IF(jet_et_definition.EQ.1)THEN
            et = sqrt(px**2 + py**2)
          ELSEIF(jet_et_definition.EQ.2)THEN
            et = e * sqrt((px**2 + py**2)/(px**2 + py**2 + pz**2))
          ELSEIF(jet_et_definition.EQ.3)THEN
            IF(e.GT.pz)THEN
              et = sqrt((e-pz) * (e+pz))
            ELSE
              et = 0.
            ENDIF
          ENDIF
          chfrac = q(ljets+18)
          IF (chfrac.GT.max_ch_fraction) THEN
            DO k = 1,num_mr
              mainring_veto = mrveto(mr_names(k))
              IF (mr_names(k).EQ.'GOOD_BEAM') mainring_veto = .NOT.
     &            mainring_veto
              IF (mainring_veto) THEN
                IF (corr_switch(k).EQ.1) THEN
                  CALL errmsg('removing CH from a jet','FIX_JET_ET',' ',
     &              'W')
                  em_et = q(ljets+14)*q(ljets+6)
                  icd_et = q(ljets+17)*q(ljets+6)
                  ch_et = q(ljets+18)*q(ljets+6)
                  et = et*(1.-chfrac)
                  IF ((jet_cut_switch.EQ.1.OR.jet_cut_switch.EQ.3).AND.
     &                    (et.LT.min_jet_et)) THEN
                    CALL errmsg('deleting vestige of CH jet',
     &                    'FIX_JET_ET',' ','W')
                    CALL mzdrop(ixcom,ljets,' ')
                  ELSE      ! Mark bank for dropping
                    IF (q(ljets+6).gt.0) then
                      q(ljets+2) = px*(et/q(ljets+6))
                      q(ljets+3) = py*(et/q(ljets+6))
                      q(ljets+4) = pz*(et/q(ljets+6))
                      q(ljets+5) = e*(et/q(ljets+6))
                    ENDIF
                    q(ljets+6) = et
                    q(ljets+14) = em_et/et
                    q(ljets+17) = icd_et/et
                    q(ljets+18) = 0.0
                  ENDIF
                ELSEIF (corr_switch(k).EQ.2) THEN
                  CALL errmsg(
     &              'annihilation of highly CH jets commencing!',
     &              'FIX_JET_ET',' ','W')
                  CALL mzdrop(ixcom,ljets,' ')
                ELSEIF (corr_switch(k).EQ.3) THEN
                  CALL errmsg('reject event with CH jet','FIX_JET_ET',
     &              ' ','W')
                  fix_jet_et = .false.
                  GOTO 999
                ENDIF
              ENDIF
            ENDDO
          ELSE
            q(ljets+6) = et
          ENDIF
          IF (jet_cut_switch.EQ.2.OR.jet_cut_switch.EQ.3) THEN
            IF (et.LT.min_jet_et) THEN
              CALL errmsg('deleting low Et split jets',
     &            'FIX_JET_ET',' ','W')
              CALL mzdrop(ixcom,ljets,' ')
            ENDIF
          ENDIF       ! Mark bank for dropping
          ljets = lq(ljets)
        ENDDO
        lcaph = lq(lcaph)
      ENDDO
      GOTO 999

      ENTRY fix_jet_et_ini()
C-
C- Initialization entry point
C-
      fix_jet_et_ini = .true.
      IF(first) THEN
C-
C- Read RCP parameters from FIX_JET_ET_RCP.
C-
        CALL ezpick_nomsg('FIX_JET_ET_RCP', ier)
        IF(ier.NE.0)THEN
          CALL inrcp('FIX_JET_ET_RCP', ier)
          CALL ezpick_nomsg('FIX_JET_ET_RCP', ier)
        ENDIF
        IF(ier.EQ.0)CALL ezget_l('do_fix_jet_et',do_fix_jet_et,ier)
        IF(ier.EQ.0)CALL ezget_i('JET_ET_DEFINITION',jet_et_definition,
     &    ier)
        IF(ier.EQ.0)CALL ezget('max_ch_fraction',max_ch_fraction,ier)
        IF(ier.EQ.0)CALL ez_get_chars('mr_names',num_mr,mr_names,ier)
        IF(ier.EQ.0)CALL ezgeta_i('CORR_SWITCH',0,0,0,nsw,ier)
        IF(ier.EQ.0)CALL ezgeta('CORR_SWITCH',1,nsw,1,corr_switch,ier)
        IF(ier.EQ.0)CALL ezget('min_jet_et',min_jet_et,ier)
        IF(ier.EQ.0)CALL ezget_i('jet_cut_switch',jet_cut_switch,ier)
        CALL ezrset
        IF (ier.NE.0) CALL errmsg('Error reading FIX_JET_ET_RCP',
     &    'FIX_JET_ET',' ','F')
        IF(jet_et_definition.EQ.0)THEN
          CALL errmsg('Jet ET definition will not be modified',
     &      'FIX_JET_ET', ' ', 'I')
        ELSEIF(jet_et_definition.EQ.1)THEN
          CALL errmsg('Jet ET''s will be changed to PT',
     &      'FIX_JET_ET', ' ', 'W')
        ELSEIF(jet_et_definition.EQ.2)THEN
          CALL errmsg('Jet ET''s will be changed to E*sin(theta)',
     &      'FIX_JET_ET', ' ', 'W')
        ELSEIF(jet_et_definition.EQ.3)THEN
          CALL errmsg(
     &       'Jet ET''s will be changed to SQRT(PT**2 + MASS**2)',
     &       'FIX_JET_ET', ' ', 'W')
        ELSE
          CALL errmsg('Bad parameter value', 'FIX_JET_ET', ' ', 'F')
          fix_jet_et_ini = .false.
        ENDIF
        first=.false.
      ENDIF

  999 RETURN
      END
