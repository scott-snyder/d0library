      SUBROUTINE L2_EM_PARSE_CUTS( TRACK_MATCH,SHAPE_CUTS,ISOL_CHECK,
     &  CHECK_TRACK,VETO_TRACK,CDC,FDC,ISO_CUT,ELECTRON,PHOTON,LONG,
     &  TRANS,TIGHT, CUTBITS)
C----------------------------------------------------------------------
C-
C-   Purpose And Methods : change the options into logical variables
C-   Inputs  : TRACK_MATCH determines type of track requirement
C-              'REQUIRE' require a track match to the cluster
C-              'VETO'    forbid a track match to the cluster
C-              'IGNORE'  don't care one way or the other
C-             SHAPE_CUTS determines type of cuts to use
C-              'ELECTRON' uses standard longitudinal and transverse cuts
C-                          (primary variables only)
C-              'PHOTON'   drops cuts on EM1 and EM2
C-              'E_LONG'     does Longitudinal only for electron
C-              'E_TRANS'    does transverse only for electron
C-              'E_IGNORE'   or anything else not among the above
C-                            does no shape cuts but calls it an electron
C-              'G_LONG'     does Longitudinal only for photon
C-              'G_TRANS'    does transverse only for photon
C-              'G_IGNORE'   or anything else not among the above
C-                            does no shape cuts but calls it a photon
C-              'xx_TIGHT'  use ALL variables; default is only 4 main variables:
C-                            FH1, EM3, SIG5-SIG3 or 5x5-3x3
C-              'P_...' also works same as 'G_....'
C-             ISOL_CHECK   isolation check
C-   Outputs : CHECK_TRACK  if need to do track match
C-             VETO_TRACK   if need to veto on tracking match
C-             CDC          if CDC should be tried
C-             FDC          if FDC should be tried
C-             ELECTRON    identified e: put only onto e candidate list
C-             PHOTON      probably a gamma: PHOTON shape cuts specified
C-                            put only onto gamma candidate list
C-             LONG        do at least some longitudinal shape cuts
C-             TRANS       do at least some transverse shape cuts
C-             TIGHT       use ALL variables, not just 4 primary cuts
C-             ISO_CUT     Isolation check
C-             CUTBITS     Bit mask describing configuration of cuts
C-
C-   Created 29-JAN-1992   James T. Linnemann
C_   Updated 25_FEB_1992   Yi  Xia  add Isolation cut.
C-   Updated  13-NOV-1993   James T. Linnemann  separate CDC and FDC control 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      CHARACTER*(*) SHAPE_CUTS,TRACK_MATCH
      CHARACTER*5 SUB,A_E_,A_P_,A_G_,A_LONG,A_TRANS,A_TIGHT
      CHARACTER*5 A_VETO,A_REQUIRE,A_ONLY,A_CDC,A_FDC
      PARAMETER( A_E_ = 'E_' )
      PARAMETER( A_P_ = 'P_' )
      PARAMETER( A_G_ = 'G_' )
      PARAMETER( A_LONG ='LONG'  )
      PARAMETER( A_TRANS = 'TRANS' )
      PARAMETER( A_TIGHT = 'TIGHT' )
      PARAMETER( A_VETO = 'VETO'  )
      PARAMETER( A_REQUIRE = 'REQUI'  )
      PARAMETER( A_ONLY ='_ONLY'  )
      PARAMETER( A_CDC = 'CDC'  )
      PARAMETER( A_FDC = 'FDC'  )
      INTEGER TRULEN
      LOGICAL ISOL_CHECK,ISO_CUT,SHAPE_CUTS_HAS,TRACK_MATCH_HAS
      LOGICAL TRANS,LONG,PHOTON,ELECTRON,TIGHT
      LOGICAL CHECK_TRACK,VETO_TRACK,REQ_TRACK,CDC,FDC
      INTEGER CUTBITS
C----------------------------------------------------------------------
C...statement function returns true if substring SUB found in SHAPE_CUTS
      SHAPE_CUTS_HAS(SUB) = INDEX(SHAPE_CUTS,SUB(1:TRULEN(SUB))).NE.0
C...statement function returns true if substring SUB found in TRACK_MATCH
      TRACK_MATCH_HAS(SUB) = INDEX(TRACK_MATCH,SUB(1:TRULEN(SUB))).NE.0
C----------------------------------------------------------------------
      VETO_TRACK = TRACK_MATCH_HAS(A_VETO)
      REQ_TRACK  = TRACK_MATCH_HAS(A_REQUIRE).OR.TRACK_MATCH_HAS(A_ONLY)
      CHECK_TRACK = VETO_TRACK.OR.REQ_TRACK     !will be false for IGNORE
      CDC = CHECK_TRACK.AND.(.NOT.TRACK_MATCH_HAS(A_FDC))
      FDC = CHECK_TRACK.AND.(.NOT.TRACK_MATCH_HAS(A_CDC))

      ISO_CUT = ISOL_CHECK
C
      ELECTRON = .FALSE.  ! defaults: type to be defined
      PHOTON = .FALSE.
      LONG = .TRUE.       ! both long and transverse cuts normally done
      TRANS = .TRUE.
      TIGHT = .FALSE.     ! but default is ONLY on primary variables
C
      IF (SHAPE_CUTS.EQ.'ELECTRON') THEN
        ELECTRON = .TRUE.
      ELSEIF (SHAPE_CUTS.EQ.'PHOTON') THEN
        PHOTON = .TRUE.
      ELSE
C
C...see if there's a tag to identify the particle type (mainly for ESUM)
        IF (SHAPE_CUTS_HAS(A_E_)) THEN
          ELECTRON = .TRUE.
        ELSEIF (SHAPE_CUTS_HAS(A_G_)) THEN
          PHOTON = .TRUE.
        ELSEIF (SHAPE_CUTS_HAS(A_P_)) THEN
          PHOTON = .TRUE.
        ENDIF
C
C...turn off some shape cuts
C
C...TIGHT means use ALL variables by default; can be modified by other tags
C
C...  if TIGHT is found, then this
        TIGHT = (SHAPE_CUTS_HAS(A_TIGHT))
        IF (SHAPE_CUTS_HAS(A_LONG)) THEN
          TRANS = .FALSE.
        ELSEIF (SHAPE_CUTS_HAS(A_TRANS)) THEN
          LONG = .FALSE.  !this won't work with this version
        ELSE              !treat as an "escape" candidate
          IF(.NOT.TIGHT) THEN
            LONG = .FALSE.     !unknown option (eg IGNORE, NONE ) turns both off
            TRANS = .FALSE.
          ENDIF
        ENDIF
      ENDIF
C
C...Now set bit mask describing cuts for L2EM bank
C
      CUTBITS = 0
      IF (ELECTRON)   CUTBITS = IBSET(CUTBITS,ELE_BIT)
      IF (PHOTON)     CUTBITS = IBSET(CUTBITS,GAM_BIT)
      IF (REQ_TRACK.AND.CDC)  CUTBITS = IBSET(CUTBITS,CDC_REQ_BIT)
      IF (VETO_TRACK.AND.CDC) CUTBITS = IBSET(CUTBITS,CDC_VETO_BIT)
      IF (REQ_TRACK.AND.FDC)  CUTBITS = IBSET(CUTBITS,FDC_REQ_BIT)
      IF (VETO_TRACK.AND.FDC) CUTBITS = IBSET(CUTBITS,FDC_VETO_BIT)
      IF (LONG)       CUTBITS = IBSET(CUTBITS,LONG_BIT)
      IF (TRANS)      CUTBITS = IBSET(CUTBITS,TRANS_BIT)
      IF (TIGHT)      CUTBITS = IBSET(CUTBITS,TIGHT_BIT)
      IF (ISO_CUT)    CUTBITS = IBSET(CUTBITS,ISO_BIT)
C
  999 RETURN
      END
