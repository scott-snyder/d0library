      SUBROUTINE JET_CONE_CORR(LJETSIN,DO_ZSP,DO_UND,DO_OOC,CRYO_CORR,
     1  ZVERT,ISYS,EOUT,ETOUT,ETAOUT,PHIOUT,OUTVEC,MATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Find a matching 0.7 cone jet and then apply standard correction
C-      If not match found apply standard correction
C-
C-   Inputs  : LJETSIN:  Pointer to jets bank
C-             ZVERT  : z-vertex of event.
C-             ISYS   : 0- Nominal correction, 1-Low correction, 2-High
C-                      Used to calculate errors
C-             DO_ZSP : True = Correct for Zero Suppression
C-             DO_UND : True = Correct for Underlying event contribution
C-             DO_OOC : True = Correct for Out of cone
C-             CRYO_CORR(3) : 1/2/3 for em-scale corrections for ECN/CC/ECS
C-
C-   Outputs : EOUT   : corrected energy
C-             ETOUT  : corrected Et
C-             ETAOUT : corrected Eta
C-             PHIOUT : corrected phi
C-             OUTVEC(3) : corrected PX, PY, PZ
C-             Match  : True if matched 0.7 cone jet found
C-
C-   Created  30-APR-1993   Serban Protopopescu
C-   Updated   8-SEP-1993   Meenakshi Narain  make it compatible to standard
C-                          QCD JET correction routine
C-   Updated  14-SEP-1993   Meenakshi Narain
C-                            change argument list (input/output)
C-   Updated   7-OCT-1993   Dhiman Chakraborty
C-                            minor bug-fix
C-   Updated  14-OCT-1993   S.J.Wimpenny - change order of argument list
C-                          to be compatible with QCD_JET_CORRECTION
C-   Updated  29-OCT-1993   Dhiman Chakraborty
C-                          Scale like an electron if jet has EMF>0.9
C-                          Look for match in circular cone instead of square
C-   Updated  20-NOV-1993   S.J.Wimpenny
C-                          Picks up Calorimter em-scale corrections from
C-                          s/r calling arguments, code tidied up.
C-                 ******* Note: I/O list has changed *******
C-   Updated  08-DEC-1993   Dhiman Chakraborty
C-                          Match with 0.7 cone jet is not sought
C-                          if input jet has EMF>0.9
C-   Updated   9-NOV-1995   Brajesh C Choudhary   Fixed problem with 
C-                          QCD_JET_CORRECTION_3 entry. Had to introduce
C-                          two real variables JET_ICD and JET_CHF to
C-                          make the routine work.
C---------------------------------------------------------------------- 
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      LOGICAL DO_OOC, DO_UND, DO_ZSP
      LOGICAL MATCH
C
      INTEGER ISYS
      INTEGER LCAPH,ICONE,ICRYO
      INTEGER GZJETS,LJETS,IER,LJETSIN
C
      REAL    ETA,PHI,EMF,ZVERT,EOUT,ETOUT,ETAOUT,PHIOUT,EMFOUT
      REAL    TEMPLATE(3),ENERGY,DPHI,DETA,DR,OUTVEC(3)
      REAL    JET_E,JET_ET,JET_ETA,JET_EMF,JET_SIZE,CONE_USED,THETA
      REAL    CORR_FAC,CRYO_CORR(3),ETAD
      REAL    JET_ICD, JET_CHF
C
      DATA TEMPLATE/ 1.,6.,0.7/     ! CONE R=0.7
C----------------------------------------------------------------------
      MATCH = .FALSE.
      EMF  = Q(LJETSIN+14)
      IF(EMF.GT.0.9)THEN
        THETA = Q(LJETSIN+7)
        CALL DET_ETA(ZVERT,THETA,ETAD)
        ICRYO = 2
        IF(ETAD.LT.-1.2)ICRYO = 1
        IF(ETAD.GT.1.2)ICRYO = 3
        CORR_FAC = CRYO_CORR(ICRYO)
        EOUT     = CORR_FAC*Q(LJETSIN+5)
        ETOUT    = CORR_FAC*Q(LJETSIN+6)
        ETAOUT   = Q(LJETSIN+9)
        PHIOUT   = Q(LJETSIN+8)
        EMFOUT   = Q(LJETSIN+14)
        CALL VSCALE(Q(LJETSIN+2),CORR_FAC,OUTVEC,3)
      ELSE
        ENERGY = Q(LJETSIN+5)
        ETA    = Q(LJETSIN+9)
        PHI    = Q(LJETSIN+8)
        CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
        LJETS=GZJETS()
        DO WHILE (LJETS.GT.0)
          DPHI=ABS(PHI-Q(LJETS+8))
          DETA=ABS(ETA-Q(LJETS+9))
          IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
          DR = SQRT(DPHI**2 + DETA**2)
          IF (DR.LT.0.15) THEN
            ENERGY=Q(LJETS+5)
            MATCH = .TRUE.
            GOTO 100
          ENDIF
          LJETS=LQ(LJETS)
        ENDDO
  100   CONTINUE
        IF (MATCH) THEN
          CALL QCD_JET_CORRECTION( LJETS, DO_ZSP, DO_UND, DO_OOC, ZVERT,
     &        ISYS, EOUT, ETOUT, ETAOUT, IER )
          CORR_FAC = EOUT/ENERGY
          PHIOUT = Q(LJETS+8)
          EMFOUT = Q(LJETS+14)
          CALL VSCALE(Q(LJETS+2),CORR_FAC,OUTVEC,3)
        ELSE
          JET_E      = Q(LJETSIN+5)
          JET_ET     = Q(LJETSIN+6)
          JET_ETA    = Q(LJETSIN+9)
          JET_SIZE   = SQRT(Q(LJETSIN+12)**2 + Q(LJETSIN+13)**2)
          JET_EMF    = Q(LJETSIN+14)
          JET_ICD    = Q(LJETSIN+17)
          JET_CHF    = Q(LJETSIN+18)
          LCAPH      = LQ(LJETSIN+1)         ! Pointer to CAPH algorithm bank
          CONE_USED  = Q(LCAPH+6)
          ICONE      = 3         ! use correction for cone 0.7
          CALL QCD_JET_CORRECTION_3( JET_E, JET_ET, JET_ETA, JET_EMF,
     &      JET_SIZE, CONE_USED, ICONE, JET_ICD, JET_CHF )
          CALL QCD_JET_CORRECTION( 0, DO_ZSP, DO_UND, DO_OOC, ZVERT,
     &        ISYS, EOUT, ETOUT, ETAOUT, IER )
          CORR_FAC = EOUT/ENERGY
          PHIOUT = Q(LJETSIN+8)
          EMFOUT = Q(LJETSIN+14)
          CALL VSCALE(Q(LJETSIN+2),CORR_FAC,OUTVEC,3)
        ENDIF
        CALL RESET_CAPH
      ENDIF
C
  999 RETURN
      END
