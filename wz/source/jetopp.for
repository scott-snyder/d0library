      SUBROUTINE JETOPP(P_REF,ET_SAME,ET_OPP,ET_BACK,DPHI_BACK,DR_MIN,
     &  ETJET_MINDR,NUMJETS,MUJET_PTREL,MUJET_ENRAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : examine jet activity around and opposite
C-                         to a reference direction
C-
C-   Inputs  : P_REF(3) = reference vector
C-   Outputs : ET_SAME = max jet Et in 30 degree CONE around ref.
C              ET_OPP  = max jet ET in +-30 degree WEDGE opposite ref.
C              ET_BACK = max jet ET in +-90 degree WEDGE opposite ref.
C              DPHI_BACK = phi angle (radians) between ET_BACK jet and
C                          ref.
C              DR_MIN = min dR Jet-reference.
C              ETJET_MINDR = Et of jet corresponding to DR_MIN.
C              NUMJETS = number of jets in the event.
C              MUJET_PTREL = Pt relative of ref to closest jet.
C              MUJET_ENRAL = E(muon)/(E(muon)+E(jet)) for closest jet.
C-   Controls:
C-
C-   Created  13-OCT-1992   Darien R. Wood
C-   Updated  15-NOV-1993   Cecilia E. Gerber: Included jet energy correction
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      REAL P_REF(3),ET_SAME,ET_OPP
      REAL REF_N(3),COSTH,COS30,TEMPLATE(3)
      REAL P(7),PHI,THETA,ETA,RT_MOD,PT_JET,X,Y,Z
      REAL ET_BACK,DPHI_BACK,PHI_REF
      REAL OLDE,OLDET,OLDETA,OLDEMF,OLDSIZ,OLDCONE,OLD_PTJET
      REAL NEW_JET_E,NEW_JET_ET,NEW_JET_ETA
      REAL DR,DETA,DPHI,DR_MIN,DPHI_MIN,P_REF_MOD,COSTHE_REF,THETA_REF
      REAL ETA_REF,ETJET_MINDPHI,ETJET_MINDR,NUMJETS,MUJET_PTREL
      REAL VMOD,VDOTN,ZVERT,OLDPT_JET,MUJET_ENRAT
C
      INTEGER IJETS,NUM_JETS,IER,IVERS,LVERT,LVERH,GZVERH
C
      LOGICAL FIRST,DOIT
C
      EXTERNAL VMOD,VDOTN
C
      DATA COS30/0.8660254/
      DATA FIRST/.TRUE./
      DATA DOIT/.TRUE./
C----------------------------------------------------------------------
C ****  SET PATH TO THE DEFAULT JET FINDING ALGORITHM
C ****       ( CONE WITH R=0.7, ET=8GEV )
C
      CALL VZERO(TEMPLATE,3)
      CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
      IF ( IER.LT.0 ) GOTO 998
C
      ET_SAME = 0.
      ET_OPP = 0.
      ET_BACK = 0.
      DPHI_BACK = 0.
      DR_MIN=9999.
      DPHI_MIN=9999.
      ETJET_MINDPHI = 0.
      ETJET_MINDR = 0.
      MUJET_ENRAT = 0.
      MUJET_PTREL = 0.
C
C get vertex to use in jet Energy correction
      ZVERT=0.
      LVERH=GZVERH()
      IF(LVERH.GT.0)THEN
        LVERT=LQ(LVERH-IZVERT)
        IF(LVERT.GT.0)THEN
          ZVERT=Q(LVERT+5)   ! z-position of vertex
        ENDIF
      ENDIF
C
      RT_MOD = VMOD(P_REF,2)
      P_REF_MOD = VMOD(P_REF,3)
C
      IF(RT_MOD.GT.0.) THEN
        PHI_REF = ATAN2(P_REF(2),P_REF(1))
        IF (P_REF_MOD.NE.0.) THEN
          COSTHE_REF = P_REF(3)/ P_REF_MOD
          THETA_REF = ACOS(COSTHE_REF)
          IF (TAN(THETA_REF/2.).GT.0) THEN
            ETA_REF = -LOG(TAN(THETA_REF/2.))
          ENDIF
        ENDIF
C
        CALL GTJETS_TOTAL(NUM_JETS,IER)
        IF(IER.EQ.0) THEN
          NUMJETS = NUM_JETS
C loop over jets
          DO IJETS=1,NUM_JETS
            CALL GTJETS (IJETS,IVERS,P,THETA,PHI,ETA,IER)
            OLD_PTJET = VMOD(P,2)
            IF(OLD_PTJET.GT.0.) THEN
C
C Correct the jet energy
C
              OLDE = P(4)
              OLDET = P(5)
              OLDETA = ETA
              OLDEMF = P(7)    ! not currently used
              OLDSIZ = 1.2     ! not currently used
              OLDCONE = .7
              CALL QCD_JET_CORRECTION_2( OLDE, OLDET, OLDETA,
     &                   OLDEMF, OLDSIZ,OLDCONE )
              CALL QCD_JET_CORRECTION(0,DOIT,DOIT,DOIT,ZVERT,
     &                    0,NEW_JET_E,NEW_JET_ET,NEW_JET_ETA,IER)
C
              PT_JET = NEW_JET_ET
C
C note that the direction of the jet is not changed. new_eta = old_eta
C
C look in 2-D cone around muon
              COSTH = VDOTN(P_REF,P,3)
              IF(COSTH.GT.COS30) THEN
                IF(PT_JET.GT.ET_SAME) ET_SAME = PT_JET
              ENDIF
C look in 1-D wedge opposite muon
              COSTH = VDOTN(P_REF,P,2)
              IF(-COSTH.GT.COS30) THEN
                IF(PT_JET.GT.ET_OPP) ET_OPP = PT_JET
              ENDIF
C look for highest ET jet in opposite semi-cicle
              IF(COSTH.LT.0.) THEN
                IF(PT_JET.GT.ET_BACK) THEN
                  ET_BACK = PT_JET
                  DPHI_BACK = PHI - PHI_REF
                  IF(DPHI_BACK.LT.0.) DPHI_BACK = DPHI_BACK + TWOPI
                ENDIF
              ENDIF
C look for closest jet to the input direction  in phi
              DPHI=ABS(PHI - PHI_REF)
              IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
              IF(DPHI.LT.DPHI_MIN) THEN
                DPHI_MIN=DPHI
                ETJET_MINDPHI = PT_JET
              ENDIF
C
C look for closest jet to the input direction in dr
C Calculated ptrel and enrat for the jet with the smallest dr(jet,ref).
C
              DETA=ABS(ETA-ETA_REF)
              DR=SQRT(DETA**2+DPHI**2)
              IF(DR.LT.DR_MIN) THEN
                DR_MIN=DR
                ETJET_MINDR = PT_JET
                MUJET_ENRAT = P_REF_MOD/(P_REF_MOD+NEW_JET_E)
                MUJET_PTREL = P_REF_MOD * SIN(ACOS(COSTH))
              ENDIF
C
            ENDIF
          ENDDO
        ENDIF
      ENDIF
  998 CALL RESET_CAPH
C
  999 RETURN
      END
