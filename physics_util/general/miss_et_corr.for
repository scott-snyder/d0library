      SUBROUTINE MISS_ET_CORR(VERSION,ZVERT,CAL_ESCALE,MET_CORR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Returns the corrections to be SUBTRACTED
C-                          from the x,y and z components of the missing
C-                          Et in the PNUT bank.  Works for V10 or
C-                          higher version of RECO for data.  For MC,
C-                          VERSION should be 0 (see below) whence no
C-                          distinction is made between different RECO
C-                          versions.  For any VERSION <10 but not 0,
C-                          the routine returns 0 correction.
C-
C-   Inputs  :
C-     VERSION = 10,11 (use 0 for Montecarlo)
C-     ZVERT = z of vertex
C-     CAL_ESCALE(3) = 1/2/3 for em-scale corrections (ECN/CC/ECS)
C-   Outputs :
C-     MET_CORR(3)= -px, -py, -pz of missing ET correction
C-   Controls:
C-
C-   Created   9-AUG-1993   Serban D. Protopopescu
C-   Updated  12-OCT-1993   Dhiman Chakraborty
C-                          To get new PX, PY, scale the old ones by
C-                          NEW_ET/OLD_ET instead of taking NEW_ET*COS(PHI)
C-                          and NEW_ET*SIN(PHI)
C-   Updated  29-OCT-1993   Dhiman Chakraborty
C-                          Correct for jets with EMF>0.9 as if they were
C-                          electrons.
C-   Updated  20-NOV-1993   S.J.Wimpenny
C-                          3rd componemt of missing Et (Ezmiss) added to
C-                          return argument list. + code cleaned up.
C-                          Calorimeter Croystat-dependent corrections added
C-                          to calling argument list.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FOUND_MATCH,DO_ZSP_CORR,DO_UNDEVT_CORR,DO_OUTOFCONE_CORR
C
      INTEGER VERSION
      INTEGER GZJETS,LJETS,IER,ISYS
C
      REAL ZVERT,MET_CORR(3),CAL_ESCALE(3),HV_CORR
      REAL TEMPLATE(3)
      REAL NEWETA,NEW_E,NEW_ET,THETA,ETA,SCALE,EMF
      REAL NEWE(5),OLDE(5),OLDETA,OLDPHI,NEWPHI,DE(5)
C
      DATA DO_ZSP_CORR,DO_UNDEVT_CORR,DO_OUTOFCONE_CORR/.FALSE.,
     1  .FALSE.,.FALSE./
      DATA ISYS/ 0/
C
C *** Hardwire jet template to use on dR=0.7 cone jets
C
      DATA TEMPLATE/ 1., 6., 0.7/
      DATA HV_CORR/ 1.015/
C----------------------------------------------------------------------
C
C *** Initialize Missing Et Correction Vector
C
      CALL VZERO(MET_CORR,3)
C
C *** Check for viable RECO Versions
C
      IF((VERSION.LT.10).AND.(VERSION.NE.0))GOTO 999
C
      CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
      LJETS=GZJETS()
      DO WHILE (LJETS.NE.0)
        EMF=Q(LJETS+14)
C
C *** Check to see if Jet is highly EM - if not, then apply QCD jet corr
C
        IF(EMF.LT.0.9) THEN
C
C *** standard hadronic jets
C
          IF(VERSION.GE.10)
     1      CALL QCD_JET_CORRECTION(LJETS,DO_ZSP_CORR,DO_UNDEVT_CORR,
     2      DO_OUTOFCONE_CORR,ZVERT,ISYS,NEW_E,NEW_ET,NEWETA,IER)
          IF(VERSION.EQ.0)THEN
            CALL UCOPY(Q(LJETS+2),OLDE,5)
            OLDPHI = Q(LJETS+8)
            OLDETA = Q(LJETS+9)
            CALL MC_ET_CORR(OLDE,OLDETA,OLDPHI,EMF,1,
     &          NEWE,NEWETA,NEWPHI,DE,FOUND_MATCH)
            NEW_ET = NEWE(5)
          ENDIF
          SCALE = NEW_ET/Q(LJETS+6)
C
C *** highly em jets
C
        ELSE  
          THETA=Q(LJETS+7)
          CALL DET_ETA(ZVERT,THETA,ETA)
          SCALE=CAL_ESCALE(2)
          IF(ETA.LT.-1.2) SCALE=CAL_ESCALE(1)
          IF(ETA.GT.1.2) SCALE=CAL_ESCALE(3)
          IF(VERSION.EQ.10) SCALE=SCALE*HV_CORR
          IF(VERSION.EQ.0) SCALE=1.0
        ENDIF
        MET_CORR(1)=MET_CORR(1) + Q(LJETS+2)*(SCALE - 1.0)
        MET_CORR(2)=MET_CORR(2) + Q(LJETS+3)*(SCALE - 1.0)
        MET_CORR(3)=MET_CORR(3) + Q(LJETS+4)*(SCALE - 1.0)
        LJETS=LQ(LJETS)
      ENDDO
      CALL RESET_CAPH
  999 RETURN
      END
