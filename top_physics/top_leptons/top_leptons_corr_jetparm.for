      SUBROUTINE TOP_LEPTONS_CORR_JETPARM(LJETS,E,ET,PX,PY,PZ,PHI,
     &  ETA,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return corrected jet energies given LJETS
C-                         pointer (for either MC or Real data)
C-
C-   Inputs  : LJETS pointer for jet
C-   Outputs : E     Corrected energy
C-             ET    Corrected Et
C-             PX    Corrected Px
C-             PY    Corrected Py
C-             PZ    Corrected Pz
C-             PHI   Corrected Phi
C-             ETA   Corrected Eta
C-
C-             IER   (1=OKAY,-1=Not corrected)
C-   Controls:
C-
C-   Created  20-APR-1993   joey thompson
C-   Modified  9-Jul-1993   re-written to use new data corrections
C-   Modified 27-Jul-1993   uses updated MC jet corrections from MN.
C-                          CAPH logic removed
C-   Modified  1-Aug-1993   Change to call in TOP_LEPTONS_JET_CORRECTION
C-                          to allow for different QCD_JET_CORRECTION
C-                          options for missng Et and Jet correction
C-                          calculations.
C-   Modified 14-Oct-1993   Uses modified arguments for MC_ET_CORR - changed
C-                          to pick up standard PYHSISC$UTIL version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_UTIL_MONTECARLO,TOP_LEPTONS_JET_CORRECTION
C
      LOGICAL TOP_LEPTONS_UTIL_MONTECARLO,FIRST
      LOGICAL FOUND_MATCH
C
      REAL TOP_LEPTONS_JET_CORRECTION
      REAL E,ET,PX,PY,PZ,PHI,ETA,EMF
      REAL CORRFAC,ECORR,DEOUT(5),JET_CONE,CONE(3)
      REAL OLDE(5),OLDETA,OLDPHI,NEWE(5),NEWETA,NEWPHI
C
      INTEGER LJETS,JET_ALG
      INTEGER IER,ICOR_MC,ICOR
C
      DATA CONE/ 0.7, 0.5, 0.3/
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Jet Algorithm
C
        CALL EZGET('JETS_ALGORITHM',JET_ALG,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_CORR_JETPARM',' ','F')
        FIRST=.FALSE.
      ENDIF
C
C *** Get Jet params from bank
C
      PX  = Q(LJETS+2)
      PY  = Q(LJETS+3)
      PZ  = Q(LJETS+4)
      E   = Q(LJETS+5)
      ET  = Q(LJETS+6)
      PHI = Q(LJETS+8)
      ETA = Q(LJETS+9)
      EMF = Q(LJETS+14)
      IER = -1
      CORRFAC=1.
C
C *** Test for supported jet algorithm
C
      IF(JET_ALG.GT.3.OR.JET_ALG.LT.1) THEN
        WRITE(12,1100) JET_ALG
        CALL ERRMSG(
     1    'Unsupported algorithm: Jet energy Corrections NOT applied',
     2    'TOP_LEPTONS_CORR_JETPARAM',' ','W')
          GO TO 999
      ENDIF
C
C *** Test for known cone radii
C
      JET_CONE = CONE(JET_ALG)
      IF ((JET_CONE.LT.0.29).OR.(JET_CONE.GT.0.71)) THEN
        WRITE(12,1000) JET_CONE,JET_ALG
        CALL ERRMSG(
     1    ' Invalid cone size => jets not corrected',
     2    'TOP_LEPTONS_CORR_JETPARAM',' ','F')
        GO TO 999
      ENDIF
C
C *** Test for Montecarlo to decide which set of corrections to use
C
      IF(.NOT.TOP_LEPTONS_UTIL_MONTECARLO()) THEN
C
C *** Real Data
C *** set correction flag to 1 to indicate we are doing jet
C *** corrections only
C
        ICOR=1
        ECORR=TOP_LEPTONS_JET_CORRECTION(LJETS,JET_CONE,ICOR)
C
        CORRFAC=ECORR/E
C
C *** Correct Data
C
        PX  = PX*CORRFAC
        PY  = PY*CORRFAC
        PZ  = PZ*CORRFAC
        E   = E*CORRFAC
        ET  = ET*CORRFAC
        ETA = ETA
        PHI = PHI
        IER = 1
      ELSE
C
C *** Get variables ready for injection to MC_ET_CORR
C
        OLDE(1) = PX
        OLDE(2) = PY
        OLDE(3) = PZ
        OLDE(4) = E
        OLDE(5) = ET
        OLDETA  = ETA
        OLDPHI  = PHI
C
C *** Set ICOR_MC according to which jet cone is being used
C
        ICOR_MC=JET_ALG
        CALL MC_ET_CORR(OLDE,OLDETA,OLDPHI,EMF,ICOR_MC,NEWE,
     1    NEWETA,NEWPHI,DEOUT,FOUND_MATCH)
        CORRFAC = NEWE(5)/OLDE(5)
C
C *** Correct MC
C
        PX  = NEWE(1)
        PY  = NEWE(2)
        PZ  = NEWE(3)
        E   = NEWE(4)
        ET  = SQRT( PX**2 + PY**2)
        ETA = NEWETA
        PHI = NEWPHI
        IER = 1
      ENDIF
C
  999 RETURN
 1000 FORMAT(//,' ==> Illegal Cone Algorith Request : CONE_SIZE =',
     1 F6.3,' Anal Alg = ',I3,' TOP_LEPTONS_CORR_JETPARM <== ',//)
 1100 FORMAT(//,' ==> Illegal Cone Algorith Request : ANAL_IALG =',
     1 I3,' TOP_LEPTONS_CORR_JETPARM <== ',//)
      END
