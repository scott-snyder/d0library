      SUBROUTINE FIX_EM_SF(LCLUS,CORRFACT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ROUTINE TO CORRECT EM CLUSTERS FOR
C-                         SAMPLING FRACTIONS
C-
C-   Inputs  : LCLUS      Link to PELC or PPHO bank
C-   Outputs : CORRFACT   Multiplicative correction factor
C-   Controls: CORRECTEM.RCP
C-
C-   Created  21-JUL-1993   Norman A. Graf
C-   Updated   3-AUG-1993   Marcel Demarteau  fixed couple of bugs 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL ECLUS,ECORR,ENERGY, CORR, CORRFACT
      REAL PULSCORR(0:11)
      INTEGER I,NCH,POINTER,IOK,IER
      INTEGER  ETAI,PHII,ILYR
      INTEGER PACKED_WORD,LCASH,LCACL,LCLUS
      INTEGER VERSION,PASS
C
      INTEGER IETA_HOT(5),IPHI_HOT(5),LAYER_HOT(5)
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5)
C
      REAL OLDWEIGHTS(5),NEWWEIGHTS(5),NEWDELTA,OLDALPHA
      REAL OLDDELTA,NEWALPHA
      DATA OLDWEIGHTS /0.996, 0.746, 0.750, 0.723, 1.354/
      DATA NEWWEIGHTS /1.308, 0.852, 1.0, 0.978, 1.840/
      DATA OLDALPHA /3.823/
      DATA NEWALPHA /2.956/
      DATA OLDDELTA /0.0/
      DATA NEWDELTA /0.347/
C
      LOGICAL FIRST
      LOGICAL WANT_FIX_CSF,DO_FIX_CSF,DO_ADD_DE
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST= .FALSE.
        CALL EZPICK('CORRECTEM_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('CORRECTEM','FIX_EM_SF',
     &    'NO RCP FILE, USING DEFAULTS','W')
        CALL EZGET('OLD_CSF_WEIGHTS',OLDWEIGHTS,IER)
        CALL EZGET('NEW_CSF_WEIGHTS',NEWWEIGHTS,IER)
        CALL EZGET('OLDALPHA',OLDALPHA,IER)
        CALL EZGET('NEWALPHA',NEWALPHA,IER)
        CALL EZGET('OLDDELTA',OLDDELTA,IER)
        CALL EZGET('NEWDELTA',NEWDELTA,IER)
        CALL EZGET('DO_FIX_CSF',WANT_FIX_CSF,IER)
        CALL EZRSET
      ENDIF
C
      DO_ADD_DE  = .FALSE. 
      DO_FIX_CSF = WANT_FIX_CSF
      CALL RECO_VERSION(VERSION,PASS)
      IF(VERSION.GE.12.AND.PASS.GE.10.AND.WANT_FIX_CSF) THEN
        CALL ERRMSG('RECO 12.10 and higher has new sampling fractions',
     &    'correctem','Turn off sampl frac correction, add delta','w')
        DO_FIX_CSF = .FALSE.
        DO_ADD_DE  = .TRUE. 
      ENDIF
C
      LCACL = LQ(LCLUS-2)
      IF(LCACL.LE.0)  THEN
        CALL ERRMSG('CORRECTEM','FIX_EM_SF','NO CACL BANK','W')
        GOTO 999
      ENDIF
      LCASH = LQ(LCACL-2)
      IF(LCASH.LE.0)  THEN
        CALL ERRMSG('CORRECTEM','FIX_EM_SF','NO CASH BANK','W')
        GOTO 999
      ENDIF
C
      CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
      ECLUS = 0.
      ECORR = 0.
      CORRFACT = 1.0
      IF(DO_FIX_CSF) THEN 
        IF(ABS(IETA_HOT(3)).LT.13) THEN
          ECORR = ECORR - OLDDELTA
          DO I = 1,5
            ECORR = ECORR + (EDPTH(I)/OLDWEIGHTS(I))*NEWWEIGHTS(I)
            ECLUS = ECLUS + EDPTH(I)
          ENDDO
          ECORR = (ECORR*NEWALPHA)/OLDALPHA + NEWDELTA
          CORRFACT = ECORR/ECLUS
        ENDIF
      ELSEIF(DO_ADD_DE) THEN 
        IF(ABS(IETA_HOT(3)).LT.13) THEN
          DO I = 1,5
            ECLUS = ECLUS + EDPTH(I)
          ENDDO
          ECORR = ECLUS + NEWDELTA
          CORRFACT = ECORR/ECLUS
        ENDIF
      ENDIF
C
      RETURN
  999 CONTINUE 
      CORRFACT = 1.0
      RETURN
      END
