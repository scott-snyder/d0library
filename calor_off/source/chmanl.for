      SUBROUTINE CHMANL(ECLUS,NEWECLUS,SHOWER_CENTER,LEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes EM calorimeter clusters (CACL)
C-                         using the H matrix method.
C-
C-   Inputs  : CACL and CACH banks
C-   Outputs : LEVEL         0 IF HMATRIX ANALYSIS NOT USED
C-   Controls: CAPHEL_RCP
C-
C-   Called by: CAPHEL
C-
C-   Created   4-APR-1990   Norman Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CHMATR.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER
      INTEGER II,I
      REAL    PREDI
C
      LOGICAL HMREAD,HMPRED,HMITER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      REAL    PROB_LONGITUDINAL_CUT,PROB_FULL_CUT
      LOGICAL PREDICT_LONGITUDINAL,PREDICT_FULL,LEVEL_TWO
C
      REAL    SHOWER_CENTER(3)
C
      REAL ECLUS,NEWECLUS,XCLUS,YCLUS,ZCLUS,OLDCHSQL,WCHSQL,PREDICTED
      LOGICAL ADDEM
C
      REAL ENERGY_MAX
      INTEGER NCELL, CENTRAL_TOWER,LEVEL
      INTEGER IETA,IPHI,LAYER,ETAMAX
      LOGICAL READIN,ARGSOK
C
C----------------------------------------------------------------------
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('HMATRIX_READ',HMREAD,IER)
        CALL EZGET('HMATRIX_PRED',HMPRED,IER)
        CALL EZGET('HMATRIX_ITER',HMITER,IER)
        CALL EZGET('PROB_LONGITUDINAL_CUT',PROB_LONGITUDINAL_CUT,IER)
        CALL EZGET('PROB_FULL_CUT',PROB_FULL_CUT,IER)
        CALL EZGET('PREDICT_LONGITUDINAL',PREDICT_LONGITUDINAL,IER)
        CALL EZGET('PREDICT_FULL',PREDICT_FULL,IER)
        CALL EZGET('LEVEL_TWO',LEVEL_TWO,IER)
        CALL EZGET('MAXIMUM_ETA',ETAMAX,IER)
        CALL EZGET('HMATRIX_MAXIMUM_ENERGY',ENERGY_MAX,IER)
        CALL EZRSET
        CALL EZPICK('CAHITS_RCP')
        CALL EZRSET
      ENDIF
C
C ****  Set defaults to avoid problems if routines not called
C
      PROBL = 1.
      PROBF = 1.
      LEVEL = 1
C
      IF(HMREAD)THEN
        IF(HMPRED) THEN
          IF(ECLUS .GT. ENERGY_MAX) THEN
            LEVEL = 0
            GOTO 999   ! ***Temporary***
          ENDIF
C
C ****  Find tower center (used in interpolation of matrix elements)
C
          LCACH = LQ(LCACL-1)
          NCELL = IQ(LCACH+2)
          CENTRAL_TOWER = IQ(LCACH+NCELL+3)
          IETA = IQ(LCATE + (CENTRAL_TOWER-1)*14 + 12)
C
C:::   Predict longitudinal matrix elements for energy
C:::   ECLUS and eta bin IETA. 
C:::   Note that this overwrites elements in CHMATR.INC
C
          READIN = .TRUE.
          IETA = ABS(IETA)
          IF (IETA.GT.etamax) THEN
            LEVEL = 0
            GO TO 999  ! ***Temporary***
          ENDIF
          if(ieta.eq.13 .or. ieta.eq.14) THEN        ! EC/CC transition
            level = 0
            go to 999      
          endif
          CALL CEMATPRDLONG(READIN,IETA,ECLUS)
          READIN = .FALSE.
        ENDIF
C
        CALL CHQUAN(1)               ! Set up longitudinal matrix quantities
        CALL CHISQ_LONG
C        IF(CHSQL.LT.0) WRITE(37,*) 'IETA ',IETA,' ECLUS ',ECLUS
        IF(PROBL.LT.PROB_LONGITUDINAL_CUT)GO TO 999
C
C    Predict Crack and Cryostat energies using Longitudinal matrix...
C
        IF(PREDICT_LONGITUDINAL) THEN
          IF(HMITER) THEN
C
C::: Now to predict dead energy and add this to ECLUS. Use new ECLUS to get
C::: better approximation to correct (longitudinal) H MATRIX elements.
C
            ADDEM = .TRUE.
C
C::: Use a chi squared weighted by the energy squared (CHISQ_WTL)
C
            CALL CHISQ_WTL(ECLUS,WCHSQL)
            DO WHILE (ADDEM)
              CALL CPREDICT_LONG
              PREDICTED = 0.
              DO I = 1,NDIMP
                PREDICTED = PREDICTED + PREDL(I)
              ENDDO
              NEWECLUS = ECLUS + PREDICTED
C
C::: Predict long. H Matrix elements for the new cluster energy...
C
              CALL CEMATPRDLONG(READIN,IETA,NEWECLUS)
              OLDCHSQL = WCHSQL
              CALL CHISQ_WTL(NEWECLUS,WCHSQL)
              IF(WCHSQL.LT.OLDCHSQL) THEN
                ECLUS = NEWECLUS
              ELSE
C
C::: If chisquared is greater, reset matrix elements to that of old eclus
C
                ADDEM = .FALSE.
                CALL CEMATPRDLONG(READIN,IETA,ECLUS)
              ENDIF
            ENDDO
          ELSE
            CALL CPREDICT_LONG
          ENDIF
        ENDIF
C
c        IF(.NOT.LEVEL_TWO)THEN
c          CALL CHQUAN(2)                  ! Set up Full matrix quantities.
c          CALL CHISQ_FULL
c          IF(PROBF.LT.PROB_FULL_CUT)GO TO 999
c          IF(PREDICT_FULL)THEN
c            IF(HMITER) THEN
C
C::: Now to predict dead energy and add this to ECLUS. Use new ECLUS to get
C::: better approximation to correct (transverse) H MATRIX elements.
C
c              ADDEM = .TRUE.
C
C::: Use a chi squared weighted by the energy squared (CHISQ_WTF)
C
c              ECLUS = Q(LCACL+7)
c              CALL CHISQ_WTF(ECLUS,WCHSQL)
c              DO WHILE (ADDEM)
c                CALL CPREDICT_FULL
c                DO I = 1,NDIMP
c                  NEWECLUS = ECLUS + PRED(I)
c                ENDDO
C
C::: Predict trans. H Matrix elements for the new cluster energy...
C
c                CALL CEMATPRDCT(NEWECLUS)
c                OLDCHSQL = WCHSQL
c                CALL CHISQ_WTF(NEWECLUS,WCHSQL)
c                IF(WCHSQL.LT.OLDCHSQL) THEN
c                  ECLUS = NEWECLUS
c                ELSE
c                  ADDEM = .FALSE.
c                  CALL CEMATPRDCT(ECLUS)
c                ENDIF
c              ENDDO
c            ELSE
c              CALL CPREDICT_FULL
c            ENDIF
C
C::: Predict Center of shower + Cryostat + Crack energies...
C
c            II = 0.
c            DO I = IPOSN , NDIMH
c              II = II + 1
c              PREDI = PRED(I-IPREDF+1)
c              SHOWER_CENTER(II) = SHOWER_CENTER(II) + PREDI
c            ENDDO
c          ENDIF
c        ENDIF
C
      ENDIF
      CALL CCLANL
C
  999 RETURN
      END
