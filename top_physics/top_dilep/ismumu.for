      SUBROUTINE ISMUMU(NANAL,NMU,PT,ETA,PHI,PTPAR,NBMU,PTB,ETAB,PHIB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MC_TREE.INC'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
C
      INTEGER NMU,NBMU,J,IDABS,NANAL,NTYPE
      REAL    PT(NWANT_MUISA),ETA(NWANT_MUISA),PHI(NWANT_MUISA)
      REAL    PTPAR(NWANT_MUISA)
      REAL    PTB(NWANT_MUISA),ETAB(NWANT_MUISA),PHIB(NWANT_MUISA)
      REAL    PTSQRT
C----------------------------------------------------------------------
C
      CALL ISFILL()
      NMU=0
      NBMU=0
      NTYPE=0
      CALL VZERO(PT,NWANT_MUISA)
      CALL VZERO(ETA,NWANT_MUISA)
      CALL VZERO(PHI,NWANT_MUISA)
      CALL VZERO(PTPAR,NWANT_MUISA)
      CALL VZERO(PTB,NWANT_MUISA)
      CALL VZERO(ETAB,NWANT_MUISA)
      CALL VZERO(PHIB,NWANT_MUISA)
      DO J=1,NMC
        IDABS=ABS(IMCDAT(JMC_ID,J))
        IF(IDABS.EQ.14) THEN
          IF(ABS(IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J))).EQ.80) THEN
            NMU=NMU+1
            IF(NMU.LE.NWANT_MUISA) THEN
              PTSQRT=SQRT(RMCDAT(JMC_PX,J)**2.+RMCDAT(JMC_PY,J)**2.)
              PT(NMU) =PTSQRT
              ETA(NMU)=RMCDAT(JMC_ETA,J)
              PHI(NMU)=RMCDAT(JMC_PHI,J)
              PTSQRT=SQRT(RMCDAT(JMC_PX,IMCDAT(JMC_PAR,J))**2.+
     &          RMCDAT(JMC_PY,IMCDAT(JMC_PAR,J))**2.)
              PTPAR(NMU) =PTSQRT
              NTYPE=80
            ENDIF
          ELSEIF(ABS(IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J))).EQ.90) THEN
            NMU=NMU+1
            IF(NMU.LE.NWANT_MUISA) THEN
              PTSQRT=SQRT(RMCDAT(JMC_PX,J)**2.+RMCDAT(JMC_PY,J)**2.)
              PT(NMU) =PTSQRT
              ETA(NMU)=RMCDAT(JMC_ETA,J)
              PHI(NMU)=RMCDAT(JMC_PHI,J)
              PTSQRT=SQRT(RMCDAT(JMC_PX,IMCDAT(JMC_PAR,J))**2.+
     &          RMCDAT(JMC_PY,IMCDAT(JMC_PAR,J))**2.)
              PTPAR(NMU) =PTSQRT
              NTYPE=90
            ENDIF
          ENDIF
          IF(ABS(IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J))).EQ.5) THEN
            NBMU=NBMU+1
            IF(NBMU.LE.NWANT_MUISA) THEN
              PTSQRT=SQRT(RMCDAT(JMC_PX,J)**2.+RMCDAT(JMC_PY,J)**2.)
              PTB(NBMU) =PTSQRT
              ETAB(NBMU)=RMCDAT(JMC_ETA,J)
              PHIB(NBMU)=RMCDAT(JMC_PHI,J)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      NANAL=NTYPE
C
  999 RETURN
      END
