      SUBROUTINE CAL_MODULE_LIMITS(IMOD,ELIM,LLIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns ETA and LYR limts for a given module
C-
C-   Inputs  : IMOD  1=CCEM 2=ECEM 3=CCMG 4=ICD  5=ECMG 
C-                   6=CCFH 7=ECIH 8=ECMH 9=CCCH 10=ECOH
C-   Outputs : ELIM(2) [I] - limits on ABS(IETA) for IMOD (ELIM(1)<ELIM(2))
C-             LLIM(2) [I] - limits on ILYR for IMOD
C-   Controls: D0$CALOR_UTIL:CAL_MODULE.RCP
C-
C-   Created  16-MAR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER ELIM(2),LLIM(2),IETA,ILYR,ETAL(2,10),LYRL(2,10)
      INTEGER IMOD,JMOD,CAL_MODULE
      LOGICAL FIRST,CEXIST
      CHARACTER MODULE*4
      SAVE FIRST,ETAL,LYRL
      DATA FIRST/.TRUE./
      DATA ETAL/20*0/,LYRL/20*0/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        DO IETA = -NETAL, NETAL
          DO ILYR = 1, NLYRL
            IF( CEXIST(IETA,1,ILYR)) THEN
              JMOD = CAL_MODULE(IETA,ILYR,MODULE) 
              IF ( ETAL(1,JMOD).EQ.0) ETAL(1,JMOD) = 100
              IF ( ABS(IETA).LT. ETAL(1,JMOD)) ETAL(1,JMOD) = ABS(IETA)
              IF ( ABS(IETA).GT. ETAL(2,JMOD)) ETAL(2,JMOD) = ABS(IETA)
              IF ( LYRL(1,JMOD).EQ.0) LYRL(1,JMOD) = 100
              IF ( ILYR.LT. LYRL(1,JMOD)) LYRL(1,JMOD) = ILYR
              IF ( ILYR.GT. LYRL(2,JMOD)) LYRL(2,JMOD) = ILYR
            END IF
          END DO
        END DO
      END IF
      ELIM(1) = ETAL(1,IMOD)
      ELIM(2) = ETAL(2,IMOD)
      LLIM(1) = LYRL(1,IMOD)
      LLIM(2) = LYRL(2,IMOD)
C----------------------------------------------------------------------
  999 RETURN
      END
