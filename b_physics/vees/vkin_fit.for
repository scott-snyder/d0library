      SUBROUTINE VKIN_FIT(PARTICLE,IEND,PRB,CHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : General fit routine
C-
C-   Inputs  :            PARTICLE (1 Vee; 2 dilepton)
C-   Outputs :            IEND   > 0 - fit is successful
C                         IEND   < 0 - fit is failed
C                         NSTEP  - number of steps iteration    
C                         PRB    - fit probability
C                         CHI    - fit chi-squared
C-   Controls: 
C-
C-   Created  18-apr-1991  V. Burtovoy
C-   Updated  21-OCT-1991   Daria Zieminska  D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*8 ART(3),XEND(12)
      INTEGER PARTICLE,IEND,NSTEP,PRUNIT,USUNIT,IPRINT,IER
      REAL PRB,CHI
      LOGICAL FIRST 
      SAVE FIRST,PRUNIT
      DATA FIRST/.TRUE./
      DATA ART / 'DECAY','INTER1','INTER2'/
      DATA XEND / 'NTR?','NX>NF','CODE?','OVERFL?','F>MAX',
     +            'NST>MAX','X2/F-BAD','X2-INCR','F-INCR',
     +            '??????','F<MIN','X2/F-OK' /
c
      IF(FIRST) THEN
         FIRST = .FALSE.
         PRUNIT=USUNIT()
         CALL EZPICK('VEES_RCP')
         CALL EZGET('IPRINT',IPRINT,IER)
         CALL EZRSET
      ENDIF
      CALL VKIN_TYPE(1)
      CALL VKIN_NCFIT( PARTICLE,IEND,NSTEP,PRB,CHI )
      IF (IEND.EQ.0) GO TO 999
      IF(IEND.LT.0)  GO TO 2
      IF (IPRINT.GT.0) 
     + WRITE(PRUNIT,2004) NSTEP,XEND(IEND+10),CHI,PRB    
 2004 FORMAT(10X,'**END** NSTEP=',I2,'  (',A8,')   CHI2=',E10.4/
     +       10X,'PROBABILITY=',E10.4/)
      CALL VKIN_TYPE(3)
      GO TO 999
    2 CONTINUE
      IF (IPRINT.GT.0) WRITE(PRUNIT,2005) NSTEP,XEND(IEND+10) 
 2005 FORMAT(10X,'==== FIT FAILED AFTER',I3,'-STEPS.   (',A8,')'/)
      CALL VKIN_TYPE(4)
  999 RETURN 
      END
