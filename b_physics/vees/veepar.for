      LOGICAL FUNCTION VEEPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : run initialization for VEES package
C-
C-   Inputs  : 
C-   Outputs : .TRUE. if run is to be processed (.FALSE. if skipped) 
C-   Controls: 
C-
C-   Created  22-JUL-1991   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FTRPAR, VTRPAR,READSTP
      INTEGER IER
C----------------------------------------------------------------------
C
      VEEPAR = .TRUE.
      CALL EZPICK('VEES_RCP')
      CALL EZGET('READSTP',READSTP,IER)
      IF (IER.NE.0.OR.READSTP.EQ..FALSE.) GO TO 999
      CALL CDINIT
      VEEPAR = FTRPAR()
      VEEPAR = VTRPAR()
      VEEPAR = .TRUE.
C
  999 RETURN
      END
