      FUNCTION STRIP_SUMMARY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out statistics from stripping package
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-MAY-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL STRIP_SUMMARY
      LOGICAL L,GET_STRIP_SUM
C
      INTEGER NPASS,NREJECTED,NREJ_USER,NREJ_L1,NREJ_L2,NREJ_MICROBLANK
      INTEGER LUN,SSUNIT
C
C----------------------------------------------------------------------
      STRIP_SUMMARY = .TRUE.
C
      L = GET_STRIP_SUM(NPASS,NREJECTED,NREJ_L1,NREJ_L2,NREJ_MICROBLANK,
     &  NREJ_USER)
C
      LUN = SSUNIT()
      WRITE (LUN,1000) NPASS,NREJECTED,NREJ_L1,NREJ_L2,NREJ_MICROBLANK,
     &  NREJ_USER
 1000 FORMAT(//,'  Summary of events rejected by STRIP_TRIG package',
     &  /,5X,'Events passed            ',I15,
     &  /,5X,'Events rejected          ',I15,
     &  /,5X,'Events rej w/ lev1 bits  ',I15,
     &  /,5X,'Events rej w/ lev2 bits  ',I15,
     &  /,5X,'Events rej by microblank ',I15,
     &  /,5X,'Events rej by user code  ',I15)
  999 RETURN
      END
