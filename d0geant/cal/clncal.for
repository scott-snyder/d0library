      LOGICAL FUNCTION CLNCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set flags/values that depend on FFREAD cards
C-                         read in parameter input
C-
C-   Inputs  : DCAL, SHWG
C-   Outputs : DECA,DUCA,DCRY,DEAD, LOECUT,HIECUT,X0STEP
C-
C-   Created  10-MAR-1988   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated   1-JUL-1992   K. Wyatt Merritt  Print out flag HONLY_PARAM
C-                          to signal use of SHWG=5; not added to DCALOG yet! 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC/LIST'
      INCLUDE 'D0$INC:DROP.INC'
C
      INTEGER NL
      PARAMETER (NL=11)
      LOGICAL CLOG(NL)
      EQUIVALENCE (CLOG(1),NO_SHLB)
      LOGICAL HONLY_PARAM
      INTEGER I
      REAL AMOD
C----------------------------------------------------------------------
      CLNCAL = .TRUE.
C
      DECA = MIN(DECA,DCAL)
      DUCA = MIN(DUCA,DCAL)
      DCRY = MIN(DCRY,DCAL)
      DEAD = MIN(DEAD,DCAL)
C
      IF ( DCAL .LE. 0 ) GOTO 999
C
C       Set up logicals using the D0LOG variables to make program flow
C       easier to follow
C
      DO I = 1,NL
        CLOG(I) = .FALSE.
      ENDDO
C
      IF (SHWG .EQ. 6)THEN
        SHWG = 3
        FORCE_SHLB = .TRUE.
      ELSE
        FORCE_SHLB = .FALSE.
      ENDIF
      IF (SHWG .EQ. 3) RAJA_SHLB = .TRUE.
      IF (SHWG .EQ. 2) JW_SHLB = .TRUE.
      IF (SHWG.LT.2 .OR. SHWG.GT.3) NO_SHLB = .TRUE.
      IF (SHWG .EQ. 1 .OR. SHWG.EQ.7) LINN_PARAM = .TRUE.
      IF (SHWG .EQ. 4) TREE_PRUNE = .TRUE.
      IF (SHWG .EQ. 5) THEN
        HONLY_PARAM = .TRUE.
        LINN_PARAM = .TRUE.
      ENDIF
      IF (SCAL(1) .EQ. 1) CALTOW = .TRUE.
      IF (AMOD(SCAL(10),2.).EQ.2.)  PLATE_GEOM = .TRUE.
C
      IF ( TREE_PRUNE ) THEN
        CALL VZERO(EDROP,988)
        LEFTOVER = 0.
        PRUNED = 0.
      ENDIF
C
      WRITE (LOUT,1000)NO_SHLB,RAJA_SHLB,JW_SHLB,CALTOW,LINN_PARAM,
     &        TREE_PRUNE,HONLY_PARAM,FORCE_SHLB,PLATE_GEOM
 1000 FORMAT(/,' NO_SHLB     ',L1,/,
     &         ' RAJA_SHLB   ',L1,/,
     &         ' JW_SHLB     ',L1,/,
     &         ' CALTOW      ',L1,/,
     &         ' LINN_PARAM  ',L1,/,
     &         ' TREE_PRUNE  ',L1,/,
     &         ' HONLY_PARAM ',L1,/,
     &         ' FORCE_SHLB  ',L1,/,
     &         ' PLATE_GEOM  ',L1,/)
C
      IF ( .NOT. LINN_PARAM ) THEN             ! Not parameterizing
        LOECUT = 0.
        HIECUT = 0.
        X0STEP = 0.
      ENDIF
      LOECUT_DEF = LOECUT
      HIECUT_DEF = HIECUT
      X0STEP_DEF = X0STEP
C
  999 RETURN
      END
