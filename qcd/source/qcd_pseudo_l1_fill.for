      SUBROUTINE QCD_PSEUDO_L1_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill TOT ET arrays for each trigger tower
C-                         using TRGR block is possible, else using
C-                         MDST
C-                         Do this only once/event
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-JUN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PSL1.INC'
      INTEGER LTRGR, GZTRGR, EVTSAVE, RUNSAVE, GZFIND_CRATE
      INTEGER L1PHIC, L1ETAC, LTPHI, LTETA
      REAL ET
      SAVE EVTSAVE, RUNSAVE
C----------------------------------------------------------------------
C
C: Have we done this already?
C
      IF ( EVTSAVE .EQ. IQ(LHEAD+9) .AND. RUNSAVE .EQ. IQ(LHEAD+6 ) )
     &  RETURN

      EVTSAVE = IQ( LHEAD + 9 )
      RUNSAVE = IQ( LHEAD + 6 )
C
      LTRGR = GZFIND_CRATE('TRGR', GZTRGR(), 11 )
C
C: Use TRGR block if possible
C
      IF ( LTRGR .GT. 0 ) THEN
        CALL QCD_L1_FILL
      ELSE
        CALL QCD_MDST_L1_FILL
      ENDIF
C
C: Make Large Tiles from the results
C
      DO L1PHIC = 1, 32
        DO L1ETAC = 1, 40
          ET = PSL1TOW( L1ETAC, L1PHIC )
          LTPHI = (L1PHIC-1)/8 + 1
          LTETA = (L1ETAC-1)/4 + 1
          PSL1LT( LTETA, LTPHI ) = PSL1LT( LTETA, LTPHI ) + ET
        ENDDO
      ENDDO


  999 RETURN
      END
