      SUBROUTINE L1DMP_LARGE_TILE_GEV(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the Large Tile Energies in GeV.
C-
C-   Inputs  : LUN      The unit number to write to.
C-   Outputs : none
C-   Controls: none
C-
C-   Created   9-JUL-1993   Philippe Laurens - MSU L1 Trigger   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_ENERGY.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
C
      INTEGER LUN
C
      INTEGER LT_ETA, LT_PHI, INGROUP
      INTEGER ISTAT
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'Large Tile Energies in GeV'
      WRITE (LUN,*) '=========================='
      WRITE (LUN,*)
      WRITE (LUN,*) 'Note: The following values have been ' 
     &           // 'corrected for zero input energy offset.'
      WRITE (LUN,*)
C
  410 FORMAT ( SP, ' ETA = [', I3, '..', I3, ']')
  420 FORMAT ( ' PHI = [1..8],[9..16],[17..24],[25..32] ', 4(' ',F8.2))
      DO LT_ETA = LT_ETA_MAX, LT_ETA_MIN, -1
        WRITE (LUN,*)
        WRITE (LUN,410) - ( (LT_ETA-1) * TT_ETA_PER_LT + 1 ),
     &                  - (  LT_ETA    * TT_ETA_PER_LT )
        WRITE (LUN,420,IOSTAT=ISTAT) 
     &        ( ( ( LT_ENERGY(NEG_ETA,LT_ETA,LT_PHI) 
     &            - LT_ZERESP(NEG_ETA,LT_ETA,LT_PHI) )
     &          * GLOBAL_ENERGY_SCALE ( PX_QUANT ) ), 
     &        LT_PHI = LT_PHI_MIN, LT_PHI_MAX )
      END DO
      DO LT_ETA = LT_ETA_MIN, LT_ETA_MAX
        WRITE (LUN,*)
        WRITE (LUN,410) ( (LT_ETA-1) * TT_ETA_PER_LT + 1), 
     &                  (  LT_ETA    * TT_ETA_PER_LT )
        WRITE (LUN,420,IOSTAT=ISTAT) 
     &        ( ( ( LT_ENERGY(POS_ETA,LT_ETA,LT_PHI) 
     &            - LT_ZERESP(POS_ETA,LT_ETA,LT_PHI) )
     &          * GLOBAL_ENERGY_SCALE ( PX_QUANT ) ), 
     &        LT_PHI = LT_PHI_MIN, LT_PHI_MAX )
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
