      SUBROUTINE L1DMP_FASTSIMUL(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump results of the Calorimeter Trigger simulation. 
C-     Used in debugging the fast Calorimeter Trigger algorithm.
C-
C-   Inputs  : LUN      The unit number to write to.
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-FEB-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$INC:L1C_ENERGY_FUTURE_USE.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_ENERGY.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_COUNT.INC'
      INCLUDE 'D0$INC:L1C_COUNT_FUTURE_USE.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_CARDS_USE.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
C
      INTEGER LUN
C
      INTEGER ISTAT
      INTEGER COUNT, REFSET, CARD
C----------------------------------------------------------------------
C
C
C       HOT_TOWER_CMP_RESULT
C
        WRITE (LUN,*)
        WRITE (LUN,*)
  500   FORMAT(' HOT_TOWER_CMP_RESULT  Count Ref#',I2, X, 8I1)
        DO COUNT = TOWER_CNT_THRSH_MIN, TOWER_CNT_THRSH_MAX
          WRITE (LUN,500,IOSTAT=ISTAT) COUNT, 
     &      (HOT_TOWER_CMP_RSLT(MOD(COUNT, CMP_PER_CARD)+1,
     &       COUNT/CMP_PER_CARD+1, REFSET),
     &       REFSET = EM_ET_REF_MIN,TOT_ET_REF_MAX)
        END DO
C
C       GLOBAL_ENERGY_CMP_RESULT
C
  600   FORMAT (' ',A7,4X, 3( X, 10I1), X, 2I1)
        WRITE (LUN,*)
        WRITE (LUN,*)
        WRITE (LUN,*) 'GLOBAL_ENERGY_CMP_RSLT'
        WRITE (LUN,*)
        WRITE (LUN,*) 
     &    '            0          1          2          3'
        WRITE (LUN,*) 
     &    'Threshold # 0123456789 0123456789 0123456789 01'
        WRITE (LUN,*)
     &    '            ---------------------------------------'
        WRITE (LUN,600,IOSTAT=ISTAT) 'EM  Et ', 
     &    ((GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, GL_EMET_THRTYP),
     &      COUNT = 1, 4),
     &     CARD = 1, CAT3_MAX)
        WRITE (LUN,600,IOSTAT=ISTAT) 'HD  Et ', 
     &    ((GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, GL_HDET_THRTYP),
     &      COUNT = 1, 4),
     &     CARD = 1, CAT3_MAX)
        WRITE (LUN,600,IOSTAT=ISTAT) '2nd EM ', 
     &    ((GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, GL_EML2_THRTYP),
     &      COUNT = 1, 4),
     &     CARD = 1, CAT3_MAX)
        WRITE (LUN,600,IOSTAT=ISTAT) '2nd HD ', 
     &    ((GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, GL_HDL2_THRTYP),
     &      COUNT = 1, 4),
     &     CARD = 1, CAT3_MAX)
        WRITE (LUN,600,IOSTAT=ISTAT) 'TOT Et ', 
     &    ((GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, GL_TOTET_THRTYP),
     &      COUNT = 1, 4),
     &     CARD = 1, CAT3_MAX)
        WRITE (LUN,600,IOSTAT=ISTAT) '2nd TOT', 
     &    ((GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, GL_TOTL2_THRTYP),
     &      COUNT = 1, 4),
     &     CARD = 1, CAT3_MAX)
C
C       TOTAL_MPT_CMP_RSLT
C
  700   FORMAT(' TOTAL_MPT_CMP_RSLT', 4(X, 8I1))
        WRITE (LUN,*)
        WRITE (LUN,*)
        WRITE (LUN,700,IOSTAT=ISTAT) 
     &    (TOTAL_MPT_CMP_RSLT(COUNT), COUNT = 1, MPT_CMP_MAX)
C
C
C----------------------------------------------------------------------
  999 RETURN
      END
