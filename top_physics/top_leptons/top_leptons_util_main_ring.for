      SUBROUTINE TOP_LEPTONS_UTIL_MAIN_RING(TIME29,MR_BITS,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : extract the main ring info from trgr
C-
C-   Inputs  : none
C-   Outputs : TIME29 = time since the beginning of the last event 29
C-                      in seconds
C-
C-             MR_BITS = packed word with main_ring & tevatron 
C-                       veto and/or bits
C-         MR_BITS Bit #     Config Name    And_term
C-         -------------     -----------    --------
C-               0           MR_SPARE_13       112
C-               1           MR_SPARE_14       113
C-               2           MR_SPARE_15       114
C-               3           MR_SPARE_16       115
C-               4           MR_PERMIT         116
C-               5           MR_SPARE_10       117
C-               6           LV0_HALOPB        118
C-               7           MR_SPARE_12       119
C-               8           MR_CAL_LOW        120
C-               9           MR_MUON_LOW       121
C-               10          MR_CAL_HIGH       122
C-               11          MR_MUON_HIGH      123
C-               12          MRBS_LOSS         124
C-               13          MICRO_BLANK       125
C-               14          MIN_BIAS          126
C-               15          LV0_HALOP         127
C-
C-             IERR = 0 if OK
C-   Controls: none
C-
C-   Created  24-MAR-1993   John M. Butler
C-   Modified  9-APR-1993   JMB
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
CC----------------------------------------------------------------------
      LOGICAL        STATE
      INTEGER        LTRGR, LTRGR_LEVEL1,GZTRGR,GZFIND_CRATE
      INTEGER        LOW, HIGH
      PARAMETER    ( LOW  = 1, HIGH = 2 )
      INTEGER        SCALER_COUNT (LOW:HIGH)
      REAL           TIME29
      INTEGER        I,IERR,MR_BITS,IBSET
C----------------------------------------------------------------------
      IERR = -1
      TIME29 = -1.
      MR_BITS = 0
C
      LTRGR = GZTRGR()
      IF(LTRGR .NE. 0) THEN
        IERR = 0
C
C Get time since beginning of last Event 29
C
        LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', LTRGR, 11 )
        CALL L1UTIL_GET_FOREIGN_SCALER( IQ( LTRGR_LEVEL1 ), 4,
     &    SCALER_COUNT )
        TIME29=SCALER_COUNT(1)*3.493E-6
C
C Retrieve the 16 Main Ring Veto bits
C
        DO I=1,16
          CALL L1EXTRACT_ANDOR_TERM(IQ(LTRGR_LEVEL1),111+I,STATE)
          IF(STATE) MR_BITS = IBSET(MR_BITS,I-1)
        ENDDO
      ENDIF
C
  999 RETURN
      END
