      SUBROUTINE L1ESUM_TRG2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the TRG2 ESUM summary bank from the
C-                         a summary of the highest et trigger towers
C-   Controls: None.
C-
C-   Created DEC-10-1992  Amber S. Boehnlein
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LTRGR_LEVEL1,GZTRGR
      INTEGER GZFIND_CRATE
      INTEGER GZESUM,LESUM
      LOGICAL OK, GOOD, FIRST
      INTEGER LIST_TYPE
      INTEGER EM_TT,JET_TT
      PARAMETER (EM_TT = 1)
      PARAMETER (JET_TT = 2)
      INTEGER FLAG_WORD
      INTEGER PHI,ETA
      REAL    CAL_ETA, CAL_PHI
      REAL    TT_ENERGY(EM_TT:JET_TT)
      REAL    TT_THRESH(EM_TT:JET_TT)
      INTEGER TT_TYPE(EM_TT:JET_TT)
      DATA FIRST/.TRUE./
      DATA TT_THRESH(EM_TT)/2.5/
      DATA TT_THRESH(JET_TT)/3.0/
      DATA TT_TYPE(EM_TT)/ID_ELECTRON/
      DATA TT_TYPE(JET_TT)/ID_JET/
C<<
C
      IF (FIRST) THEN
        CALL CL2_RING_INIT    ! FILL common block for conversion
                              ! eta and phi to offline phi.
        FIRST = .FALSE.
      END IF
C
C
C ****  Check if the TRG2 ESUM BANK is present, skip processing if so.
C
      LESUM = GZESUM('TRG2')
      IF(LESUM.EQ.0) THEN
C
C ****  Get the trigger block
C
        LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', GZTRGR(), 11 )
        IF (LTRGR_LEVEL1.GT.0) THEN
          DO PHI = PHI_MIN,PHI_MAX
            DO ETA = -ETA_MAX, ETA_MAX
              IF (ETA.NE.0) THEN
C    GET ENERGY
                CALL L1EXTRACT_TRGTWR_ADC_ENERGY(IQ(LTRGR_LEVEL1),
     &                 ETA,PHI,TT_ENERGY(EM_TT),TT_ENERGY(JET_TT))
C
C ****  If the tower energy is greater than the threshold, put them in ESUM
C
                DO LIST_TYPE = EM_TT , JET_TT
                  IF ( TT_ENERGY(LIST_TYPE).GE.TT_THRESH(LIST_TYPE))
     &              THEN
C GET physics coordinate
                    CALL PHYS_COR(PHI,ETA,CAL_PHI,CAL_ETA)
                    FLAG_WORD = 0
                    CALL ESUMFL('TRG2',TT_TYPE(LIST_TYPE),
     &                TT_ENERGY(LIST_TYPE),
     &                CAL_ETA,CAL_ETA,CAL_PHI,FLAG_WORD)
                  ENDIF      !passed the threshold?
                ENDDO        !tower type list
              ENDIF          !Skip eta = 0
            ENDDO            ! ETA loop
          ENDDO            ! PHI loop
          CALL ESUM_PUSH ! compact the banks
        ENDIF          ! TRGR bank must be present
      ENDIF          ! only process if the bank didn't exist
  999 RETURN
      END
