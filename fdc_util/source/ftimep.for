      FUNCTION FTIMEP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read electronic channel for timing pulse
C-                         from FTRAKS_RCP, translate it to a logical
C-                         channel and put into FTRAKS_RCP
C-
C-   Inputs  : none
C-   Outputs : Timing pulse logical channel address to RCP
C-
C-   Created  29-MAR-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER TPECHN,TIMCHA             ! Electronic, Logical address for
C                                       ! timing pulse
      INTEGER TIMPCH,TIMPCU,TIMPCQ,TIMPCS,TIMPCW
      INTEGER UBIT,IER
      LOGICAL FTIMEP
      LOGICAL FIRST
C
      SAVE FIRST,TPECHN
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      FTIMEP = .TRUE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TPECHN',TPECHN,IER)
        CALL EZRSET
        IF (TPECHN.NE.-1) THEN
C
          CALL NUM_TO_ADDR(TPECHN,TIMCHA,1)     ! conversion routine
          CALL FCODER(TIMCHA,TIMPCH,TIMPCU,TIMPCQ,TIMPCS,TIMPCW,UBIT,1)
C
          CALL EZPICK('FTRAKS_RCP')
          CALL EZSET('TIMPCH',TIMPCH,IER)
          CALL EZSET('TIMPCU',TIMPCU,IER)
          CALL EZSET('TIMPCQ',TIMPCQ,IER)
          CALL EZSET('TIMPCS',TIMPCS,IER)
          CALL EZSET('TIMPCW',TIMPCW,IER)
          CALL EZSET('TIMCHA',TIMCHA,IER)
          CALL EZRSET
        END IF
       END IF
C-----------------------------------------------------------------------
  999 RETURN
      END
