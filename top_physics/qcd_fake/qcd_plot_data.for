      SUBROUTINE QCD_PLOT_DATA(IOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plot data quans (unsmeared)
C-
C-   Inputs  : IOFF = offset for histograms to appear in
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INTEGER IOFF
      INTEGER IER
      INTEGER I
C----------------------------------------------------------------------
      CALL DHDIR_DECLARE_FILE('FAKES')
C
      CALL DHDIR('QCD_FAKE_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('QCD_FAKE','QCD_FAKE_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      CALL DO_HF1(IOFF+1,METC1,1.0) !CORRECTED MISSING ET
      CALL DO_HF1(IOFF+2,WMTC,1.0)  !CORRECTED TRANSVERSE MASS
      DO I = 1 , NELE
        CALL DO_HF1D(IOFF+3,ELEC(5,I),1.0)
      ENDDO
C
      DO I = 1 , NJETS
        CALL DO_HF1D(IOFF+4,JETS(5,I),1.0)
      ENDDO
      CALL DO_HF1(IOFF+11,FLOAT(NJETS),1.0)
      CALL DO_HF1(IOFF+12,NPELC,1.0)
C
      CALL DO_HF1D(IOFF+5,REST_PT,1.0)
      IF ( IOFF.EQ.300.AND.BAD_ELE.EQ.0) THEN
        CALL DO_HF1D(601,ZMASS,1.0)  !ZMASS FOR ALL DATA
      ENDIF
C
  999 RETURN
      END
