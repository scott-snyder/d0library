      SUBROUTINE LJTOP_DECODE_NTUPLE(USE_EVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Ntuple info and fill in quans.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTUPLE_ID
      CHARACTER*32 NTUPLE_FILES(200)
      INTEGER NCHRS
      CHARACTER*32 TOP_DIR,FILE
      SAVE TOP_DIR,FILE
      INTEGER IER
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL USE_EVENT
C----------------------------------------------------------------------
      IF ( FIRST  ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CALFRAME_RCP')
        CALL EZGET('NTUPLE_ID',NTUPLE_ID,IER)
        CALL EZ_GET_CHARS('NTUPLE_FILES',NCHRS,NTUPLE_FILES,IER)
        FILE    = NTUPLE_FILES(1)
        TOP_DIR = NTUPLE_FILES(2)  !ASSUME 1ST LINE IS READ FILE
        CALL EZRSET
      ENDIF
C
      IF ( TOP_DIR.EQ.'FAKES' ) THEN
        CALL LJTOP_DECODE_QCD_FAKE_NTUPLE(USE_EVENT)
      ELSE
        CALL LJTOP_DECODE_MCDAT_NTUPLE(USE_EVENT)
      ENDIF
C
  999 RETURN
      END
