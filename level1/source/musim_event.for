      LOGICAL FUNCTION MUSIM_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-OCT-1992   Kamel A. Bazizi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1_MUON_BITS(16),L15_MUON_BITS(16)
      INTEGER DELTA_WORDS,TRGR_MUON_DATA(5000),SKIP_LEVEL
      INTEGER IERR
C-- From MUSIM.RCP
      LOGICAL DOL1,DOL15

      INTEGER IER
      CHARACTER*80 STRING
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C-- initialization
C
      MUSIM_EVENT=.FALSE.
C
C-- Book Histograms
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C--     Read the to do flags
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_SUP_L1',STRING,'S')
          GOTO 999
        ENDIF
        CALL EZGET('DO_L1',DOL1,IER)   ! Flag to skip or not the L1 processing
        CALL EZGET('DO_L15',DOL15,IER)   ! Flag to skip or not the L1 processing
        CALL EZRSET()

        CALL MUSIM_BOOK_L1
        CALL MUSIM_BOOK_L15
      ENDIF

C-- Skip everything if DOL1=.FALSE.AND.DOL15=.FALSE
      IF (.NOT.(DOL1.OR.DOL15))GOTO 999 
C
C---------------------------------------------------------------
C  CALL to the MU_SUPERVISOR routine which returns with level 1,
C       level 1.5 muon trigger information
C---------------------------------------------------------------
C
C-- calculate the trigger information
C
      CALL MU_SUPERVISOR(L1_MUON_BITS,L15_MUON_BITS,DELTA_WORDS,
     &  TRGR_MUON_DATA,IERR)
      IF(IERR.NE.0) GOTO 888
C
C-- Fill Histograms
C
      CALL MUSIM_FILL_L1
      CALL MUSIM_FILL_L15
C
C-- Set Flag
C
  888 MUSIM_EVENT=.TRUE.
C
C----------------------------------------------------------------------
  999 RETURN
      END
