      LOGICAL FUNCTION MUSIM_INIT()
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
      INTEGER IER,TRMASK
      CHARACTER*72 STRING
C
      MUSIM_INIT=.FALSE.
C-- Initialize /ZEBSTP/
C
      CALL INZSTP
C
C-- Read control file into an SRCP bank
C
      CALL INRCP('MUSIM_RCP',IER)
C
      IF(IER.NE.0) THEN
        CALL ERRMSG(' INRCP','MUSIM_INIT','Could not read: '
     &  // 'MUSIM_RCP','F')
        GOTO 999
      ENDIF
C
C-- Get the trigger region mask word from RCP file 
C
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MUSIM_INIT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('TRG_REG_MSK_WORD',TRMASK,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' TRG_REG_MSK_WORD','MUSIM_INIT',STRING,'F')
          GOTO 999
        ENDIF
C
C-- De-select RCP bank
C
      CALL EZRSET()

C
C-- Set trigger region mask bits in MU_SUPERVISOR
C
      CALL MU_TRIG_REG_MASK(TRMASK)

C
C-- Read in the muon level 1.5 Tables
C
      CALL MU_L15_TAB_INIT

C
C.. Create a new Zebra subdirectory for MUSIM histograms
      CALL HMDIR('//PAWC/MUSIM_L1','S')

C.. Create a new Zebra subdirectory for MUSIM histograms
      CALL HMDIR('//PAWC/MUSIM_L15','S')

C-- reset Flag if everything is OK
C
      MUSIM_INIT=.TRUE.
C
C----------------------------------------------------------------------
  999 RETURN
      END
