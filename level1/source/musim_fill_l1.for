      SUBROUTINE MUSIM_FILL_L1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make final statistics, fill in histograms and compare
C-   h/s 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-OCT-1992   Kamel A. Bazizi
C-   Updated  FEB-94        Jussara Miranda
C----------------------------------------------------------------------
      IMPLICIT NONE
C-- Total number of triggers/region for hardware and simulator
      INTEGER NTRIG_H(7),NTRIG_S(7)
      INTEGER IER, IERP
      LOGICAL HARDBK,SIMBK,ISABK,GOODCCT
      LOGICAL FIRST
        CHARACTER*72 STRING
      DATA FIRST/.TRUE./
C-- Skip event flag, generated in MU_SUP_L1
      LOGICAL SKIP
C
C----------------------------------------------------------------------

C-- Check if event skipped L1 simulation
      CALL SKIP_EVENT(SKIP)
      IF (SKIP) GO TO 999
C
C-- Read flags from MUSIM.RCP to book histograms for hardware and/or
C   simulator.

      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('MUSIM_RCP')
	CALL EZERR(IERP)     ! Check if error
	IF(IERP.EQ.0)THEN
           CALL EZGET('HARD_BK',HARDBK,IER)
           CALL EZGET('SIM_BK',SIMBK,IER)
           CALL EZGET('ISA_BK',ISABK,IER)
	   CALL EZRSET()
	ELSE
	   CALL EZGET_ERROR_TEXT(IER,STRING)
           CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'MUSIM_FILL_L1',STRING,'F')
           GOTO 999
	ENDIF
      ENDIF

C
C-- Go to MUSIM directory in memory

      CALL HCDIR('//PAWC/MUSIM_L1',' ')

C-- Do final statistics
      CALL L1BIT_COUNTERS(NTRIG_S, NTRIG_H)

C-- Fill in hardware histos
      IF(HARDBK) CALL HARD_FILL_L1(NTRIG_H)

C-- Fill in simulator histos
      IF(SIMBK) CALL SIM_FILL_L1(NTRIG_S)

C-- Fill in ISAJET histos
      IF(ISABK) CALL ISA_FILL_L1

C-- Compare hardware and simulator 
      CALL HS_COMPARE

C----------------------------------------------------------------------
      CALL HCDIR('//PAWC',' ')

  999 RETURN
      END
