      FUNCTION ZTR_RESET ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RESET CD PACKAGE HISTOGRAMS AT BEGINNING OF RUN
C-
C-   Returned value  : TRUE 
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: ZTRAKS_RCP, VERTEX_RCP, DTRAKS_RCP, FTRAKS_RCP, VTRAKS_RCP
C-
C-   Created  24-APR-1991   Chip Stewart
C-   Updated  25-APR-1991   Susan K. Blessing   
C-   Updated  28-APR-1992   Susan K. Blessing  Remove error notification
C-    if DHDIR fails.  Assume directory doesn't exist.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER
C
      LOGICAL ZTR_RESET
C
C----------------------------------------------------------------------
C
      ZTR_RESET = .TRUE.
C
C ****  RESET ZTRAKS
      CALL DHDIR('ZTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
C ****  RESET VERTEX 
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
C ****  RESET DTRAKS 
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
C ****  RESET FTRAKS 
      CALL DHDIR('FTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
C ****  RESET VTRAKS 
C VTRAKS doesn't use this at the moment
C      CALL DHDIR('VTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
C      IF (IER.EQ.0) CALL HRESET(0,' ')
C
C ****  RESET TRD
      CALL DHDIR('TRD_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
C **** RESET SPECIAL EXAMINE DIRECTORIES
      CALL DHDIR(' ','//PAWC/CD',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
      CALL DHDIR(' ','//PAWC/VTX',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
      CALL DHDIR(' ','//PAWC/CDC',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
      CALL DHDIR(' ','//PAWC/TRH',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
      CALL DHDIR(' ','//PAWC/FDC',IER,' ')
      IF (IER.EQ.0) CALL HRESET(0,' ')
C
  999 RETURN
      END
