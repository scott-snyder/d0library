      SUBROUTINE DUMP_W_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE OUT PROPERTIES OF W CANDIDATES
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-SEP-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      LOGICAL FIRST,DUMP_EVENT
      INTEGER HM_USER
      CHARACTER*80 FILENAME
      REAL    WMASS_CUT
      INTEGER WUNIT,IER
      INTEGER INUM
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('DUMP_W_EVENT',DUMP_EVENT,IER)
        CALL EZGET('W_TRANSVERSE_MASS_CUT',WMASS_CUT,IER)
        CALL EZGET('HMATRIX_USER',HM_USER,IER)
        CALL EZ_FILE_OPEN(HM_USER,'W_DUMP_FILE','OF',WUNIT,
     &    FILENAME,IER)
C
        WRITE(WUNIT,1)WMASS_CUT
    1   FORMAT(' ********** W DUMP FILE *********** ',/,
     &    ' ALL WS WITH TRANSVERSE MASS GREATER THAN ',F15.7,/,
     &    ' WILL BE DUMPED ',//)
        CALL EZRSET
        INUM = 0
      ENDIF
C
      IF ( TRMASS.GT.WMASS_CUT) THEN
        INUM = INUM + 1
        WRITE(WUNIT,2)INUM
    2   FORMAT(' NUMBER OF WS FOUND SO FAR ',I8)
        CALL DUMP_EVENT_QUAN(WUNIT)
      ENDIF
C
  999 RETURN
      END
