      SUBROUTINE DUMP_TOP_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE OUT PROPERTIES OF TOP CANDIDATE
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
      REAL    WMASS_CUT,TOP_CUT
      INTEGER TUNIT,IER
      INTEGER INUM
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('DUMP_W_EVENT',DUMP_EVENT,IER)
        CALL EZGET('W_TRANSVERSE_MASS_CUT',WMASS_CUT,IER)
        CALL EZGET('TOP_LIKELIHOOD_CUT',TOP_CUT,IER)
        CALL EZGET('HMATRIX_USER',HM_USER,IER)
        CALL EZ_FILE_OPEN(HM_USER,'TOP_DUMP_FILE','OF',TUNIT,
     &    FILENAME,IER)
        WRITE(TUNIT,1)WMASS_CUT,TOP_CUT
    1   FORMAT(' ********** TOP DUMP FILE *********** ',/,
     &    ' ALL TOPS WITH W TRANSVERSE MASS GREATER THAN ',F15.7,/,
     &    ' AND TOP LIKELIHOOD  GREATER THAN ',F15.7,/,
     &    ' WILL BE DUMPED ',//)
        INUM = 0
        CALL EZRSET
      ENDIF
C
      IF ( TRMASS.GT.WMASS_CUT.AND.TOP_LIKE.GT.TOP_CUT) THEN
        INUM = INUM + 1
        WRITE(TUNIT,2)INUM
    2   FORMAT(' NUMBER OF TOPS FOUND SO FAR ',I8)
        CALL DUMP_EVENT_QUAN(TUNIT)
      ENDIF
C
  999 RETURN
      END
