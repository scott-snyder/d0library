      LOGICAL FUNCTION TRD_ANALYSIS_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization for package TRD_ANALYSIS
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JUN-1993   Alain PLUQUET
C-   Updated   4-OCT-1994   Alain PLUQUET  Fatal error if no STP file found
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*26 FILNAM
      INTEGER IER,L,LOC
      LOGICAL FIRST,TAI
      DATA FIRST /.TRUE./
      SAVE TAI
      IF (FIRST) THEN
        FIRST = .FALSE.
        IER = 0
        CALL EZLOC ('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_RCP',IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK('TRD_RCP')
          CALL EZGETS ('TRD_STPFILE',1,FILNAM,L,IER)
          CALL EZRSET
          IF (IER.EQ.0) THEN
            CALL INTMSG(' READ STATIC PARAMETER FILE '//FILNAM)
            CALL TRISTP(FILNAM,IER)
            IF (IER.EQ.0) THEN
              TAI=.TRUE.
            ELSE
              TAI=.FALSE.
              CALL ERRMSG(' TRD_ANALYSIS_INI','TRD_ANALYSIS_INI',
     &     'No STP file. Wrong initialization of TRD_ANALYSIS package.',
     &        'F')
              CALL INTMSG(' TRD_ANALYSIS_INI : can not open file '//
     &          FILNAM)
            ENDIF
          ELSE
            TAI=.FALSE.
            CALL ERRMSG(' TRD_ANALYSIS_INI','TRD_ANALYSIS_INI',
     &            'Can not find STP file name','F')
          ENDIF
        ELSE
          TAI=.FALSE.
          CALL ERRMSG(' TRD_ANALYSIS_INI','TRD_ANALYSIS_INI',
     &            'Wrong RCP file name','F')
        ENDIF
      ENDIF
      TRD_ANALYSIS_INI = TAI
      END
