      LOGICAL FUNCTION NP_SCALAR ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter for high scalar Et events.
C-
C-   Returned value: .TRUE. to keep the event, .FALSE. on error.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   ENTRY  NP_SCALAR_EOJ  performs end-of-job summary for this filter.
C-   Created  14-DEC-1992   Marc Paterno
C-   Updated  16-FEB-1993   Jay A. Wightman  Modify to cut on total E 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      LOGICAL  NP_SCALAR_EOJ
C----------------------------------------------------------------------
      INTEGER  LCAEP, GZCAEP, LGLOB, GZGLOB, LPNUT, GZPNUT, IER
      REAL     SUMMARY(20)
      INTEGER  I, NCH
      EXTERNAL GZCAEP, GZGLOB, GZPNUT
      REAL     ETOT, ETOT_CUT, SCALAR_ET, SCALAR_ET_CUT
      LOGICAL  FIRST
      DATA     FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP ('NP_SCALAR_RCP', IER)

        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'Could not find NP_SCALAR_RCP',
     &      'NP_SCALAR', ' ', 'F')
        ENDIF                           ! if ier .eq. 0

        CALL EZGET ('ETOT_CUT', ETOT_CUT, IER)
        CALL EZGET ('SCALAR_ET_CUT', SCALAR_ET_CUT, IER)
        CALL EZRSET


      ENDIF                             ! if first
C----------------------------------------------------------------------
      NP_SCALAR = .FALSE.               ! reject by default

      LGLOB=GZGLOB()
      LCAEP=GZCAEP()
      LPNUT=GZPNUT(2)

      IF(LGLOB.GT.0) THEN
        ETOT=Q(LGLOB+8)
        IF(ETOT.GT.ETOT_CUT) RETURN
      ELSEIF(LCAEP.GT.0) THEN
        NCH=IQ(LCAEP+3)
        ETOT=0.0
        DO I=1,NCH
          ETOT=ETOT+Q(LCAEP+3+I*2)
        ENDDO
        IF(ETOT.GT.ETOT_CUT) RETURN
      ENDIF
      IF(LPNUT.LE.0) RETURN
      SCALAR_ET=Q(LPNUT+14)
      IF(SCALAR_ET.LT.SCALAR_ET_CUT) RETURN

      NP_SCALAR = .TRUE.
      RETURN
C#######################################################################
      ENTRY NP_SCALAR_EOJ (SUMMARY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job summary for this filter.  Dummy for now.
C-
C-   Inputs  : none
C-   Outputs : SUMMARY   [R(20)] array of 20 integers
C-   Controls: none
C-
C-   Created  18-DEC-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      NP_SCALAR_EOJ = .TRUE.
      CALL VZERO (SUMMARY, 20)
      RETURN
      END
