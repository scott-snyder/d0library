      LOGICAL FUNCTION IS_DATAFILE ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test to see if the current event is DATA or Monte
C-   Carlo.
C-
C-   Returned value  : TRUE for data, FALSE for Monte Carlo
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-SEP-1993   Marc Paterno
C-   Updated  25-OCT-1993   Marc Paterno  Alter to conform to D0 standards.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER  GZHEAD, ITYPE
      EXTERNAL GZHEAD
C----------------------------------------------------------------------
      LHEAD = GZHEAD()
      ITYPE = IQ(LHEAD+1)
      IF (ITYPE .GE. 5 .AND. ITYPE .LE. 999) THEN
        IS_DATAFILE = .TRUE.
      ELSE IF (ITYPE .GE. 1005 .AND. ITYPE .LE. 1999) THEN
        IS_DATAFILE = .FALSE.
      ELSE
        IS_DATAFILE = .FALSE.
        CALL ERRMSG ('UNRECOGNIZED TYPE', 'IS_DATAFILE',
     &    'Event header error: neither DATA nor MC', 'W')
      ENDIF
      RETURN
      END
