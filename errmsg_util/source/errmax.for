      SUBROUTINE ERRMAX(KEY,LOG,WRN)
C--------------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the maximum number of times the identified
C-                         messages will be sent to the logging and warning
C-                         devices ; the count is still kept
C-
C-   Inputs  : KEY         The identified message
C-             LOG         The maximum number of times sent to logging unit
C-             WRN         The maximum number of times sent to warning unit
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann   
C-
C--------------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)KEY
      CHARACTER*32 KEYIN, KEYUPC
      INTEGER LOG, WRN
      LOGICAL FOUND
      INTEGER POS, I
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C-------------------------------------------------------------------------
      CALL ERRINT
      KEYIN = KEY                       ! Chop down to standard length
C
C **** if KEY = ' ' then change all MAXLOG and MAXWRN
C
      IF (KEYIN .EQ. ' ') THEN
        IF ( LOG .GE. 0 ) THEN
          LOGDFL = LOG
        ELSEIF ( LOG .LT. 0 ) THEN
          LOGDFL = LARGE
        ENDIF
        IF ( WRN .GE. 0 ) THEN
          WRNDFL = WRN
        ELSEIF ( WRN .LT. 0 ) THEN
          WRNDFL = LARGE
        ENDIF
C
        DO 10 I = 1, NENTRY
          MAXLOG(I) = LOGDFL
          MAXWRN(I) = WRNDFL
   10   CONTINUE
      ELSE
C
C **** KEY is not blank, set all MAXLOG and MAXWRN
C
        CALL UPCASE(KEYIN, KEYUPC)
        CALL ERRFND( KEYUPC, POS, FOUND )
        IF (OVFL) THEN
          OVLCNT = OVLCNT + 1
        ELSE
C
C **** Reset the individual MAXLOG and MAXWRN
C
          IF( .NOT. FOUND ) THEN
            CALL ERRINS(KEYUPC, POS)
          ENDIF
          IF (LOG .GE. 0) THEN
            MAXLOG(POS) = LOG
          ELSE
            MAXLOG(POS) = LARGE
          ENDIF
          IF (WRN .GE. 0) THEN
            MAXWRN(POS) = WRN
          ELSE
            MAXWRN(POS) = LARGE
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
