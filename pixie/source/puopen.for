      SUBROUTINE PUOPEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a new retained segment, and update MAXSEG
C-
C-   Inputs  : /PXPARA/ MAXSEG : last retained segment number
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-AUG-1988   Olivier Callot
C-   Modified 14-NOV-1990   Nobu. (makes retained seg. list for rotation.)
C-   Modified 18-DEC-1990   Nobu. (cancel the modification on 14-NOV-1990,
C-                                 due to header, menu and message are
C-                                 temporary segment now.)
C-   Updated  18-DEC-1990   Harrison B. Prosper
C-                          Add call to PU_UPDATE_SEGMENT_LIST
C-   Updated   2-JAN-1991   Nobuaki Oshima
C-      Remove PU_UPDATE_SEGMENT_LIST (not needed anymore!)
C-      Add entry point pu_get_segment_number
C-   Updated   9-JAN-1991   Harrison B. Prosper
C-      Add entry point PUOPEN_CHECK_VIEW3D
C-   Updated  29-JAN-1991   Lupe Howell
C-      The filling of segment type array only when E&S.
C-   Modified 27-DEC-1991   Nobuaki Oshima
C-      Add 'STORE_VIEWP_WINDO'for 3D picking with rotation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
C----------------------------------------------------------------------
      LOGICAL CHECK_VIEW3D, CHECK,FIRST,CALLED_PUOPEN
      INTEGER CURSEG, NSMAX,IDX,IER,IVAL
      PARAMETER( NSMAX = 512 )
      INTEGER SEGTYPE(NSMAX)
      INTEGER NSEG,RSEGTYP(*)
      REAL    RLEVEL
      CHARACTER*40 COMM
      SAVE CHECK_VIEW3D,CALLED_PUOPEN
      DATA FIRST /.TRUE./
      DATA CALLED_PUOPEN/.FALSE./
C----------------------------------------------------------------------
      CALLED_PUOPEN = .TRUE.
C
      CALL PX_CLOSE_OPEN_SEGMENT
C
      MAXSEG = MAXSEG + 1
C
C **** FILL SEGMENT TYPE ARRAY. Only get the VIEW3D parameter
C **** if the logical CHECK_VIEW3D is TRUE. This logical is set
C **** by PUOPEN_CHECK_VIEW3D which is called from PUMENUDO
C **** If we are not using Evans and Sutherland this will not
C **** be perform.
C
      IF ( FIRST ) THEN
        CALL JIQDIL(RLEVEL)
        FIRST = .FALSE.
      ENDIF
      IF ( RLEVEL .EQ. -2.) THEN
        IF (MAXSEG .LE. NSMAX) THEN
          IF ( CHECK_VIEW3D ) THEN
C
C ****  Check the error code: The active command may NOT be
C ****  associated with a SCREEN.
C
            CALL PU_GET_ACTIVE_SCREEN(COMM)
            CALL PU_GET_SCREEN_INDEX(COMM, IDX, IER)
C
            IF ( IER .EQ. 0 ) THEN
              CALL PU_GET_SCREEN_PARAM(IDX,'VIEW3D', IVAL, IER)
              IF ( IER .NE. 0 ) THEN
                IVAL = 0                  ! VIEW3D Not found
              ENDIF
            ENDIF
          ELSE
            IVAL = 0
          ENDIF
          SEGTYPE(MAXSEG) = IVAL
        ELSE
          CALL PUMESS('*** PUOPEN - MAXSEG exceeds 512!',
     &         'This retained segment cannot be rotated. ***')
        ENDIF
      ENDIF
C
C ****  Open retained segment MAXSEG
C
      CALL JROPEN( MAXSEG )
      PICSEG   = MAXSEG
      RETURN
C
C ****  Return current retained segment number and type list
C
      ENTRY PU_GET_SEGMENT_TYPE(NSEG,RSEGTYP)
      NSEG = MAXSEG
      CALL UCOPY(SEGTYPE,RSEGTYP,NSEG)
      RETURN
C
      ENTRY PUCLOSE
      IF ( CALLED_PUOPEN ) THEN
        CALLED_PUOPEN = .FALSE.
        CALL JRCLOS
      ENDIF
      RETURN
C
      ENTRY PUOPEN_CHECK_VIEW3D(CHECK)
      CHECK_VIEW3D = CHECK
  999 RETURN
      END
