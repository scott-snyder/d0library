      SUBROUTINE GMD_CHECK_FLAGS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the PIXIE flags EXIT_PIXIE and
C-   GOTO_EVENT and take appropriate action.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-DEC-1991   Harrison B. Prosper
C-   Updated   7-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Correct FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      LOGICAL FLGVAL, OK, GB_READ_GOTO_EVENT
C----------------------------------------------------------------------
      IF     ( FLGVAL('EXIT_PIXIE') .AND. .NOT.
     &         FLGVAL('NEXT_EVENT') ) THEN
        CALL FLGSET('END_PROCESSING',.TRUE.)
      ELSEIF ( FLGVAL('GOTO_EVENT') ) THEN
        OK =  GB_READ_GOTO_EVENT(EVTCNT)
      ENDIF
      RETURN
      END
