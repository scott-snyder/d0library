      FUNCTION PXEVENT_READ_OK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface between PBD routine EVENT_READ_OK()
C-                         and PIXIE routine PUFINDEVNT.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-SEP-1992   Nobuaki Oshima and Jasbir Singh
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GETEVNT, PXEVENT_READ_OK, FLGVAL
C----------------------------------------------------------------------
      PXEVENT_READ_OK = .TRUE.
C
C ****  Find run and event number
C
      IF ( FLGVAL('GOTO_EVENT') ) THEN
        CALL PUFINDEVNT(GETEVNT)
        IF ( GETEVNT ) THEN
          PXEVENT_READ_OK = .FALSE.
        ENDIF
      ENDIF
  999 RETURN
      END
