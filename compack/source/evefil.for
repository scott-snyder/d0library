      SUBROUTINE EVEFIL(FILNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call EVE editor to edit FILNAM. VAX-specific
C-
C-   Inputs  : FILNAM : Name of file to edit
C-   Outputs : None
C-
C-   Created  28-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C&IF VAXVMS
      INTEGER ISTAT,TPU$TPU,LIBREP,CURONF
C----------------------------------------------------------------------
      IF(FULSCR) ISTAT=CURONF(0)
      CALL BROADC(.FALSE.)        !Turn off broadcast trapping
      ISTAT=TPU$TPU('TPU '//FILNAM)
      IF(.NOT.ISTAT) THEN
        CALL MSGSCR(ISTAT,'EVE return-->')
        CALL PFWAIT
      ELSE
        ISTAT=LIBREP()
      ENDIF
      CALL BROADC(.TRUE.)        !Turn on broadcast trapping again
C&ELSE
C&      CALL OUTMSG('0File edit NOT implemented here!')
C&ENDIF
  999 RETURN
      END
