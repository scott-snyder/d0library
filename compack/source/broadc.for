      SUBROUTINE BROADC(ONOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Turn on or off trapping of broadcast messages
C-                         VAX-specific
C-
C-   Inputs  : ONOFF : Logical, .FALSE. for OFF, .TRUE. for ON
C-   Outputs : None
C-
C-   Created  19-FEB-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ONOFF
C&IF VAXVMS
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,SMG$DISABLE_BROADCAST_TRAPPING
      INTEGER SMG$SET_BROADCAST_TRAPPING
      LOGICAL ON/.FALSE./
      EXTERNAL BROAST
C----------------------------------------------------------------------
      IF(SMGON) THEN
        IF(ONOFF.AND..NOT.ON) THEN
          ISTAT=SMG$SET_BROADCAST_TRAPPING(PASTID,BROAST,)
          IF(.NOT.ISTAT) THEN
            CALL MSGSCR(ISTAT,'SET_BROADCAST-->')
          ELSE
            ON=.TRUE.
          ENDIF
        ELSEIF(ON) THEN
          ISTAT=SMG$DISABLE_BROADCAST_TRAPPING(PASTID)
          IF(.NOT.ISTAT) THEN
            CALL MSGSCR(ISTAT,'DIS_BROADCAST-->')
          ELSE
            ON=.FALSE.
          ENDIF
        ENDIF
      ENDIF
C&ENDIF
  999 RETURN
      END
