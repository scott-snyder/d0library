      SUBROUTINE EVTMSG(EVTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns a SWORDS'd string with the event
C-                         and run numbers formatted into it.
C-
C-   Inputs  : None
C-   Outputs : EVTSTR      String with the run & event string
C-   Controls: None
C-
C-   Created  21-JUN-1989   Jason McCampbell (MSU)
C-   Updated  14-APR-1992   James T. Linnemann  add ELN version 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) EVTSTR
      CHARACTER*12 RSTR, ESTR
      INTEGER RUN,EVENT
C&IF VAXELN
C&      INTEGER GL_RUNNO,HIGH2,IAND,LOW2
C&      PARAMETER( LOW2 = 2**16-1 )
C&ELSE
C&ENDIF
      INTEGER I1,J1,N1, I2,J2,N2
C----------------------------------------------------------------------
C&IF VAXELN
C&      RUN = GL_RUNNO()                  !global run number
C&      CALL EVINUM(HIGH2,EVENT)          !just grab lowest 3 bytes
C&C      EVENT = IAND(EVENT,LOW2)          !and actually chop down to 2 bytes
C&ELSE
      CALL EVNTID(RUN,EVENT)            ! Get numbers
C&ENDIF
      WRITE(RSTR, FMT=100) RUN          ! Convert both to strings
  100 FORMAT(I12)
      WRITE(ESTR, FMT=100) EVENT
      CALL SWORDS(RSTR, I1,J1,N1)       ! Trim down strings
      CALL SWORDS(ESTR, I2,J2,N2)
C&IF VAXELN
C&      WRITE(EVTSTR, FMT=200) RSTR(I1:J1), ESTR(I2:J2) !duplicate for D0FLAVOR
C&  200 FORMAT(' Gl Run ',A,' L1 Evt ',A)
C&ELSE
      WRITE(EVTSTR, FMT=200) RSTR(I1:J1), ESTR(I2:J2)
  200 FORMAT(' Run ',A,' Event ',A)
C&ENDIF
C
  999 RETURN
      END
