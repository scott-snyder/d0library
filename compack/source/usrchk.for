      SUBROUTINE USRCHK(NUMQ,CHARIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called when there is a USER qualifier on command
C-                         line.
C-
C-                         This version is a DUMMY template.
C-
C-   Inputs  : NUMQ:    Number of qualifiers in list
C-             CHARIN : Character strings from the /USER=CHARIN on command
C-                      line
C-   Outputs : None
C-
C-   Created  10-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMQ
      CHARACTER*(*) CHARIN(NUMQ)
      CHARACTER*132 CTEMP
      INTEGER I,WAITIT
      CHARACTER*20 CIN
C----------------------------------------------------------------------
      DO I=1,NUMQ
C&IF VAXVMS
        CALL INTMSG('0CHARIN part->'//CHARIN(I)(1:LEN(CHARIN(I))))
C&ENDIF
C&IF SIUNIX,IBMAIX,ULTRIX,ALFOSF
C&        CTEMP = '0CHARIN part->'//CHARIN(I)(1:LEN(CHARIN(I)))
C&        CALL INTMSG(CTEMP)
C&ENDIF
      ENDDO
      CALL GETPAR(1,'Ready to continue?','C',CIN)
  999 RETURN
      END
