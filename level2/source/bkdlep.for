      SUBROUTINE BKDLEP(LDLEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book DLEP bank under Filter results.
C-
C-   Inputs  : none
C-   Outputs : LDLEP = Address of the booked bank.
C-   Controls: none
C-
C-   Created  22-APR-1991   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZDLEP.LINK'
C
      INTEGER LDLEP,LSUP,GZFRES
      INTEGER NIO,ND
      DATA ND /200/
C
C----------------------------------------------------------------------
C
      CALL MZFORM('DLEP','1I -F',NIO)
C
      LSUP  = GZFRES()                  ! link to particle header bank
      IF (LSUP .LE. 0) CALL BKFRES(LSUP)
C
      LDLEP = LQ(LSUP - IZDLEP)
      IF (LDLEP.EQ.0) THEN
        CALL MZBOOK(IXMAIN,LDLEP,LSUP,-IZDLEP,'DLEP',0,0,ND,NIO,0)
      ENDIF
C
  999 RETURN
      END
