      SUBROUTINE BKBRCP(ND,LBRCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK THE NEXT BRCP BANK
C-
C-   Inputs  :    ND    = Number of data words to book 
C-                LISAB = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : LBRCP - LINK OF BRCP BANK
C-   Controls: NONE
C-
C-   Created  28-JUN-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER LBRCP,LRECB,GZRECB
      INTEGER IXIO
      INTEGER ND
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZBRCP.LINK/LIST'
C
C----------------------------------------------------------------------
C
      LBRCP = 0
      IXIO = 1 ! MIXED DATA TYPE
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LRECB = GZRECB ()
      
      IF(LRECB.EQ.0 ) THEN
        CALL BKRECB(LRECB)
      END IF
      IF(LRECB.EQ.0) THEN
        CALL ERRMSG('BKBRCP','BKHEADR',
     &    ' CAN NOT MAKE BEGIN RUN RECB BANK','W')
        GOTO 999
      END IF
C                                      
      CALL MZBOOK
     &  (IXDVR,LBRCP,LRECB,-IZBRCP,'BRCP',1,1,ND,IXIO,0)
C
C----------------------------------------------------------------------
  999 RETURN
      END
