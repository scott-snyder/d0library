      SUBROUTINE GTL0VX(BUNCH,WORDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch contents of Zebra bank L0VX for a 
C-                         given bunch.
C-
C-   Inputs  : BUNCH - bunch number
C-   Outputs : WORDS - unpacked vertex data for selected bunch
C-   Controls: None
C-
C-   Created  21-JUL-1992   Freedy Nang
C-   Updated  30-NOV-1992   Jeffrey Bantly  change position of VZERO call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER BUNCH, IBUNCH
      INTEGER WORDS(52)
      INTEGER LKL0VX
      INTEGER I
      INTEGER GZL0VX_BUNCH, LZFIND
      EXTERNAL GZL0VX_BUNCH, LZFIND
C----------------------------------------------------------------------
C
C ****  Fetch bank link of desired bunch.
C
      LKL0VX=GZL0VX_BUNCH(BUNCH)
C
C ****  Load array with bank values.
C
      CALL VZERO(WORDS,52)
      IF ( LKL0VX.NE.0 ) THEN
        IBUNCH=IQ(LKL0VX+1)
        DO 10 I = 1, 52
          WORDS(I)=IQ(LKL0VX+I)
   10   CONTINUE
      ELSE
C
C ****  else zero bunch number.
C
        IBUNCH=0
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
