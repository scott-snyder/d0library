      SUBROUTINE BKHITS(LHITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        Book HITS bank
C-        PATHBK will book supporting banks if missing
C-        supporting banks depend on chosen path.
C-
C-   Outputs : 
C-      LHITS = pointer to HITS bank
C-
C-   Created  25-APR-1989   Serban D. Protopopescu
C-   Updated  26-OCT-1995   Qizhong Li-Demarteau   added one more stru. link 
C-                             (from 8 to 9 links) and changed version # to 3
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZHITS.LINK/LIST'
C
      INTEGER LHITS,LSUP
      INTEGER NDATA
      PARAMETER (NDATA=1)
C  --------------------------------------------------------------
C
      CALL PATHBK(LSUP)   ! Get supporting link or book if missing
C
      LHITS=LQ(LSUP-IZHITS)
      IF(LHITS.NE.0) GOTO 999  ! return if HITS exists already
C
C   Create HITS bank 
C
      CALL MZBOOK(IXMAIN,LHITS,LSUP,-IZHITS,
     +            'HITS',9,9,NDATA,2,-1)
C
      IQ(LHITS+1)=3          ! version number
C
  999 RETURN
      END
