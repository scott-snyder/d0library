      SUBROUTINE ISRCFL(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank ISRC
C-
C-   Inputs  :RCP_BANK - RCP bank to fill ISRC with
C-
C-   Outputs :NONE
C-   Controls:NONE
C-
C-   Created  11-JAN-1990 16:49:35.86  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LISRC, GZISRC, LRCP, ND
      CHARACTER*(*) RCP_BANK
C----------------------------------------------------------------------
C ****  GET RCP BANK ADDRESS & BANK SIZE
C
      CALL EZLOC(RCP_BANK,LRCP)
      ND = IC(LRCP-1)
C
C ****  BOOK ISRC BANK
C
      CALL BKISRC(ND,LISRC) 
C
C ****  FILL ISRC BANK WITH RCP DATA
C
      CALL UCOPY (IC(LRCP+1), IQ(LISRC+1), ND)
C
C----------------------------------------------------------------------
  999 RETURN
      END
