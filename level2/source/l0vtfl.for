      SUBROUTINE L0VTFL
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Extract the Level0 data from the
C_                         TRGR bank and put results into bank
C-                         L0VT which hangs under FILT.
C-
C-   INPUTS  : Level 0 vertex information from TRGR bank
C-   OUTPUTS : L0VT bank
C-   CONTROLS: NONE
C-
C-   CREATED   5-JUN-1992   TOM FAHLAND
C-   Updated  19-JUL-1992   James T. Linnemann  more paranoid zebra handling
C-                                              make parameter for z bin size
C-   Updated  21-SEP-1992   Jeffrey Bantly      include level0 slow z result
C-   Updated  14-OCT-1992   James T. Linnemann  rename to L0VTFL
C----------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FGOOD,SGOOD
      INTEGER GZL0VT, ZBIN, FGOOD_EVT, SGOOD_EVT
      INTEGER LL0VT, MI_FLAG
      REAL FZ_POS, SZ_POS
C----------------------------------------------------------------------------
      LL0VT = GZL0VT()
      IF (LL0VT .LE.0) CALL BKL0VT(LL0VT)
      IF (LL0VT .GT.0 ) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  EXTRACT L0 DATA
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL L2_L0_VERTEX(FZ_POS,FGOOD,SZ_POS,SGOOD,MI_FLAG)
C
        IF (FGOOD) THEN
          FGOOD_EVT = 1
        ELSE
          FGOOD_EVT = 2          ! board says it's bad, or no L0 info in event
        ENDIF
C
        IF ( SGOOD ) THEN
          SGOOD_EVT = MI_FLAG 
        ELSE
          SGOOD_EVT = 0
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  FILL ZEBRA BANK
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        Q(LL0VT+2) = FZ_POS
        IQ(LL0VT+3) = FGOOD_EVT
        Q(LL0VT+4) = SZ_POS
        IQ(LL0VT+5) = SGOOD_EVT
      ENDIF
C
  999 RETURN
      END
