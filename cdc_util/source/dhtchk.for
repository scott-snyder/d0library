      SUBROUTINE DHTCHK(LAYER,SECTOR,TASK,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set or check (task=1 or 2) the bit in the 
C-                         CDCH bank for hitfinding having been done 
C-                         in the specified sector
C-
C-   Inputs  : LAYER: layer #
C-             SECTOR: sector #
C-             TASK = 1: set bit of sector, 
C-                    2: check bit of sector
C-   Outputs : OK = TRUE if bit has been set for either task
C-
C-   Created   7-NOV-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LAYER, SECTOR, TASK
      INTEGER LKCDCH, GZCDCH, IBIT, IBSET, IH
      LOGICAL OK
C----------------------------------------------------------------------
      OK = .FALSE.
      IF ( TASK .LT. 1 .OR. TASK .GT. 2 ) GOTO 999
      LKCDCH = GZCDCH()
      IF ( LKCDCH .LE. 0 ) GOTO 999
      IBIT = SECTOR
      IF ( TASK .EQ. 1 ) THEN   
C
C    set bit in CDCH bank for the sector
C
        IH = IQ(LKCDCH + LAYER + 6)
        IQ(LKCDCH + LAYER + 6) = IBSET(IH,IBIT)
        OK = .TRUE.
      ELSE                        
C
C    check bit in CDCH bank for the sector
C
        IH = IQ(LKCDCH + LAYER + 6)
        OK = BTEST(IH,IBIT)
      ENDIF
C
  999 RETURN
      END
