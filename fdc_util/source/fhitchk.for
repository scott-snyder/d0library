      SUBROUTINE FHITCHK( HALF,UNIT,QUAD,SECTOR,TASK,OK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set or check (task=1 or 2) the bit in the 
C-                         FDC Unit Hits bank for hitfinding having
C-                         been done in the specified sector
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR = Logical Address location of sector
C-             TASK =  1-set bit of sector, 2-check bit of sector
C-   Outputs : OK = TRUE if bit has been set for either task
C-
C-   Created  12-JUL-1990   Jeffrey Bantly
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER HALF,UNIT,QUAD,SECTOR,TASK
      INTEGER LKFDUN,IBIT,IH,OFFSET
      INTEGER GZFDUN
C
      LOGICAL OK
C
C----------------------------------------------------------------------
      OK = .FALSE.
C
      IF ( TASK .LT. 1 .OR. TASK .GT. 2 ) GOTO 999
      LKFDUN = GZFDUN(HALF,UNIT)
      IF ( LKFDUN .LE. 0 ) GOTO 999
      IF ( UNIT .LE. 0 ) THEN
        OFFSET = QUAD/4
        IBIT = (QUAD - (OFFSET*4))*8 + SECTOR
      ELSEIF ( UNIT .EQ. 1 ) THEN
        OFFSET = SECTOR/18
        IBIT = SECTOR - (OFFSET*18)
      ELSE
        GOTO 999
      ENDIF
      IF ( TASK .EQ. 1 ) THEN           ! set bit in unit bank for sector
        IH = IQ(LKFDUN + OFFSET + 2)
        IQ(LKFDUN + OFFSET + 2) = IBSET(IH,IBIT)
        OK = .TRUE.
      ELSE                              ! check bit in unit bank for sector
        IH = IQ(LKFDUN + OFFSET + 2)
        OK = BTEST(IH,IBIT)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
