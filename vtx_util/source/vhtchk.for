      SUBROUTINE VHTCHK(LAYER,SECTOR,IFL,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set (IFL=1) or check (IFL=2) the bit in the
C-                         VTXH bank indicating that hitfinding for LAYER and
C-                         SECTOR has been done.
C-
C-   Inputs  : LAYER, SECTOR
C-             IFL : =1 ==> Set the bit indicating hitfinding has been done
C-                   =2 ==> Return the value of the bit corresponding to LAYER,
C-                   SECTOR
C-   Outputs : OK : for IFL = 1, always returned .TRUE.
C-                  for IFL = 2, .TRUE. if hitfinding already done
C-   Controls: 
C-
C-   Created  26-FEB-1991   Peter Grudberg from DHTCHK
C-   Updated  15-OCT-1991   Peter M. Grudberg  Remove TYPE input (strips
C-                                             removed) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LAYER, SECTOR, IFL
      LOGICAL OK
C
      INTEGER IH, LVTXH, GZVTXH, IBIT
C----------------------------------------------------------------------
      OK = .FALSE.
      IF ( IFL .LT. 1 .OR. IFL .GT. 2 ) GO TO 999
      LVTXH = GZVTXH()
      IF ( LVTXH .LE. 0 ) GO TO 999
      IBIT = SECTOR
      IF ( IFL .EQ. 1 ) THEN
C
C ****  Set bit in VTXH corresponding to LAYER, SECTOR
C
        IH = IQ(LVTXH + LAYER + 10)
        IQ(LVTXH + LAYER + 10) = IBSET(IH,IBIT)
        OK = .TRUE.
      ELSE
C
C ****  Check bit corresponding to LAYER, SECTOR.  If this bit is set,
C ****  hitfinding has been done.
C
        IH = IQ(LVTXH + LAYER + 10)
        OK = BTEST(IH,IBIT)
      ENDIF
C
  999 RETURN
      END
