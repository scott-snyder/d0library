      SUBROUTINE KQUEV(XPT,YPT,ZPT,MODE)
      IMPLICIT NONE
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      REAL XPT,YPT,ZPT
      CHARACTER*4 MODE
C
C    Optimize the vector structure by eliminating two consecutive moves.
C
      IF ( VSTAT(NVECTS) .OR. MODE .EQ. 'DRAW' ) THEN
          NVECTS = NVECTS + 1
      ENDIF
C
C    Add the vector to the list.
C
      VECTS(1,NVECTS) = XPT
      VECTS(2,NVECTS) = YPT
      VECTS(3,NVECTS) = ZPT
      VECTS(4,NVECTS) = MAX(FLOAT(CINTEN / 32767), MINTEN)
C
C    Determine and store the mode of this queue action.
C
      VSTAT(NVECTS) = (MODE .EQ. 'DRAW')
      IF (NVECTS .EQ. MVECTS) CALL KUPDV
      RETURN
      END
