      SUBROUTINE DIVDIFL(FF,XX,SIZE,X,ANS,SLOPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Linear interpolation -- assumes SIZE points XX are
C-               monotonic increasing.  Adapted from KERNLIB routine DIVDIF.
C-               Also returns derivitive.
C-
C-   Inputs  :  FF = dependant table 
C-              XX = independant table
C-              SIZE = size of table
C-              X = desired point to evaluate
C-   Outputs :  ANS     = linear interpolation from table
C-              SLOPE   = slope, dF/dX, of linear segment
C-   Controls: 
C-
C-   Created   2-JUN-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c I/O
      REAL FF(*),XX(*),X,SLOPE,ANS
      INTEGER SIZE
c Locals:
      INTEGER IX,IY,MID
C----------------------------------------------------------------------
      IX = 1
      IY = SIZE
      DO WHILE (IY-IX .GT. 1)
        MID = (IX+IY)/2
        IF (X .LT. XX(MID)) THEN
          IY = MID
        ELSE
          IX = MID
        ENDIF
      ENDDO
      SLOPE = (FF(IY)-FF(IX))/(XX(IY)-XX(IX))
      ANS = FF(IX) + SLOPE*(X-XX(IX))
  999 RETURN
      END
