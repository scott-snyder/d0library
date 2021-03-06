      SUBROUTINE QM_MOVE(X,Y,Z)
C  Do pen-up vector.
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      DATA IPP/1/
      LOGICAL INPROG
      DATA INPROG/.FALSE./
      SAVE
C
      XPOSN=X
      YPOSN=Y
      ZPOSN=Z
      CALL DEV_TRANSF(X,Y,Z,X1,Y1)
      IX1=X1
      IY1=Y1
    8 WRITE(D2LINE(IPP:IPP+14),13) IY1,IX1
   13 FORMAT(' ^U',I5,':',I5)
      IPP=IPP+14
      GO TO 50
C-------------------------------------------
      ENTRY QM_MOVSCR(XXX,YYY)
C  Do QMS move in screen (paper) coordinates
      IX1=XXX
      IY1=YYY
      GO TO 8
C-------------------------------------------
      ENTRY QM_DRAW(XX,YY,ZZ)
C  Do QMS draw.
      XPOSN=XX
      YPOSN=YY
      ZPOSN=ZZ
      CALL DEV_TRANSF(XX,YY,ZZ,X2,Y2)
      IX2=X2
      IY2=Y2
   18 WRITE(D2LINE(IPP:IPP+14),12) IX2,IY2
   12 FORMAT(' ^D',I5,':',I5)
      IPP=IPP+14
      XOLD=X2
      YOLD=Y2
      GO TO 50
C-------------------------------------------
      ENTRY QM_DRASCR(XXXX,YYYY)
C  Do QMS draw in screen (paper) coordinates
      IX1=XXXX
      IY1=YYYY
      GO TO 18
C-------------------------------------------
      ENTRY QM_FORCE
C  Force output of last of line buffer.
      GO TO 100
C
C  Ending operations
   50 IF(IPP.LT.71) RETURN               ! MAX LINE LENGTH
  100 IF(IPP.EQ.1)  RETURN               ! NOTHING TO WRITE
      WRITE(IDRUNI,110) D2LINE(1:IPP-1)
  110 FORMAT(A)
      IPP=1
C
      RETURN
      END
