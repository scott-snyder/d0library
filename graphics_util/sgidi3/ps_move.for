      SUBROUTINE PS_MOVE(X,Y,Z)
C-------------------------------------------------------------------
C-
C-  Do postscript move.  If no path in progress, MOVETO X1,Y1.
C-  If path in progress, STROKE or FILL and MOVETO X1,Y1.
C-
C-   Update 15-JUN-1991  Lupe Howell Initialize IPP out of data stm.
C-
C-------------------------------------------------------------------
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C-------------------------------------------------------------------
      LOGICAL INPROG,FIRST
      DATA INPROG/.FALSE./
      DATA FIRST /.TRUE./
      SAVE
C-------------------------------------------------------------------
C
C *** Initialize common block element
C
      IF ( FIRST ) THEN
        IPP = 1
        FIRST = .FALSE.
      ENDIF
C
      XPOSN=X
      YPOSN=Y
      ZPOSN=Z
      CALL DEV_TRANSF(X,Y,Z,X1,Y1)
      IX1=X1
      IY1=Y1
C
C  Clip for protection
    8 IF(IX1.LT.0) IX1=0
      IF(IX1.GT.3000) IX1=3000
      IF(IY1.LT.0) IY1=0
      IF(IY1.GT.3000) IY1=3000
C
      IF(INPROG) THEN                     
        IF(IPREVI.NE.1) THEN
          IF(IFILL.EQ.0) THEN
            WRITE(D2LINE(IPP:IPP+14),13) IX1,IY1
   13       FORMAT(' s',2I5,' m')
          ELSE
            WRITE(D2LINE(IPP:IPP+14),17) IX1,IY1
   17       FORMAT(' f',2I5,' m')
          ENDIF
        ELSE
C  Previous operation was a move.
          WRITE(D2LINE(IPP:IPP+14),16) IX1,IY1
        ENDIF
        IPP=IPP+14
      ELSE
        INPROG=.TRUE.
        WRITE(D2LINE(IPP:IPP+14),16) IX1,IY1
   16   FORMAT('  ',2I5,' m')
        IPP=IPP+14
      ENDIF
      IPREVI=1                            
      GO TO 50
C-------------------------------------------
      ENTRY PS_MOVSCR(XXX,YYY)
C  Do postscript move in screen (paper) coordinates
      IX1=XXX
      IY1=YYY
      GO TO 8
C-------------------------------------------
      ENTRY PS_DRAW(XX,YY,ZZ)
C  Do postscript draw.  LINETO X2,Y2.
C
      XPOSN=XX
      YPOSN=YY
      ZPOSN=ZZ
      CALL DEV_TRANSF(XX,YY,ZZ,X2,Y2)
      IX2=X2
      IY2=Y2
C
C  Clip for protection
   18 IF(IX2.LT.0) IX2=0
      IF(IX2.GT.3000) IX2=3000
      IF(IY2.LT.0) IY2=0
      IF(IY2.GT.3000) IY2=3000
C
      IF(INPROG) THEN                      
        WRITE(D2LINE(IPP:IPP+14),12) IX2,IY2
   12   FORMAT('  ',2I5,' l')
        IPP=IPP+14
        XOLD=X2
        YOLD=Y2
        IPREVI=2
      ENDIF
      GO TO 50
C-------------------------------------------
      ENTRY PS_DRASCR(XXXX,YYYY)
C  Do postscript draw in screen (paper) coordinates
      IX2=XXXX
      IY2=YYYY
      GO TO 18
C-------------------------------------------
      ENTRY PS_FORCE
C  Force output of last of line buffer.
   30 IF(INPROG) THEN
        IF(IFILL.EQ.1) THEN
          WRITE(D2LINE(IPP:IPP+14),15)
   15     FORMAT(' f            ')             
        ELSE
          WRITE(D2LINE(IPP:IPP+14),55)
   55     FORMAT(' s            ')             
        ENDIF
        IPP=IPP+14
        INPROG=.FALSE.
        IPREVI=3
      ENDIF
      GO TO 100
C
C  Ending operations
   50 IF(IPP.LT.71) RETURN               
  100 IF(IPP.EQ.1)  RETURN               
      WRITE(IDRUNI,110) D2LINE(1:IPP-1)
  110 FORMAT(A)
      IPP=1
C
      RETURN
      END
