      SUBROUTINE PS_TEXT(STRING)
C  Write a postscript text string
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*(*) STRING
      SAVE
C---------------------------------
      CALL PS_FORCE
      IF(YSIZE.NE.YSZOLD) THEN
        CALL PS_FONT(IFONTC)
        YSZOLD=YSIZE
      ENDIF
      ILEN=LEN(STRING)
      X0=-.5*(IHJUST-1)*ILEN*XSIZE             !HORIZ JUSTIF
      Y0=-.5*(IVJUST-1)*YSIZE                  !VERT JUSTIF
      CALL DEV_TRANSF(XPOSN+X0,YPOSN+Y0,ZPOSN,X2,Y2)
      IX2=X2
      IF (ZPOSN .NE. 0.) IX2=X2-(10.*XSCAL)
      IY2=Y2
      IF(IX2.LT.0) RETURN
      IF(IX2.GT.3000) RETURN
      IF(IY2.LT.0) RETURN
      IF(IY2.GT.3000) RETURN
C-
      WRITE(IDRUNI,10) IX2,IY2,STRING(1:LEN(STRING))
  10  FORMAT(' ',2I5,' m (',A,') sh')
      END
