      SUBROUTINE PS_COLOR(IC)
C  Change the color index
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      SAVE
      CALL PS_FORCE
      IF(IC.EQ.ICCURR) RETURN
      ICCURR=IC
      INDX=IC+1
      IF(INDX.LT.1.OR.INDX.GT.16) THEN
        TYPE *,' PS_COLOR - INDEX OUT OF RANGE:',IC
        R=1.
        G=1.
        B=1.
      ELSE
        R=RVEC(INDX)/255.
        IF(R.LT.0..OR.R.GT.1.) R=1.
        G=GVEC(INDX)/255.
        IF(G.LT.0..OR.G.GT.1.) G=1.
        B=BVEC(INDX)/255.
        IF(B.LT.0..OR.B.GT.1.) B=1.
      ENDIF
      IF(IC.EQ.0.OR.IC.EQ.7) THEN     ! NORMAL AND WHITE --> BLACK
	R=0.
	G=0.
	B=0.
      ELSEIF(IC.EQ.9) THEN            ! COMPLEMENT --> WHITE
        R=1.
        G=1.
        B=1.
      ENDIF
      WRITE(IDRUNI,10) R,G,B
   10 FORMAT(3(' ',F5.3),' rgb')
      END
