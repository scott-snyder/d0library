      SUBROUTINE J_STRING(STRING)
C-  Draw character strings with various stroke fonts.
C-  The fonts selectable by STFONT are:
C-      0 - Stroke, 2 - Roman, 4 - Greek, 6 - Script, 8 - Old English,
C-      10 - Math and symbolic
C-  The odd numbered fonts are italicized.
C-----------------------------------------------------------------
      CHARACTER*(*) STRING
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
C
C  STROKE fonts
      CHARACTER*1 CHR,CHAR(0:127)
      LOGICAL*1 BCHR
      EQUIVALENCE(CHR,BCHR)
      INCLUDE 'D0$INC:VAXFONT.INC'
C
C  Fancy fonts
      COMMON/FNTCOM/NSTROKE,LADJ,RADJ,POINTER,FONT
      INTEGER*2 NSTROKE(512),LADJ(512),RADJ(512)
      INTEGER*4 POINTER(512)
      BYTE FONT(17152)
C
C  XWN_FANCYFONT.INC contains previous contents of FONTS.BIN
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWN_FANCYFONT.INC'
C
      DATA PI/3.1415926536/
      DATA SLANT/0.2/,SUPFRAC/0.6/
      DATA FAC/0.053/
      LOGICAL RELO,FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------
C
      ILSAVE=ILSTYL
      CALL JLSTYL(0)
      LENSTR=LEN(STRING)
      SUPFACTOR=1.
      SUPSHIFT=0.
      XSAVE=XPOSN                   !GET COPY OF ORIG POSITION
      YSAVE=YPOSN
      ZSAVE=ZPOSN
      X0=-.5*(IHJUST-1)*LENSTR      !HORIZ JUSTIF (CHAR UNITS)
      Y0=-.5*(IVJUST-1)             !VERT JUSTIF
C
      IF(IFONTY.GT.1) THEN
        X0=X0*.95*XSIZE
        Y0=(Y0+.5)*YSIZE
      ENDIF
C
      DO 300 ICC=1,LENSTR
        CHR=STRING(ICC:ICC)
        IASCII=BCHR
        IASCII=IASCII.AND.255                !8 BITS
C           ELSE IF(ASC(J).EQ.'U'.OR.ASC(J).EQ.'u') THEN
C             ISUPER=ISUPER+1
C             SUPSHIFT=SUPSHIFT+16.*SUPFACTOR
C             SUPFACTOR=SUPFACTOR**IABS(ISUPER)
C           ELSE IF(ASC(J).EQ.'D'.OR.ASC(J).EQ.'d') THEN
C             ISUPER=ISUPER-1
C             SUPFACTOR=SUPFACTOR**IABS(ISUPER)
C             SUPSHIFT=SUPSHIFT-16.*SUPFACTOR
        IF(IASCII.GT.133)IASCII=32           !UNPRINTABLE
        IF(IASCII.LT.32)IASCII=32            !UNPRINTABLE
        IF(IFONTY.LE.1) THEN
          NSTROK=16
          IY=1
        ELSE
          JCHAR=IASCII+1+IOFFSET
          NSTROK=NSTROKE(JCHAR)
          RELO=.TRUE.
        ENDIF
C  Do the character strokes
        DO 100 I=1,NSTROK
          IF(IFONTY.LE.1) THEN
            IC=ICHAR(I,IASCII)
            IF(IC.GE.128)GOTO 200              !TERMINATOR
            X=X0+(1+((IC/8).AND.7))/6.
            Y=Y0+(1+(IC.AND.7))/9.
            X=X*XSIZE*.85
            Y=Y*YSIZE*1.05
            IF(MOD(IFONTY,2).EQ.1) X=X+SLANT*Y
          ELSE
            J=I-1
            IX=FONT(POINTER(JCHAR)+2*J)
            IF(IX.EQ.31) THEN
              RELO=.TRUE.
              GO TO 100
            ENDIF
            IY=FONT(POINTER(JCHAR)+2*J+1)
            X=IX-LADJ(JCHAR)
            IF(MOD(IFONTY,2).EQ.1) X=X+SLANT*(IY+9)
            X=X0+X*XSIZE*.95*SUPFACTOR/25.
            Y=Y0+YSIZE*(IY*SUPFACTOR+SUPSHIFT)/25.
          ENDIF
          XPOS=XSAVE+X*XBASEN+Y*XPLANN
          YPOS=YSAVE+X*YBASEN+Y*YPLANN
          ZPOS=ZSAVE+X*ZBASEN+Y*ZPLANN
          IF(IFONTY.LE.1) THEN
            IF((IC.AND.64).EQ.0)THEN
              CALL J3MOVE(XP,YP,ZP)
              CALL J3DRAW(XPOS,YPOS,ZPOS)
            ENDIF
          ELSE
            IF(RELO) THEN
              RELO=.FALSE.
            ELSE
              CALL J3MOVE(XP,YP,ZP)
              CALL J3DRAW(XPOS,YPOS,ZPOS)
            ENDIF
          ENDIF
          XP=XPOS
          YP=YPOS
          ZP=ZPOS
  100   CONTINUE
c
  200   CONTINUE
C  X POSN FOR NEXT CHARACTER
        IF(IFONTY.LE.1) THEN
          X0=X0+1.
        ELSE
          XA=XSIZE*(RADJ(JCHAR)-LADJ(JCHAR))*SUPFACTOR/25.
          X0=X0+XA
          SLENGTH=SLENGTH+XA
        ENDIF
  300 CONTINUE
      YPOSN=YSAVE
      XPOSN=XPOSN+.190*XSIZE
      CALL JLSTYL(ILSAVE)
      RETURN
      END
