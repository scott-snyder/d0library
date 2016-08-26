      SUBROUTINE J_RZTEXT(VD_ID,ATB,STRING,XX,YY)
C  ROUTINE TO PUT "STROKE" FONTS STRINGS INTO THE WORLD COORDINATE SYSTEM
C  AT POSITION X,Y  (2D ONLY)
      INTEGER*4 VD_ID,ATB
      CHARACTER*(*) STRING
      COMMON/TSLOPE/AN,CS,SN
      COMMON/FONTSZ/XSIZE,YSIZE
      COMMON/VFONTS/ICHAR(16,0:133)
      CHARACTER*1 CHAR(0:127)
      CHARACTER*1 CHR
      LOGICAL*1 BCHR
      DATA XSIZE,YSIZE/.03,.03/
      DATA AN,CS,SN/0.,1.,0./
      EQUIVALENCE(CHR,BCHR)
C
      LENSTR=LEN(STRING)
      X0=0.
      DO 700 ICC=1,LENSTR
        CHR=STRING(ICC:ICC)
        IASCII=BCHR
        IASCII=iand(IASCII,255)                   !8 BITS
        IF(IASCII.GT.133)IASCII=32              !UNPRINTABLE
        IF(IASCII.LT.32)IASCII=32               !UNPRINTABLE
        DO 100 I=1,16
          IC=ICHAR(I,IASCII)
          IF(IC.GE.128)GOTO 200                 !TERMINATOR
          X=X0+(1./6.)*(1+(iand((IC/8),7)))
          X=X*XSIZE                             !X SIZE
          Y=(1./9.)*(1+iand(IC,7)) 
          Y=Y*YSIZE                             !Y SIZE
          XPOS=XX+X*CS-Y*SN
          YPOS=YY+Y*CS+X*SN
          IF(iand(IC,64).EQ.0)THEN
            CALL J_LINE2(VD_ID,ATB,XOLD,YOLD,XPOS,YPOS)
          ENDIF
          XOLD=XPOS
          YOLD=YPOS
  100   CONTINUE
  200   CONTINUE
C  X POSN FOR NEXT CHARACTER
        X0=X0+1.
  700 CONTINUE
      END
