      SUBROUTINE J3STRG(STRING)
C------------------------------------------------------------------
C-
C-   Purpose and Methods: Output a stroke precision text string
C-   as an output primitive.
C-
C-   Input:  STRING[C*]: String to be displayed
C-
C-   Output: None
C-
C-   Author  Unknown      Mike Shupe
C-   Updated 28-AUG-1991  Lupe Howell include statement fixed     
C-
C------------------------------------------------------------------
      CHARACTER*(*) STRING
      CHARACTER*1 CHAR(0:127)
      CHARACTER*1 CHR
      LOGICAL*1 BCHR
      EQUIVALENCE(CHR,BCHR)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      INCLUDE 'D0$INC:VAXFONT.INC'
C------------------------------------------------------------------
C
C *** ADJUST FOR DEVICE SCALING
C
      LENSTR=LEN(STRING)
      XSAVE=VPOSN(1)                   
      YSAVE=VPOSN(2)
      ZSAVE=VPOSN(3)
C      type *,' J3STRG - VPOSN:',vposn
      X0=-.5*(IHJUST-1)*LENSTR      
      Y0=-.5*(IVJUST-1)             
      DO 700 ICC=1,LENSTR
        CHR=STRING(ICC:ICC)
        IASCII=BCHR
        IASCII=IASCII.AND.255       
        IF(IASCII.GT.133)IASCII=32  
        IF(IASCII.LT.32)IASCII=32   
        DO 100 I=1,16
          IC=ICHAR(I,IASCII)
          IF(IC.GE.128)GOTO 200           
          X=X0+(1./6.)*(1+((IC/8).AND.7))
          X=X*XSIZE*.95                   
          Y=Y0+(1./9.)*(1+(IC.AND.7))
          Y=Y*YSIZE                       
          XPOS=XSAVE+X*XBASEN+Y*XPLANN
          YPOS=YSAVE+X*YBASEN+Y*YPLANN
          ZPOS=ZSAVE+X*ZBASEN+Y*ZPLANN
          IF((IC.AND.64).NE.0)THEN
            CALL J3MOVE(XPOS,YPOS,ZPOS)
          ELSE
            CALL J3DRAW(XPOS,YPOS,ZPOS)
          ENDIF
  100   CONTINUE
  200   CONTINUE
C
C *** X POSN FOR NEXT CHARACTER
C
        X0=X0+1.
  700 CONTINUE
      VPOSN(2)=YSAVE
      VPOSN(1)=VPOSN(1)+.190*XSIZE
      END
