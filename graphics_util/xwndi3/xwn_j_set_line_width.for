      SUBROUTINE J_SET_LINE_WIDTH(IWW,MODE)
      COMMON/LINCOM/LWIDTH,LSTYLE,LCAPS
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      INCLUDE 'SYS$LIBRARY:DECW$XLIBDEF.FOR'
      LWIDTH=IWW
      LCAPS=X$C_CAP_BUTT
      CALL X$SET_LINE_ATTRIBUTES(VD_ID,ATB(ILINE),
     &         LWIDTH,LSTYLE,LCAPS,JSTY)
      END
