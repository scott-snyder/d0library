      SUBROUTINE JLWIDE(LWIDE)
      INCLUDE 'D0$INC:DI3INC.INC'
      INTEGER*4 MODE
      ILWID=LWIDE
      WIDDLE=FLOAT(LWIDE-16383)*20./16383.
      IF(WIDDLE.LT.1.) WIDDLE=1.
C  WIDDLE IS THE WIDTH IN PIXELS
      CALL J_SET_LINE_WIDTH(VD_ID,ATB,ATB,WIDDLE,MODE)
C  DEFAULT=16383
      IF(PUTS) CALL J_PUTSG(-IJWID,ILWID)
      END