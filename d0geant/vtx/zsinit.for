      SUBROUTINE ZSINIT
C-----------------------------------------------------------------------
C-  Subroutine ZSINIT is called to initialize NZHITS before beginning to 
C-  store the hits for a VTX layer.
C-
C-  T. Trippe, 7 Jan. 1987
C-  Modified 16-NOV-1989 Peter Grudberg - add common VZDATA 
C-----------------------------------------------------------------------
      INCLUDE 'D0$INC:VZHITS.INC/LIST'
      INCLUDE 'D0$INC:VZDATA.INC/LIST'
C
C **** Reset ZHITSV:
C
      NZHITS = 0
      NZHTMX = MZHTMX
C
C ****  Reset ZDATAV:
C
      NZDATA = 0
      NZDTMX = MZDTMX
C
  999 RETURN
      END
