      SUBROUTINE KINIC
C
C   Initialize current primitives with the default set.
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
C
C   Initialize current pick id to the Default
C
      CPIKID = DPIKID
C
C   Initialize drawing primitive attributes.
C
      CURCOL = DEFCOL
      CINTEN = DINTEN
      CLSTYL = DLSTYL
      CMARKR = DMARKR
C
C   Initialize text primitive attributes.
C
      CPATH  = DPATH
      CHJUST = DHJUST
      CVJUST = DVJUST
      CXSIZE = DXSIZE
      CYSIZE = DYSIZE
      CGAP   = DGAP
      CXBASE = DXBASE
      CYBASE = DYBASE
      CZBASE = DZBASE
      CXPLAN = DXPLAN
      CYPLAN = DYPLAN
      CZPLAN = DZPLAN
      RETURN
      END
