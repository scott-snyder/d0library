      SUBROUTINE VTPLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create permanent link area for VTX hit banks
C-
C-
C-   Created  16-OCT-1991   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZLINK(IXCOM,'/VTXLNK/',VTXLNK,LCDD1,VTXLNK)
      ENDIF
  999 RETURN
      END
