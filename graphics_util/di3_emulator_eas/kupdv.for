      SUBROUTINE KUPDV
C
C    Subroutine update the screen with the current vector list.
C
      IMPLICIT NONE
      EXTERNAL ERRHND
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PROCNS.INC/LIST'
      CHARACTER*4 PRIMI
      LOGICAL*1 BLKNRM, COLBLN
      INTEGER DIMENS
      CHARACTER*4 PATNAM
C
C    Return if there is nothing to draw.
C
      IF (NVECTS .LE. 1) RETURN
C
C    Generate a primitive name.
C
      CALL KPRIM(PRIMI)
C
C    Initialize block normalization, and color blending to false.
C
      BLKNRM = .FALSE.
      COLBLN = .FALSE.
C
C    Set the dimensionality to 3-D
C
      IF (THREED) THEN
         DIMENS = 3
      ELSE
         DIMENS = 2
      ENDIF
      CALL PBEGS(PRIMI,ERRHND)
      CALL PSECOL('"', HUECOL(CURCOL+1,1), SATCOL(CURCOL+1,1), 
     +            '"', ERRHND)
      CALL PVCBEG('D', NVECTS, BLKNRM, COLBLN, DIMENS, PVITEM, ERRHND)
      CALL PVCLIS(NVECTS, VECTS, VSTAT, ERRHND)
      CALL PVCEND(ERRHND)
      CALL PENDS(ERRHND)
      IF (MOD(CLSTYL,10) .NE. 0) THEN
         CALL KDFPT(MOD(CLSTYL,10), PATNAM)
         CALL PPATWI(PRIMI//'.D"', PATNAM, ERRHND)
      ENDIF
      CALL PINCL(PRIMI//'"', INST, ERRHND)
C
C    Requeue the last vector sent as the first of a new structure.
C
      VSTAT(1) = .FALSE.
      VECTS(1,1) = VECTS(1,NVECTS)
      VECTS(2,1) = VECTS(2,NVECTS)
      VECTS(3,1) = VECTS(3,NVECTS)
      VECTS(4,1) = VECTS(4,NVECTS)
C
C    Reset vector counter
C
      NVECTS = 1
      RETURN
      END
