      SUBROUTINE ISDROP
C-----------------------------------------------------------------------
C-    This routine drops JVERTX and JKINE banks, if they exist.
C-
C-    S.Kunori,   Mar., 1986
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC/LIST'
C
C  for geant310...
      IF(JVERTX.NE.0) CALL MZDROP(IXSTOR,JVERTX,' ')
      IF(JKINE.NE.0)  CALL MZDROP(IXSTOR,JKINE,' ')
C  For Geant 3.08...
C      IF(JVERTX.NE.0) CALL GZDROP(JVERTX)
C      IF(JKINE.NE.0)  CALL GZDROP(JKINE)
C
      RETURN
      END
