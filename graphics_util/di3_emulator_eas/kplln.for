      SUBROUTINE KPLLN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C    this module displays a Nvertice polygon defined by VERTIC with the
C    current attributes.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-NOV-1988   S. ABACHI, A. VIRGO
C-   UPDATED  10-AUG-1989   S. ABACHI   Replaced PVCONN by PVITEM in PVCBEG
C-                                      to gain ability to move in addition
C-                                      to DRAW.
C-   UPDATED  19-sep-1989   S. ABACHI   Block normalized the data to gain
C-                                      speed and memory.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL ERRHND
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
      INCLUDE 'D0$INC:PROCNS.INC/LIST'
C
      INCLUDE 'D0$INC:GDINEW.INC/LIST'
c
      CHARACTER*4 PRIMI
      LOGICAL*1 BLKNRM, COLBLN
      INTEGER DIMENS, I
      REAL  VINTEN
C
      IF (NVERT .LE. 1) RETURN
C
      CALL KPRIM(PRIMI)
      BLKNRM = .TRUE.
      COLBLN = .FALSE.
      IF (THREED) THEN
        DIMENS = 3
      ELSE
        DIMENS = 2
      ENDIF
      IF(.NOT. NUGDI) THEN
        DO 10 I=2,NVERT
          POSLIN(I) = .TRUE.
   10   CONTINUE
      ENDIF
      POSLIN(1) = .FALSE.
      VINTEN = 1.0
CC      IF (MOD(CPEDGE, 2) .EQ. 1) VINTEN = 0.0
      DO 20 I=1,NVERT
        VERTIC(4,I) = VINTEN
   20 CONTINUE
      CALL PBEGS(PRIMI, ERRHND)
      CALL PSECOL('"', HUECOL(CURCOL+1, 1), SATCOL(CURCOL+1, 1),
     +               '"', ERRHND)
      CALL PVCBEG('D"', NVERT, BLKNRM, COLBLN, DIMENS, PVITEM, ERRHND)
      CALL PVCLIS(NVERT, VERTIC, POSLIN,ERRHND)
      CALL PVCEND(ERRHND)
      CALL PENDS(ERRHND)
      CALL PINCL(PRIMI, INST, ERRHND)
      RETURN
      END
