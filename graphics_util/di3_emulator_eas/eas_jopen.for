      SUBROUTINE JOPEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module opens a temporary segment on the E & S display device.
CD   It uses the current window and viewport settings to generate a
CD   viewing transformation to apply to the segment.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-DEC-1988   A. VIRGO
C-   UPDATED  10-feb-1989   S. ABACHI    KMODL introduced to do modeling
C-   UPDATED  18-AUG-1990   S. ABACHI    rendering node added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL ERRHND
      INTEGER I
      REAL TTRV(3)
      CHARACTER*3 STR
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:DEVSTS.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
C
      IF (SEGOPN) THEN
         CALL ERROR('JOPEN: A SEGMENT IS ALREADY OPEN')
      ENDIF
      CHJUST = DHJUST
      CVJUST = DVJUST
      CALL KBLDN(NTSEG, STR)
      SEGNAM = 'T' // STR
      INST = SEGNAM//'.S'
      SEGNUM = 0
      NTSEG = NTSEG + 1
C
C   Initialize internal vector storage.
C
      NVECTS = 1
      VSTAT(NVECTS) = .FALSE.
      CALL KQUEV(CPX, CPY, CPZ, 'MOVE')
C--------------------
CC      IF(.NOT. NUDI3) THEN
CC      CALL PINCL(SEGNAM, EMDISP, ERRHND)
CC      ELSE
C--------------------
      CALL PINCL(SEGNAM, TDISP, ERRHND)
CC      ENDIF
C--------------------
C
C   Create segment with viewing transformation applied.
C
      CALL PBEGS(SEGNAM, ERRHND)
      CALL KVWTR
      IF (MODEL) THEN
         CALL KMODL
      ENDIF
      CALL PINST('S','"',ERRHND)
      CALL PENDS(ERRHND)
C
      SEGOPN = .TRUE.
      SEGSOP = .TRUE.
      CALL KINIC
      RETURN
      END
