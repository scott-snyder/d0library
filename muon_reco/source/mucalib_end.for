      SUBROUTINE MUCALIB_END(MUCALIB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : called at end of job
C-
C-   Inputs  : MUCALIB: defines what processes to run
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-SEP-1991   David Hedin: dummy for now
C-             7-JAN-1992   Eric James: added end routines for 
C-                                      t0,dt0 calibration      
C-    DH 1/30/92 spell ZERO not ZER0
C     Dh 2/92 add remaining hooks
C     DH 2/92 add output hook
C     DH 6/92 Add ALIGN hook
C-    MF 7/94 Change names: COR_TZERO->MTZERO_END, PADSUMRY->MPAD_END
C-    RM 12/94 Add call to scint. T0 tuning routine
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MUCALIB
C----------------------------------------------------------------------
C
CC    call subroutine to write out new t0 values if
CC    mucalib rcp value equals 0,1
CC    =============================================
C  
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.1) THEN
        CALL MTZERO_END
        CALL MTTOD_END
      ENDIF
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.2) THEN
        CALL MDT0_END
      ENDIF
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.3) THEN
        CALL MPAD_END
      ENDIF
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.4) THEN
        CALL MUALIGN_END
      ENDIF
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.5) THEN
        CALL MNT0_END
      ENDIF
      CALL MUCALIB_OUT
C
  999 RETURN
      END
