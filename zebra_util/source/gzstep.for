      INTEGER FUNCTION GZSTEP(NSTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the pointer to the STEP bank for step NSTEP
C-                          (STEP is a linear structure)
C-                          Return 0 if no such bank is found   
C-
C-   Inputs  : NSTEP number of the calibration step
C-   Outputs : GZSTEP address of bannk for NSTEP
C-   Controls: 
C-
C-   Created  10-OCT-1988   Jim Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INCLUDE 'D0$LINKS:IZSTEP.LINK'
C----------------------------------------------------------------------
      INTEGER NSTEP     ! number of the step in CALIB
      INTEGER LZFIND
C----------------------------------------------------------------------
C
      GZSTEP = 0
      LSTEP = LW(LWRKH - IZSTEP)
      IF (LSTEP.NE.0) THEN
C                              find the first bank with NSTEP in word 2
        GZSTEP = LZFIND(IDVWRK,LSTEP,NSTEP,2)
      ENDIF
      RETURN
      END
