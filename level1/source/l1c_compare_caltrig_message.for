      SUBROUTINE L1C_COMPARE_CALTRIG_MESSAGE(LUN, MSGNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out a particular message dealing with
C-     comparing the Fast and normal simulations.
C-
C-   Inputs  : LUN      The unit number to write the message to
C-             MSGNUM   The message to print. Possible:
C-                        LV1_DUMP_DATABLOCK
C-                        LV1_DUMP_FASTSIMUL
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
C
      INTEGER MSGNUM, LUN
C
      IF (MSGNUM .EQ. LV1_DUMP_DATABLOCK) THEN
        WRITE (LUN,*)
        WRITE (LUN,*)
        WRITE (LUN,*) 'FAST SIMULATION VERIFICATION FAILED'
        WRITE (LUN,*) '-----------------------------------'
        WRITE (LUN,*) 
        WRITE (LUN,*) 'The results of the Fast Level 1 simulation did'
        WRITE (LUN,*) 'not match the results of the normal simulation'
        WRITE (LUN,*)
        WRITE (LUN,*) 'RESULTS OF NORMAL SIMULATION'
        WRITE (LUN,*)
        GOTO 999
      ENDIF
C
      IF (MSGNUM .EQ. LV1_DUMP_FASTSIMUL) THEN
        WRITE (LUN,*)
        WRITE (LUN,*)
        WRITE (LUN,*) 'RESULTS OF FAST SIMULATION'
        WRITE (LUN,*)
        GOTO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
