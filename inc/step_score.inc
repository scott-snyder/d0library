c-----------------------------------------------------------------------------
c
c        This include file contains the table that keeps track of steps for
c        calib_taker.
c
c        17-March-1989   B. Gibbard
c
c-----------------------------------------------------------------------------

      INTEGER MAX_STEPS,MAX_NODES
      PARAMETER (MAX_STEPS = 100)
      PARAMETER (MAX_NODES = 50)

      INTEGER NEXT_STEP,NUM_STEPS,NUM_NODES,CLOCK_CHAN
      CHARACTER*6 NODE_NAMES(MAX_NODES)
      LOGICAL*1 PART_DONE(MAX_NODES,MAX_STEPS),STEP_DONE(MAX_STEPS)
      LOGICAL*1 ALL_CONNECT,PART_CONNECT(MAX_NODES)
      COMMON /STEP_SCORE/NEXT_STEP,NUM_STEPS,NUM_NODES,NODE_NAMES,
     1 STEP_DONE,PART_DONE,ALL_CONNECT,PART_CONNECT,CLOCK_CHAN
