      SUBROUTINE L2_PRE_GET_ONLINE(FOUND,ONLINE_PASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods: try to recover whether L2 prescale passed online
C-
C-  This code does MANY illegal things.  DO NOT COPY THIS CODE FOR OTHER USES!!!
C-  
C-    The purpose is to allow exact simulation of online processing, mainly for
C-    making verification runs possible when the verify processor does not see
C-    the same sample of events that the online nodes saw.
C-    
C-    The method is to look at the last tool which ran for this bit online
C-    IF:
C-      - L2_PRESCALE always goes first in a script
C-      - and the script really is the same in the simulation as it was online
C-    THEN the method works, and if the last tool was anything other than the
C-    present tool number (which we assume is l2prescale, and assume has not
C-    changed), then prescale must have passed.
C-
C-    However, if l2_prescale was the ONLY tool to run in the script, this logic
C-    is insufficient, and the answer must be reconstructed from the FILT PASS
C-    bits if they are still unavailable.
C-  
C-   Inputs  : NONE, though it peeks at things better left unseen
C-   Outputs : FOUND  .TRUE. if there is still a FRES result bank for this bit
C-                from online running and we could reconstruct the result
C-             ONLINE .TRUE. if FOUND is TRUE and if L2_PRESCALE passed online
C-             
C-   Controls: NONE
C-
C-   Created  23-JAN-1994   James T. Linnemann
C-   Updated  16-APR-1994   James T. Linnemann  do script with just L2_PRESCALE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FOUND,ONLINE_PASS
      INTEGER I,J,K,T
      INCLUDE 'D0$INC:L2_PRE_ONL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$LINKS:IZFRES.LINK'
      INTEGER LFILT,GZFILT,LOLD,LFROLD,LAST_TOOL_RUN
      BYTE LAST_USED(2,0:127) ! last tool and parameter set to run on each bit
C----------------------------------------------------------------------
      FOUND = .FALSE.
      ONLINE_PASS = .FALSE.
      LFILT = GZFILT()
      IF ( LFILT.GT.0 ) THEN
        LOLD = LQ(LFILT)  ! search for previous FILT bank from online run
        IF ( LOLD.GT.0 ) THEN
          LFROLD = LQ(LOLD-IZFRES)
          IF ( LFROLD.GT.0 ) THEN
C...look for tool number which ran last on this bit
            CALL UCOPY(IQ(LFROLD+2),LAST_USED,64)
            LAST_TOOL_RUN = LAST_USED(1,K)
            FOUND = .TRUE.
            IF ( LAST_TOOL_RUN.NE.0 ) THEN  !script was really run
              IF ( LAST_TOOL_RUN.NE.T ) THEN  ! last was NOT prescale
                ONLINE_PASS = .TRUE.  !must have gotten past it
              ELSE  ! prescale ran last => failed unless it was only tool
                ONLINE_PASS = .FALSE.
                IF ((FILTER_SCRIPT(1,1,K).EQ.T).AND.
     &            (FILTER_SCRIPT(1,2,K).EQ.0)) THEN  !prescale was only tool
                    ONLINE_PASS = BTEST(IQ(LOLD+9+I),J) !copy old PASS bit
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
