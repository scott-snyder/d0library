      FUNCTION RUN_TYPE(RUNNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the run number return a code indicating
C-   the type of run. If RUNNUM > 0 scan the appropriate 
C-                    TRIG_FILT_xxxxx.INFO.
C-                    If RUNNUM < 0 scan the file TRIG_FILT_RUN.
C-
C-   Returned value  : Code [I]   1 - GLOBAL      run (ALL, EXPRESS)
C-                                2 - SPECIAL     run 
C-                                3 - CALIBRATION run (CALIB)
C-   Inputs  : RUNNUM [I] Run number
C-
C-   Created  25-AUG-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUNNUM
      INTEGER RUN_TYPE
C----------------------------------------------------------------------
      INTEGER NSTREAM, RUNTYPE, STATUS, MAXSTREAM
      PARAMETER( MAXSTREAM = 100 )
      CHARACTER*16 STREAM(MAXSTREAM)
C----------------------------------------------------------------------
      CALL GET_RUN_STREAMS(RUNNUM,
     &                     MAXSTREAM,
     &                     NSTREAM,STREAM,RUNTYPE,STATUS)
      RUN_TYPE = RUNTYPE
  999 RETURN
      END
