C----------------------------------------------------------------------
C-
C-   Created  26-NOV-1991   Lee Lueking
C-
C----------------------------------------------------------------------
C Common block for monitoring jobs
C
      INTEGER QID(MAX_MON),CURRENT_EVENT(MAX_MON),LAST_EVENT(MAX_MON)
      COMMON /PM_MONITOR/ QID,CURRENT_EVENT,LAST_EVENT
