C----------------------------------------------------------------------
C-
C-   Created   8-AUG-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER NEV_MAX
      PARAMETER( NEV_MAX = 105 )
      COMMON /EVENT_HEAD_LINKS/LEVENT_HEAD(NEV_MAX),LRUN_HEAD(2)
      INTEGER LEVENT_HEAD,LRUN_HEAD
