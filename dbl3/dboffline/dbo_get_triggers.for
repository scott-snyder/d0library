      SUBROUTINE DBO_GET_TRIGGERS
     &  (RUN,NTRIG,TRIGBIT,TRIGGER,NFILT,FILTBIT,FILTER,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return triggers and filters associated with
C-   the specified run from the DBL3 Offline Database.
C-
C-   Inputs  : RUN        [I]   Run Number
C-   Outputs : NTRIG      [I]   Number of Triggers
C-             TRIGBIT(*) [I]   Trigger bits
C-             TRIGGER(*) [C*]  Trigger names
C-             NFILT      [I]   Number of Filters
C-             FILTBIT(*) [I]   Filter bits
C-             FILTER(*)  [C*]  Filter names
C-   Controls: None
C-
C-   Created  19-MAR-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN
      INTEGER NTRIG,TRIGBIT(*)
      CHARACTER*(*) TRIGGER(*)
      INTEGER NFILT,FILTBIT(*)
      CHARACTER*(*) FILTER(*)
      INTEGER STATUS
C----------------------------------------------------------------------
      STATUS = 0
      NFILT  = 0
      NTRIG  = 0
  999 RETURN
      END
