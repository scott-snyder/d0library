      SUBROUTINE USER_STREAM_FILTER(STREAM,LPASS,USER_ONLY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to which user can add substitute/
C-                         additional rejection criteria to the standard
C-                         stripping on trigger bits.  *** Note: only the
C-                         header has been read in at this point, so 
C-                         rejection can only involve header words. ****
C-
C-   Inputs  : STREAM     C*3      Name of stream being filtered
C-   Outputs : LPASS      .TRUE.   if the event should be kept
C-             USER_ONLY  .TRUE.   if LPASS alone can keep the event
C-                                 (otherwise it is kept if (LPASS
C-                                 .AND. STREAM_FILTER) = .TRUE.; STREAM_FILTER
C-                                 is calculated in function STREAM_FILTER.
C-
C-   Created  28-MAY-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL LPASS,USER_ONLY
C
      CHARACTER*3 STREAM
C----------------------------------------------------------------------
      LPASS = .TRUE.
      USER_ONLY = .FALSE.
  999 RETURN
      END
