      FUNCTION HPLOT_SAVE ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store histograms automatically in Examine
C-                         upon EXM_END_ANALYSIS
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JUL-1990   Chip Stewart, Boaz Klima 
C-   Updated   4-SEP-1992   Susan K. Blessing  Only store histograms 
C-    when some events have been processed.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:EXM_STATISTICS.INC/LIST'
C
      INTEGER RUN,PREV_RUN,RUNNO
      INTEGER PREV_NEVS
C
      LOGICAL HPLOT_SAVE
      LOGICAL HPLOT_ZERO
C
      DATA RUN/0/
      DATA PREV_RUN/-1/
C----------------------------------------------------------------------
      HPLOT_SAVE = .TRUE.
C
      RUN = RUNNO()
      IF (RUN.NE.PREV_RUN.AND.RUN.NE.0) THEN
C
        IF (NEVS.GT.0) THEN
          CALL D0HSTR(0)                  ! store all away
        ELSE
          CALL INTMSG('  The histograms are not being stored.')
          CALL INTMSG('  No events were processed.')
C
        END IF
C
        PREV_RUN = RUN
        PREV_NEVS = NEVS
C
      ELSE IF (RUN.EQ.PREV_RUN.AND.RUN.NE.0) THEN
C
        IF (NEVS.GT.PREV_NEVS) THEN
          CALL D0HSTR(0)
          PREV_NEVS = NEVS
        ELSE
          CALL INTMSG('  The histograms are not being stored.')
          CALL INTMSG(
     &      '  A file with more events already exists for this run.')
        END IF
C
      ELSE
        CALL INTMSG('  The histograms are not being stored.')
        CALL INTMSG('  The run number is 0.')
C
      END IF
C
  999 RETURN      
C
      ENTRY HPLOT_ZERO ()
C
      HPLOT_ZERO = .TRUE.      
      CALL D0HCLR(0)                  ! CLEAR THEM ALL
C
 1999 RETURN      
C
      END
