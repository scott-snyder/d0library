      SUBROUTINE TOP_LEPTONS_MORE_NTUPLE(MORE_QUANS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : User routine to add in their own entries to 
C-                         the TOP LEPTONS package ntuple.
C-
C-   Inputs  : none
C-   Outputs : MORE_QUANS [F] : values to be added to ntuple
C-   Controls: none
C-
C-   To add in more entries, increase the array sizes to the number of
C-   entries you plan to add.   Enter the tag names into the EXTRA_TAGS
C-   DATA statement.   Set the NUM_MORE DATA value to the number of 
C-   added entries.   Replace the code that fills MORE_QUANS so that it
C-   fills your values into the MORE_QUANS array.  
C-
C-   Created  25-MAY-1993   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, NUM_MORE, NUM_MORE_ENTRIES
      REAL MORE_QUANS(*)
      CHARACTER*8 MORE_NAMES(*),EXTRA_TAGS(1)
      SAVE NUM_MORE
      DATA EXTRA_TAGS/'JUNK8'/
      DATA NUM_MORE/1/
C----------------------------------------------------------------------
      CALL VZERO(MORE_QUANS,NUM_MORE)
      DO I=1,NUM_MORE
        MORE_QUANS(I)=1.0
      ENDDO
  999 RETURN
C
C
      ENTRY TOP_LEPTONS_MORE_NTUPLE_INI(NUM_MORE_ENTRIES,MORE_NAMES)
C----------------------------------------------------------------------
C-
C-   Entry point : TOP_LEPTONS_MORE_NTUPLE_INI - Initialize the extra
C-                                               tag names for the ntuple.
C-
C-   Inputs  : none
C-   Outputs : NUM_MORE   [I]   : number of added entries
C-             MORE_NAMES [C*8] : tagnames to be added to ntuple
C-   Controls: none
C-
C----------------------------------------------------------------------
C
      NUM_MORE_ENTRIES=NUM_MORE
      DO I=1,NUM_MORE
        MORE_NAMES(I)=EXTRA_TAGS(I)
      ENDDO
C
      RETURN
      END
