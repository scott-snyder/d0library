      LOGICAL FUNCTION L1DMP_SIMULATED_JET_LIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate the Jet List builder using the Level 1 Data
C-      Block, and output the resulting Jet Lists to the dump file.
C-
C-   Returned value  : Success value
C-   Inputs  : ZEBRA event input
C-   Outputs : Dump File output
C-   Controls: none
C-
C-      NOTE: This uses a Jet List Builder algorithm OTHER than the one used by
C-            L1SIM as of 14-SEP-1992. Eventually this Jet List Builder
C-            algorithm will be used by L1SIM, as it accurately reflects the
C-            behavior of the Level 1 VME Jet List Builder Program.
C-
C-   Created  14-SEP-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER MAX_LENGTH
      PARAMETER (MAX_LENGTH = 1280)
      INTEGER   SPEC_TRIG_MASK, TWR_ADDR
      PARAMETER ( SPEC_TRIG_MASK = 0, TWR_ADDR = 1 )
      INTEGER   EM_ET_LIST, TOT_ET_LIST    
      PARAMETER ( EM_ET_LIST = 0, TOT_ET_LIST = 1 )
C
      INTEGER LTRGR_LEVEL1, GZTRGR, GZFIND_CRATE, DMPUNI
      INTEGER LUN
      INTEGER ENTRY_LIST(SPEC_TRIG_MASK:TWR_ADDR, 1:1280)
      INTEGER JET_LIST_LENGTH(1:2)
      EQUIVALENCE ( JET_LIST_LENGTH(2), ENTRY_LIST(SPEC_TRIG_MASK,1) )
C
      L1DMP_SIMULATED_JET_LIST = .TRUE.
C
      CALL L1UTIL_JET_LIST_BUILDER_FIRST(EM_ET_LIST)
      CALL L1UTIL_JET_LIST_BUILDER_FIRST(TOT_ET_LIST)
      LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', GZTRGR(), 11)
      CALL L1UTIL_JET_LIST_BUILDER( LTRGR_LEVEL1 , EM_ET_LIST, 
     &  MAX_LENGTH, JET_LIST_LENGTH, ENTRY_LIST)
C
C       Existing Jet List
C
      LUN = DMPUNI()
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'Jet Lists (From Existing TRGR Bank)'
      WRITE (LUN,*) '---------'
      WRITE (LUN,*)
      WRITE (LUN,*) 'EM Et Jet List'
      WRITE (LUN,*)
      CALL PRTRGR_JET_LIST_PRINT(LUN, 
     &                           IQ(LTRGR_LEVEL1+TRGR_HEADER_LENGTH+1), 
     &                           IQ(LTRGR_LEVEL1+TRGR_HEADER_LENGTH+1 
     &                              +(EM_ET_JET_LIST-1)/2) )
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'TOT Et Jet List'
      WRITE (LUN,*)
      CALL PRTRGR_JET_LIST_PRINT(LUN,
     &                           IQ(LTRGR_LEVEL1+TRGR_HEADER_LENGTH+1), 
     &                           IQ(LTRGR_LEVEL1+TRGR_HEADER_LENGTH+1
     &                              + (TOT_ET_JET_LIST-1)/2) )
C
C       Simulate new Jet Lists
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'Jet Lists (Seperately Simulated from Level 1)'
      WRITE (LUN,*) '---------'
      WRITE (LUN,*)
      WRITE (LUN,*) 'EM Et Jet List'
      WRITE (LUN,*)
      CALL PRTRGR_JET_LIST_PRINT(LUN, 
     &                           IQ(LTRGR_LEVEL1+TRGR_HEADER_LENGTH+1), 
     &                           JET_LIST_LENGTH)
C
      CALL L1UTIL_JET_LIST_BUILDER( LTRGR_LEVEL1 , TOT_ET_LIST, 
     &  MAX_LENGTH, JET_LIST_LENGTH, ENTRY_LIST)
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'TOT Et Jet List'
      WRITE (LUN,*)
      CALL PRTRGR_JET_LIST_PRINT(LUN,
     &                           IQ(LTRGR_LEVEL1+TRGR_HEADER_LENGTH+1), 
     &                           JET_LIST_LENGTH)
C
C----------------------------------------------------------------------
  999 RETURN
      END
