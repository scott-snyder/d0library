      FUNCTION CPRTST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up Print, Dump and Tape output
C-    streams from SRCP for calorimeter frame
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-APR-1989   Rajendran Raja
C-   Updated   5-OCT-1989   Harrison B. Prosper  
C-   Add entry point to close unit numbers 
C-   Updated  10-OCT-1989   Harrison B. Prosper  
C-   Made into logical function 
C-   Updated  11-DEC-1989   Harrison B. Prosper  
C-      Use new RCP routine EZGETS 
C-   Updated  13-SEP-1990   Harrison B. Prosper  
C-      Direct ZEBRA I/O (unit=3) to logical file ZEBRA 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPRTUN,IER,LENGTH
      INTEGER FILNA1(20)
      LOGICAL OK,CPRTST,CPREND
      CHARACTER*80 FILNAM
      INCLUDE 'D0$INC:DUMP.INC'
      INCLUDE 'D0$PARAMS:CALID.DEF'
C----------------------------------------------------------------------
      CPRTST = .TRUE.
C
C ****  Direct ZEBRA I/O to logical file ZEBRA
C
      CALL D0OPEN (3,'ZEBRA','OF',OK)
C
      CALL EZPICK('CALFRAME_RCP')
      CALL EZGETS('PRINT_FILE',1,FILNAM,LENGTH,IER)
C
      CALL GTUNIT(CALID,IPRTUN,IER)
      CALL STSSUN(IPRTUN)               ! store away
      CALL D0OPEN (IPRTUN,FILNAM,'OF',OK)
C
      IF(.NOT.OK)THEN
        CALL ERRMSG('CALORIMETER','CPRTST',
     &  'COULD NOT OPEN PRINTOUT FILE','W')
      ELSE
C
        CALL HOUTPU(IPRTUN)           ! HBOOK Print out goes there
        CALL HERMES(IPRTUN)           ! And error messages
C
      ENDIF
C
C ****  Initialize Dump unit
C
      CALL GTUNIT(CALID,DUNIT,IER)
      CALL EZGETS('DUMP_FILE',1,FILNAM,LENGTH,IER)
      CALL D0OPEN (DUNIT,FILNAM,'OF',OK)
      IF(.NOT.OK)CALL ERRMSG('CALORIMETER','CPRTST',
     &  'COULD NOT OPEN DUMP FILE','W')
C
      CALL EZRSET
  999 RETURN
C
C ****  ENTRY point to close unit numbers
C
      ENTRY CPREND
      CLOSE(UNIT=IPRTUN)
      CLOSE(UNIT=DUNIT)
      CLOSE(UNIT=3)                     ! Zebra
      END
