      SUBROUTINE NTUPLE_BOOK (TOP_DIRECTORY,N_VARIABLES,CHTAGS,
     &                        TITLE,NTUPLE_ID,STATUS)
C----------------------------------------------------------------------
C-
C- Purpose and Methods : Book Ntuple for data_logging. By default the
C- Ntuple identifiers will begin  at 1 and will be incremented by 1.
C- These defaults can be changed before calling NTUPLE_BOOK by calling
C- the routine  NTUPLE_SET_ID(ID,IDSTEP) to set the next ID returned by
C- NTUPLE_BOOK. IDSTEP is the amount by which IDs are to be incremented.
C-
C-
C-   Inputs  : TOP_DIRECTORY  [C*]  Name of top Hbook directory
C-             N_VARIABLES    [I]   Number of variables in ntuple
C-             CHTAGS(*)      [C*]  Array containing ntuple field names (tags)
C-                                     (8 characters max.)
C-             TITLE          [C*]  Ntuple title
C-
C-   Outputs : NTUPLE_ID                ! Integer ID of a Ntuple
C-             STATUS                   ! Error status, 0 means NO error
C-   Controls: none
C-
C-   Created  14-MAY-1991   B.S.Acharya
C-   Modified 27-AUG-1991   S. Krzywdzinski   Local counter NT_COUNT for
C-                                            Ntuple ID
C-   Updated  27-SEP-1991   Harrison B. Prosper
C-    Add entry point NTUPLE_SET_ID
C-   Updated  14-NOV-1991   Harrison B. Prosper
C-    Add call to restore directory
C-   Updated   2-DEC-1991   Harrison B. Prosper
C-    Make compatible with DHDIR
C-   Updated  13-FEB-1992   Marc Paterno  Make buffer size a multiple of
C-   N_VARIABLES; set maximum to 10,000 words. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) CHTAGS(*)
      CHARACTER*(*) TITLE
      CHARACTER*(*) TOP_DIRECTORY
C
      INTEGER N_VARIABLES
      INTEGER NTUPLE_ID
      INTEGER STATUS
      INTEGER IDD,IDDSTEP
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
      CHARACTER*80 RZPATH
      INTEGER NT_COUNT,NT_STEP,IDX,LTOP,LTITLE,NPRIME,NPRIMEMAX,MULT
      PARAMETER ( MULT = 100 )          ! this many events per write
      PARAMETER (NPRIMEMAX = 10000)     ! largest buffer allowed
      SAVE NT_COUNT,NT_STEP
      DATA NT_COUNT /0/
      DATA NT_STEP  /1/
C----------------------------------------------------------------------
C
C ****  Select correct file
C
      LTOP   = LEN(TOP_DIRECTORY)
      LTITLE = LEN(TITLE)
      STATUS = 0
C
      CALL NTUPLE_FILE_SELECT1(TOP_DIRECTORY(1:LTOP),IDX)
      IF ( IDX .LE. 0 ) THEN
        STATUS = -1
        GOTO 999
      ENDIF
C
C ****  Get current RZ directory
C
      RZPATH = ' '
      CALL RZCDIR(RZPATH,'R')
C
C ****  Set the buffer size for memory.
C
      NPRIME = N_VARIABLES * MULT
      NPRIME = MIN(NPRIMEMAX, NPRIME)
C
C ****  Book the ntuple
C
      COUNTER(IDX)= COUNTER(IDX) + 1
      NT_COUNT    = NT_COUNT + NT_STEP
      NTUPLE_ID   = NT_COUNT
      CALL HBOOKN (NTUPLE_ID,
     &             TITLE(1:LTITLE),
     &             N_VARIABLES,
     &             RZPATH,
     &             NPRIME,
     &             CHTAGS)
      RETURN
C
      ENTRY NTUPLE_SET_ID(IDD,IDDSTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the value of the first identifier.
C-
C-   Inputs  : IDD      [I] Value of first identifier.
C-             IDDSTEP  [I] Value by which to increment ID.
C-   Outputs : NOne
C-   Controls: None
C-
C-   Created  27-SEP-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      NT_COUNT = IDD - 1
      NT_STEP  = IDDSTEP
  999 RETURN
      END
