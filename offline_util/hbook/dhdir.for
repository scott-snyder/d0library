      SUBROUTINE DHDIR (BANK,PATH,STATUS,CHTOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-    Set the HBOOK directory given by PATH. The latter is the name of a
C-    path descriptor array in the specified RCP bank.
C-    
C-    Use entry point
C-    
C-        CALL DHDIR_DECLARE_FILE(TOP_DIRECTORY)
C-        
C-    to declare the top directory of the file in which the directory
C-    tree in //PAWC is to be replicated. This routine can be called
C-    at any time to re-define that file.
C-    
C-    Use entry point
C-    
C-        CALL DHDIR_SAVE_FILE
C-        CALL DHDIR_RESTORE_FILE
C-        
C-    to save/restore the previously declared top directory.
C-
C-    See below for an example of a path descriptor.
C-    Call DHSHOW from the debugger to display the current directory.
C-
C-    You can also specify the path directly in the string PATH. In this
C-    case the variable BANK should be set to a single blank: ' '.
C-
C-    If CHTOP is blank then the top directory is set to //PAWC, when
C-    creating the full pathname, otherwise CHTOP is used as the top
C-    directory.
C-
C-    If however the full pathname is given in variable PATH, that is it
C-    includes '//top directory', and if CHTOP is blank, then the top 
C-    directory specified in the full pathname is used; otherwise CHTOP 
C-    is used as the top directory.
C-
C-    If the top directory is omitted from PATH the new directories
C-    are created starting from the current directory.
C-
C-   Inputs  : BANK        Name of RCP bank
C-             PATH        Name of HBOOK directory path descriptor
C-                         OR directory path.
C-
C-   Outputs : STATUS      0--> OK
C-
C-   Controls: CHTOP       Top Directory. If blank set to //PAWC.
C-
C-   Example : Path descriptor
C-
C-      \ARRAY HBOOK_DIRECTORY
C-          'CALORIMETER'
C-          'CAHITS'
C-      \END
C-
C-      This descriptor defines the path CALORIMETER/CAHITS. The
C-      full pathname would be //PAWC/CALORIMETER/CAHITS.
C-
C-   Created  20-APR-1989   Harrison B. Prosper, Chip Stewart
C-   Updated  18-DEC-1991   Harrison B. Prosper
C-      Call DHDIR1 to create path both in memory and, optionally, in
C-      an RZ file.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH,BANK,CHTOP
      INTEGER STATUS
      CHARACTER*(*) TOP_DIRECTORY
C----------------------------------------------------------------------
      CHARACTER*32  TOPDIR,SAVE_TOPDIR
      SAVE TOPDIR,SAVE_TOPDIR
      DATA TOPDIR     /' '/
      DATA SAVE_TOPDIR/' '/
C----------------------------------------------------------------------
C
C ****  Create/Set path in the RZ-file specified by topdir
C
      IF ( TOPDIR(1:1) .NE. ' ' ) THEN
        CALL DHDIRECTORY(BANK(1:LEN(BANK)),
     &                   PATH(1:LEN(PATH)),
     &                   STATUS,
     &                   TOPDIR)
      ENDIF
C
C ****  Create/Set path in //PAWC
C
      CALL DHDIRECTORY(BANK(1:LEN(BANK)),
     &                 PATH(1:LEN(PATH)),
     &                 STATUS,
     &                 CHTOP(1:LEN(CHTOP)))
      RETURN
C
C ****  Give top directory of file in which paths are to be replicated.
C
      ENTRY DHDIR_DECLARE_FILE(TOP_DIRECTORY)
      TOPDIR = TOP_DIRECTORY
      RETURN
C
      ENTRY DHDIR_SAVE_FILE
      SAVE_TOPDIR = TOPDIR
      RETURN
C
      ENTRY DHDIR_RESTORE_FILE
      TOPDIR = SAVE_TOPDIR
  999 RETURN
      END
