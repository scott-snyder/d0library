      PROGRAM PBD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : The Program Builder ( PBD ) generates PBD FORTRAN
C-                         source file, link file, option files and setup file
C-                         for the given framework and packages.   
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Modules called by this routine:  PBD_INIT_COM, PBD_COMMAND, PBD_MSG, 
C-                                    PBD_READ_FILE, PBD_READ_PACK, 
C-                                    PBD_BLD_INIT, PBD_SWITCH, PBD_FLAGS, 
C-                                    PBD_INTERFACE, PBD_COMPILE, PBD_BLD_LINK,
C-                                    PBD_BLD_COM, PBD_HSTRFL
C-  
C-   Based on the PASCAL Program Builder sources
C-
C-   Created  17-JUN-1991   Hyon Joo Kehayias
C-   Updated  14-MAR-1992   Hyon Joo Kehayias 
C-   Updated  24-May-1992    Herbert Greenlee
C-     Changes for transportability.  Inserted call to PBD_INIT_COM.
C-   Updated  28-MAY-1993   Hyon Joo Kehayias 
C-   Updated  10-Jan-1994   H. Greenlee (from C. Silva)
C-       Add call to PBD_BLD_CSH (builds unix setup script).
C-   Updated  10-Feb-1994   Carmem Silva
C-       Add call to PBD_BLD_LNK_CSH (builds unix link script).
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
      INTEGER*2 FILE_TYPE               ! FILE TYPE TO READ
      CHARACTER*80 ERROR_BUF(3)         ! ERROR MESSAGE BUFFER
      CHARACTER*80 MESSAGE              ! MESSAGE BUFFER
C
      DATA ERROR_BUF /
     &  '-- PBD aborting - FATAL ERRORS in command qualifiers',
     &  '-- PBD aborting - FATAL ERRORS in PBD files',
     &  '-- PBD aborting - FATAL ERRORS '/

      DATA MESSAGE /
     &  '-- PBD Normal Completion -- '/
C
C Initialize COMMON variables
C
      CALL PBD_INIT_COM
C
C     Generate the qualifier table by calling PBD_COMMAND
C
      CALL PBD_COMMAND
C
C     Check if any error in command parsing.  If so, display an error 
C     message and exit
C
      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(1) )
        STOP
      END IF
C
C     Check if HSTRFL qualifier present.  If so skip the normal processing and
C     generate only HSTRFL routine.
C
      IF ( QUALFLAG(19) ) THEN
        CALL PBD_HSTRFL
        IF ( ERROR_FLAG ) THEN 
          CALL PBD_MSG ( ERROR_BUF(3) )
        ELSE
          CALL PBD_MSG ( MESSAGE )
        END IF
        STOP
      END IF
C
C     Read the master hook name file and generate hook name table
C
      FILE_TYPE = MASTER_HOOK
      CALL PBD_READ_FILE ( FILE_TYPE )
C
C     Check if any error while reading master hook name file.  
C     If so, display an error message and exit
C
      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(3) )
        STOP
      END IF
C
C     Read the framework file and validate the hook names
C
      FILE_TYPE = FRAME
      CALL PBD_READ_FILE ( FILE_TYPE )
C
C     Check if any error while reading framework file.  
C     If so, display an error message and exit
C
      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(3) )
        STOP
      END IF
C
C     Check input package names, read if any combined packages and 
C     generate global package names table
C
      CALL PBD_CHK_PACK 
C
C     Check if any error during file read.  If so, display an error 
C     message and exit
C
      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(2) )
        STOP
      END IF
C 
C     Read package file and generate tables for user hook names , interface 
C     routine names, input/output banks, object file names and RCP file names
C
      CALL PBD_READ_PACK
C
C     Check if any error during file read.  If so, display an error 
C     message and exit
C
      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(2) )
        STOP
      END IF
C
C     Build FORTRAN source file 
C
C     PBD initialization routine 
C
      CALL PBD_BLD_INIT

      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(3) )
        STOP
      END IF
C
C     Generate code for package switch function
C
      IF ( .NOT. HISTORY ) THEN
        CALL PBD_SWITCH
      END IF
C
C     Generate code for flag routines
C
      CALL PBD_FLAGS
C
C     Generate code for user interface routines
C
      CALL PBD_INTERFACE
C
C     Compile the FORTRAN source and generate .OBJ files
C
      IF ( COMPILE ) CALL PBD_COMPILE

      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(3) )
        STOP
      END IF
C
C     Generate the link file
C
      CALL PBD_BLD_LINK
      IF ( QUALFLAG(21) ) THEN
        CALL PBD_BLD_LNK_CSH
      ENDIF

      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(3) )
        STOP
      END IF
C
C     Build the COM file
C
      CALL PBD_BLD_COM
      IF ( QUALFLAG(21) ) THEN
        CALL PBD_BLD_CSH
      ENDIF

      IF ( ERROR_FLAG ) THEN 
        CALL PBD_MSG ( ERROR_BUF(3) )
        STOP
      END IF

      CALL PBD_MSG ( MESSAGE )
C
      END
