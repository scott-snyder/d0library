      SUBROUTINE DHDIRECTORY(BANK,PATH,STATUS,CHTOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-    Set the HBOOK directory given by PATH. The latter is the name of a
C-    path descriptor array in the specified RCP bank.
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
C-    are created starting from the current directory in MEMORY.
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
C-   Updated  13-DEC-1989   Harrison B. Prosper
C-      Corrected level counting
C-   Updated  12-AUG-1990   Chip Stewart
C-      Reset HBOOK DIRECTORY to top if path descriptor is null string
C-   Updated   5-JAN-1991   Harrison B. Prosper
C-      Initialize STATUS to 0
C-   Updated  15-APR-1991   Scott Snyder
C-      Call EZRSET on errors after the bank has been picked.
C-   Updated  25-NOV-1991   Harrison B. Prosper
C-      Can now specify the path directly
C-   Updated  14-DEC-1991   Harrison B. Prosper
C-      Fix problem with ' ' directory
C-   Updated  18-DEC-1991   Harrison B. Prosper
C-      Renamed DHDIRECTORY; called from DHDIR.
C-   Updated   2-JAN-1992   Harrison B. Prosper
C-      Remove LTMP in LOCSTR
C-   Updated  11-FEB-1992   Harrison B. Prosper
C-      Use RZCDIR for RZ directories
C-   Updated  30-APR-1992   Harrison B. Prosper
C-      Allow creation of one directory at a time
C-   Updated  17-JUN-1992   Harrison B. Prosper
C-      Add entry point to delete RZpaths
C-   Updated  22-SEP-1992   Harrison B. Prosper
C-      Fix book-keeping bug in deletion of RZ pathes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH,BANK,CHTOP
      CHARACTER*(*) PREFIX
      INTEGER STATUS
C----------------------------------------------------------------------
      INTEGER NMAX,I,J,K,L,N,M,II,JJ,III,JJJ,NEXT
      INTEGER LMAX
      PARAMETER( NMAX = 50 )            ! Maximum number of sub-directories
      PARAMETER( LMAX = 10 )            ! Maximum number of possible
C                                       ! sub-directory levels
      CHARACTER*132 HDIR(NMAX,2)        ! Directory path-names
      CHARACTER*32  SUBDIR(LMAX)        ! Sub-directories
      CHARACTER*80  RCPATH(NMAX,2)      ! Names of path aliases
      CHARACTER*32  STRING,TOPDIR,RCPBANK
      CHARACTER*80  TMPATH,DIRPATH,CURDIR,CURDIR_IN_PAWC,CURDIR_IN_RZ
      CHARACTER*80  REMARK
C
      INTEGER LEVEL,NLEVEL
      INTEGER LPATH,LBANK,LSTR,LSIZE,LCHTOP,LTMP,LENDIR
      INTEGER IMAP(NMAX,2)              ! MAPS HDIR TO PATH
      INTEGER TOTAL(2)                  ! Total number of
C                                       ! sub-directories found
      INTEGER LOC,IPT,CONTROL
      LOGICAL FOUND,DONE,EZERROR,GET_FROM_RCP,LAST
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST,CURDIR_IN_PAWC,CURDIR_IN_RZ,HDIR,RCPATH,IMAP,TOTAL
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      STATUS = 0
      IF (FIRST) THEN
        FIRST = .FALSE.
        TOTAL(1) = 0    ! For memory
        TOTAL(2) = 0    ! For file
        CURDIR_IN_PAWC = ' '
        CURDIR_IN_RZ   = ' '
      ENDIF
C
      LBANK = LEN(BANK)
      LPATH = LEN(PATH)
      LCHTOP= LEN(CHTOP)
C
      TOPDIR  = CHTOP(1:LCHTOP)
      DIRPATH = PATH(1:LPATH)
      RCPBANK = BANK(1:LBANK)
C
C ****  Check whether to get path from RCP
C
      IF ( RCPBANK(1:1) .EQ. ' ' ) THEN
C
        GET_FROM_RCP = .FALSE.
C
C ****  Check if top directory has been specified; if so split
C ****  the path into TOPDIR and DIRPATH
C
        IF ( DIRPATH(2:2) .EQ. '/' ) THEN
          K = INDEX(DIRPATH(3:),'/')
C
C ****  TOP DIRECTORY SPECIFIED IN PATH; use this as the
C ****  as the top directory unless CHTOP is given in which case
C ****  use CHTOP as the top directory.
C
          IF ( K .GT. 0 ) THEN
            K = K + 2
            IF ( TOPDIR(1:1) .EQ. ' ' ) THEN
              TOPDIR = DIRPATH(3:K-1)
            ENDIF
            DIRPATH= DIRPATH(K:)
          ELSE
            IF ( TOPDIR(1:1) .EQ. ' ' ) THEN
              TOPDIR = DIRPATH(3:)
            ENDIF
            DIRPATH= ' '
          ENDIF
        ELSE
C
C ****  TOP DIRECTORY NOT SPECIFIED
C
C ****  Prefix DIRPATH with '/' if not present
C
          IF ( (DIRPATH(1:1) .NE. ' ') .AND.
     &         (DIRPATH(1:1) .NE. '/') ) THEN
            DIRPATH= '/'//DIRPATH
          ENDIF
C
C ****  The top directory has not been specified so
C ****  append "new" path to current path in memory
C
          CURDIR = CURDIR_IN_PAWC
          CALL WORD(CURDIR,I,J,N)
          K = INDEX(CURDIR(3:J),'/')
C
          IF ( K .GT. 0 ) THEN
            K = K + 2
            DIRPATH= CURDIR(K:J)//DIRPATH
          ENDIF
        ENDIF
      ELSE
        GET_FROM_RCP = .TRUE.
      ENDIF
C
C ****  Set default TOP DIRECTORY
C
      IF ( TOPDIR(1:1) .EQ. ' ' ) THEN
        TOPDIR = 'PAWC'
      ENDIF
C
C ****  Build directory alias string
C
      CALL WORD(TOPDIR,I,J,L)
      CALL WORD(RCPBANK,II,JJ,M)
      CALL WORD(DIRPATH,III,JJJ,N)
C
      IF ( TOPDIR(1:4) .EQ. 'PAWC' ) THEN
        IPT = 1                         ! Select memory
      ELSE
        IPT = 2                         ! Select RZ-file
      ENDIF
      IF ( GET_FROM_RCP ) THEN
        TMPATH = TOPDIR(I:J)//RCPBANK(II:JJ)//DIRPATH(III:JJJ)
        LTMP = L + M + N
      ELSE
        TMPATH = TOPDIR(I:J)//DIRPATH(III:JJJ)
        LTMP = L + N
      ENDIF
C
C ****  Check if PATH has already been made
C
      CALL LOCSTR (TMPATH,RCPATH(1,IPT),TOTAL(IPT),FOUND,LOC)
C
C ****  Set up directory if not yet existing
C
      IF ( .NOT. FOUND ) THEN
C
C ****  Define top directory
C
        SUBDIR(1) = '//'//TOPDIR(I:J)
C
C ****  Check for explicit directory path
C
        IF ( GET_FROM_RCP ) THEN
C
          CALL EZPICK(RCPBANK)            ! SELECT RCP RCPBANK
          IF ( EZERROR (STATUS) ) THEN    ! Problem picking rcpbank
            CALL ERRMSG('NO_RCP_RCPBANK','DHDIR',
     &          'RCP RCPBANK NOT FOUND','W')
            GOTO 999     ! Problem picking rcpbank
          ENDIF
C
C ****  Get FIRST SUB-DIRECTORY NAME
C
          CALL EZGETS(DIRPATH(III:JJJ),1,SUBDIR(2),LENDIR,STATUS)
          IF(STATUS.NE.0) THEN
            CALL ERRMSG('EZ_ERROR','DHDIR','RCP ARRAY NOT FOUND','W')
            CALL EZRSET
            GOTO 999
          ENDIF
C
          IF(LENDIR.EQ.0) THEN
            CALL ERRMSG('ZERO_LEN_STRING','DHDIR',
     &          ' RESET HBOOK DIR TO TOP','W')
            DO I=1,17
              STRING(I:I)=CHAR(92)
            ENDDO
            CALL HCDIR(STRING(1:17),' ')     ! Go to top directory
            CALL EZRSET
            GOTO 999
          ENDIF
C
C ****  If sub-directory is ' ' set nlevel = 1
C
          IF ( SUBDIR(2)(1:1) .EQ. ' ' ) THEN
            NLEVEL = 1    ! top directory only
          ELSE
            NLEVEL = 2
            DO LEVEL = 3, lMAX
              CALL EZGETS(DIRPATH(III:JJJ),LEVEL-1,SUBDIR(LEVEL),LENDIR,
     &          STATUS)
              IF(STATUS.NE.0) GOTO 5
              IF(LENDIR.EQ.0) GOTO 5
              NLEVEL = LEVEL
            ENDDO
          ENDIF
C
    5     CONTINUE
          CALL EZRSET                     ! RESET RCP RCPBANK
C
        ELSE
C
C ****  Path given explicitly.
C
          NLEVEL  = 1
          CONTROL =-1
          LAST    = .FALSE.
          DO WHILE ( .NOT. LAST )
            CALL GET_NEXT_TOKEN(DIRPATH,'/',I,J,N,LAST,CONTROL)
            IF ( NLEVEL .LT. NMAX ) THEN
              IF ( N .GT. 0 ) THEN
                NLEVEL = NLEVEL + 1
                SUBDIR(NLEVEL) = DIRPATH(I:J)
              ELSE
                LAST = .TRUE.
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C
C ****  CREATE DIRECTORY WITH DHPATH
C
        CALL DHPATH(TOPDIR,
     &              RCPBANK,
     &              TMPATH,
     &              RCPATH(1,IPT),
     &              HDIR(1,IPT),
     &              IMAP(1,IPT),
     &              TOTAL(IPT),
     &              SUBDIR,
     &              NLEVEL,
     &              STATUS)
C
C ****  Re-determine LOC (check FOUND)
C
        CALL LOCSTR (TMPATH,RCPATH(1,IPT),TOTAL(IPT),FOUND,LOC)
        IF (.NOT.FOUND) THEN
          CALL ERRMSG('DHDIR','DHDIR','DIRECTORY WAS NOT MADE','W')
          GOTO 999
        ENDIF
      ENDIF
C
C ****   SET HBOOK DIRECTORY
C
C ****  NOTE: Duplicate paths are prefixed with a $ sign
C
      J = IMAP(LOC,IPT)
      IF ( HDIR(J,IPT)(1:1) .EQ. '$' ) THEN
        K = 2
      ELSE
        K = 1
      ENDIF
C
C ****  Use HCDIR for PAWC and RZCDIR for RZ
C
      IF ( IPT .EQ. 1 ) THEN
        CURDIR_IN_PAWC = HDIR(J,IPT)(K:)
        CALL HCDIR(CURDIR_IN_PAWC,' ')
      ELSE
        CURDIR_IN_RZ   = HDIR(J,IPT)(K:)
        CALL RZCDIR(CURDIR_IN_RZ,' ')
      ENDIF
C
      RETURN
C
C ****  Entry point to return paths starting with a given path
C ****  segment containing the top-directory
C
      ENTRY DHDIR_GET_NEXT_PATH(PREFIX,PATH,NEXT)
      IF ( PREFIX(1:6) .EQ. '//PAWC' ) THEN
        IPT = 1
      ELSE
        IPT = 2
      ENDIF
C
      CALL WORD(PREFIX(1:LEN(PREFIX)),I,J,N)
C
      PATH = ' '
      DO I = 1, TOTAL(IPT)
        IF ( PREFIX(1:J) .EQ. HDIR(IMAP(I,IPT),IPT)(1:J) ) THEN
          PATH = HDIR(IMAP(I,IPT),IPT)
          NEXT = NEXT + 1
          GOTO 950
        ENDIF
      ENDDO
  950 CONTINUE
C
C ****  No path found if PATH = ' '
C
      IF ( PATH .EQ. ' ' ) THEN
        NEXT = -1
      ENDIF
      RETURN
C
C ****  Show current directory
C
      ENTRY DHSHOW
      CURDIR = ' '
      CALL HCDIR(CURDIR,'R')
      CURDIR = '   '//CURDIR
      CALL WORD(CURDIR,I,J,K)
      CALL INTMSG(CURDIR(1:J))
      RETURN
C
      ENTRY DHSHOWRZ
      CURDIR = ' '
      CALL RZCDIR(CURDIR,'R')
      CURDIR = '   '//CURDIR
      CALL WORD(CURDIR,I,J,K)
      CALL INTMSG(CURDIR(1:J))
      RETURN
C
C ****  Entry point to delete specified RZ-path from the list of pathes
C
      ENTRY DHDIR_DELETE_RZPATH(PATH)
C
      IPT = 2
      CALL WORD(PATH(1:LEN(PATH)),I,J,N)
C
C ****  Find Index of path
C
      N = 0
      DO I = 1, TOTAL(IPT)
        IF ( PATH(1:J) .EQ. HDIR(IMAP(I,IPT),IPT)(1:J) ) THEN
          N = 1
          RCPATH(I,IPT) = '_'//RCPATH(I,IPT)   ! Force this path to the end
          GOTO 960
        ENDIF
      ENDDO
C
C ****  Remove path from list RCPATH(1..TOTAL(IPT),IPT)
C
  960 CONTINUE
C
      IF ( N .GT. 0 ) THEN
C
C ****  Re-sort list RCPATH; decrement path counter
C
        CALL SRTCHR(RCPATH(1,IPT),TOTAL(IPT),IMAP(1,IPT))
        TOTAL(IPT) = TOTAL(IPT) - 1
      ENDIF
      RETURN
C
C ****  Dump pathes
C
      ENTRY DHDUMP
      DO IPT = 1, 2
        CALL INTMSG(' ')
        IF ( TOTAL(IPT) .GT. 0 ) THEN
          IF ( IPT .EQ. 1 ) THEN
            REMARK = ' PAWC directories known to DHDIR'
          ELSE
            REMARK = ' RZ   directories known to DHDIR'
          ENDIF
          CALL INTMSG(REMARK)
          DO I =  1, TOTAL(IPT)
            IF ( HDIR(IMAP(I,IPT),IPT)(1:1) .NE. '$' ) THEN
              CALL SWORDS(HDIR(IMAP(I,IPT),IPT),II,JJ,JJJ)
              REMARK = '  '//HDIR(IMAP(I,IPT),IPT)(II:JJ)
              CALL INTMSG(REMARK)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
  999 RETURN
      END
