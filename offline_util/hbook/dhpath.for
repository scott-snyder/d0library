      SUBROUTINE DHPATH
     &    (CHTOP,BANK,PATH,RCPATH,HDIR,IMAP,TOTAL,SUBDIR,NLEVEL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create hbook directory tree.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-APR-1989   Harrison B. Prosper
C-   Updated   2-JAN-1992   Harrison B. Prosper
C-      Fix duplicate bug
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       TOTAL            ! TOTAL NUMBER OF PATHS
C
      CHARACTER*(*) CHTOP
      CHARACTER*(*) BANK
      CHARACTER*(*) PATH
      CHARACTER*80  RCPATH(TOTAL+1)  ! Names of path descriptors
      CHARACTER*132 HDIR(TOTAL+1)    ! Directory path-names
      CHARACTER*132 CTEMP
      INTEGER       IMAP(TOTAL+1)    ! MAPS HDIR TO PATH
      CHARACTER*(*) SUBDIR(*)        ! Sub-directories
      INTEGER       NLEVEL
      INTEGER       IER
C
      INTEGER NMAX,I,J,K,L,M,N,II,JJ,III,JJJ
      INTEGER LPATH,LSTR,LSIZE
      INTEGER LEVEL
C
      LOGICAL DONE,DUPLICATE
C----------------------------------------------------------------------
      LPATH = LEN(PATH)
      CALL WORD(PATH(1:LPATH),III,JJJ,N)
C
C ****  It is possible that two or more path descriptors lead to the
C ****  same directory. Prefix duplicates with a $ sign.
C
      DUPLICATE = .TRUE.
C
      DO 100 LEVEL = 1, NLEVEL
C
        CALL WORD (SUBDIR(LEVEL),K,L,LSTR)
        IF ( LEVEL .LE. 1 ) THEN
          TOTAL = TOTAL + 1           ! BUMP UP TOTAL DIR-COUNT
          IMAP(TOTAL)   = TOTAL       ! Initialize order map
          RCPATH(TOTAL) = PATH(III:JJJ)
          HDIR(TOTAL)   = SUBDIR(LEVEL)(K:L)
          LSIZE = LSTR
        ELSE
          CTEMP = HDIR(TOTAL)(1:LSIZE)//'/'//SUBDIR(LEVEL)(K:L)
          HDIR(TOTAL) = CTEMP
          LSIZE = LSIZE + 1 + LSTR
        ENDIF
C
C ****  Check if sub-directory already exists at this level
C
        DONE = .FALSE.
        IF ( TOTAL .GT. 1 ) THEN
          DO 50 M = 1,TOTAL-1
            IF ( HDIR(TOTAL)(1:LSIZE) .EQ. HDIR(M)(1:LSIZE) ) THEN
              DONE = .TRUE.
              GOTO 60
            ENDIF
   50     CONTINUE
        ENDIF
C
   60   CONTINUE
C
        IF ( DONE ) THEN
C
C ****  Set sub-directory
C
          CALL HCDIR(SUBDIR(LEVEL)(K:L),' ')
        ELSE
C
C ****  Create sub-directory or set top-directory. The top
C ****  directory should already exist (either //PAWC or a top
C ****  directory created by HROPEN).
C
          IF ( LEVEL .EQ. 1 ) THEN
            CALL HCDIR(SUBDIR(LEVEL)(K:L),' ')
          ELSE
            CALL HMDIR(SUBDIR(LEVEL)(K:L),'S')
          ENDIF
          DUPLICATE = .FALSE.
        ENDIF
  100 CONTINUE
C
C ****  SORT RCPATH FOR NEXT LOCSTR CALL
C
      CALL SRTCHR(RCPATH,TOTAL,IMAP)
C
      IF ( DUPLICATE ) THEN
        CTEMP = '$'//HDIR(TOTAL)
        HDIR(TOTAL) = CTEMP
      ENDIF
C
  999 RETURN
      END
