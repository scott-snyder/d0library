C DEC/CMS REPLACEMENT HISTORY, Element DECODE_PATHS.FOR
C *1     7-FEB-1990 23:01:55 STEWART "DROP_BANKS ASSOCIATED ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element DECODE_PATHS.FOR
      SUBROUTINE DECODE_PATHS
     &  (IXDIV,BRANCH,MAXDEPTH,NBRANCH,IZLINK,NDEPTH,LSTART,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given an array of path descriptors return
C-   for each path the link offsets in IZLINK(MAXDEPTH,*). A path
C-   descriptor is defined by giving a sequence of bank names within
C-   a string; for example, 'RECO JETS'.
C-
C-   Inputs  : IXDIV            [I]     Division number (Unused as yet)
C-             BRANCH(*)        [C*]    Bank paths
C-             MAXDEPTH         [I]     Maximum number of depths 
C-             NBRANCH          [I]     Number of paths
C-             LSTART           [I]     Address at which to start scan
C-                                      of tree
C-             
C-   Outputs : NBRANCH          [I]     Number of paths
C-             IZLINK(MAXDEPTH,*)       [I]     Link offsets
C-             NDEPTH(*)        [I]     Number of depths/path
C-             IER              [I]     0 --- success
C-   Controls:
C-
C-   Created   6-FEB-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IXDIV
      CHARACTER*(*) BRANCH(*)
      INTEGER MAXDEPTH
      INTEGER NBRANCH
      INTEGER IZLINK(MAXDEPTH,*)
      INTEGER NDEPTH(*)
      INTEGER LSTART
      INTEGER IER

      INTEGER MAXWORD
      PARAMETER( MAXWORD = 100 )
      INTEGER LENGTH(MAXWORD)
      CHARACTER*4 BANK(MAXWORD)        ! Internal work space
C
      INTEGER K,JJ,KK,LTARGET,NBANK
      INTEGER LCPATH
C
C ****  For debug ONLY
C
      INTEGER LDEBUG
      LOGICAL DEBUG
      DATA LDEBUG /1/
      DATA DEBUG  /.TRUE./
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C----------------------------------------------------------------------
C
C ****  Loop over each branch and determine link offsets
C
      JJ = 0                          ! Branch counter
      DO KK = 1, NBRANCH
C
C ****  Chop string into words
C
        CALL UPCASE (BRANCH(KK),BRANCH(KK))
        CALL CHOP   (BRANCH(KK),BANK,LENGTH,NBANK)
C
        IF ( NBANK .GT. 0 ) THEN
C
C ****  Check last word; could be a control word
C ****  An asterisk means IGNORE this branch
C
          IF ( BANK(NBANK)(1:1) .NE. '*' ) THEN
            LTARGET = LCPATH (IXDIV,BANK,NBANK,LSTART)
C
            IF ( LTARGET .GT. 0 ) THEN
              JJ = JJ + 1
              CALL ZCHAIN (IXDIV,LSTART,LTARGET,MAXDEPTH,
     &                    BANK,IZLINK(1,JJ),NDEPTH(JJ),IER)
C
              IF ( IER .NE. 0 ) THEN
                GOTO 999              ! Problem
              ELSEIF ( DEBUG ) THEN
                WRITE(LDEBUG,1000) BRANCH(KK)
                WRITE(LDEBUG,1010)
     &                (K,BANK(K),IZLINK(K,JJ),K=1,NDEPTH(JJ))
                WRITE(LDEBUG,1020)
              ENDIF
            ELSE
              ! Put some error stuff here
            ENDIF
C
C ****  Add code for linear chains HERE
C
          ENDIF
        ENDIF
      ENDDO
      NBRANCH = JJ                      ! Number of branches
C
  999 RETURN
C
 1000 FORMAT(1X,10X,'Bank',5X,' Link', '     Path: ',A32)
 1010 FORMAT(1X,I5,5X,A4,5X,I5)
 1020 FORMAT(1x)
      END
