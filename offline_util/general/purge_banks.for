      SUBROUTINE PURGE_BANKS (RCPFILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Keep/Drop banks specified in KEEP_BANKS,
C-   DROP_BANKS structures in given RCP file.
C-   Uses:
C-      KEEP_BANKS
C-      DROP_BANKS
C-
C-   Inputs  : RCPFILE  [C*]    Name of RCP bank
C-   Outputs : IER      [I]     0 --- routine successful
C-   Controls: None
C-
C-   Created   6-FEB-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RCPFILE
      INTEGER IER
C
      INTEGER MAXDEPTH
      PARAMETER( MAXDEPTH =  50 )
      INTEGER MAXBRANCH
      PARAMETER( MAXBRANCH = 25 )
C
      INTEGER NBRANCH,NDROP,NKEEP
      INTEGER IZLINK(MAXDEPTH,2*MAXBRANCH)
      INTEGER NDEPTH(2*MAXBRANCH)
      INTEGER LNBANK(MAXDEPTH)
      CHARACTER*80 BRANCH(2*MAXBRANCH)
C
      INTEGER LSTART
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST,NDROP,NKEEP,IZLINK,NDEPTH
C----------------------------------------------------------------------
      LSTART= LHEAD
C
C ****  Do command decoding once only.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Select RCP bank
C
        CALL EZPICK (RCPFILE(1:LEN(RCPFILE)))
C
C ****  Do KEEP banks
C
        CALL EZ_GET_CHARS ('KEEP_BANKS',NBRANCH,BRANCH,IER)
C
        IF ( IER .EQ. 0 ) THEN
          CALL DECODE_PATHS (IXCOM,BRANCH,MAXDEPTH,
     &    NBRANCH,IZLINK,NDEPTH,LSTART,IER)
          NKEEP = NBRANCH
        ELSE
          NKEEP = 0
        ENDIF
C
C ****  Do DROP banks
C
        CALL EZ_GET_CHARS ('DROP_BANKS',NBRANCH,BRANCH,IER)
C
        IF ( IER .EQ. 0 ) THEN
          CALL DECODE_PATHS (IXCOM,BRANCH,MAXDEPTH,
     &    NBRANCH,IZLINK(1,MAXBRANCH+1),NDEPTH(MAXBRANCH+1),LSTART,IER)
          NDROP = NBRANCH
        ELSE
          NDROP = 0
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  Keep banks
C
      IF ( NKEEP .GT. 0 ) THEN
        CALL KEEP_BANKS (IXCOM,MAXDEPTH,NKEEP,IZLINK,NDEPTH,LSTART,IER)
      ENDIF
C
C ****  Drop banks
C
      IF ( NDROP .GT. 0 ) THEN
        CALL DROP_BANKS (IXCOM,MAXDEPTH,NDROP,
     &          IZLINK(1,MAXBRANCH+1),NDEPTH(MAXBRANCH+1),LSTART,IER)
      ENDIF
C
  999 RETURN
      END
