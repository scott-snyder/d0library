C----------------------------------------------------------------------
      LOGICAL FUNCTION D0DBL3_CLOSEFZ (FILID,IRET)
C----------------------------------------------------------------------
C-   Purpose and Methods : Will close an FZ file
C-
C-   Return   .true.  it went ok
C-            .false. something went wrong
C-            
C-   Input     FILID  FZ file ID, returned from D0DBL3_OPENFZ
C-   Output    FILID  It will be set to zero, if close succeded  
C-             IRET = iquest(1)
C-                  if -10 invalid file ID
C-
C-   Updated  24-NOV-1992   Lars Rasmussen, Make it possible to have 
C-                          mutiple open FZ files
C-   Updated  15-JAN-1993   Shahriar Abachi  IRET corrected & extended
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER FILID,IRET,IR
C
      INTEGER DBFZ_MXFZ
      PARAMETER (DBFZ_MXFZ=8)
      INTEGER DBFZ_NFLG
      PARAMETER (DBFZ_NFLG=8)
      INTEGER DBFZ_NFZ, DBFZ_LUN(DBFZ_MXFZ), DBFZ_ZDV(DBFZ_MXFZ)
      INTEGER DBFZ_FLG(DBFZ_NFLG,DBFZ_MXFZ)
      COMMON /D0DBL3FZ/ DBFZ_NFZ, DBFZ_LUN, DBFZ_ZDV, DBFZ_FLG
C----------------------------------------------------------------------
      IRET = 0
      D0DBL3_CLOSEFZ = .FALSE.
      IF (FILID .LT. 1 .OR. FILID .GT. DBFZ_MXFZ) THEN
         CALL ERRMSG ('Invalide File ID','D0DBL3_CLOSEFZ',' ','E')
         IRET = -10
         RETURN
      ELSE IF (DBFZ_LUN(FILID) .LE. 0) THEN
         CALL ERRMSG ('Invalide File ID','D0DBL3_CLOSEFZ',' ','E')
         IRET = -10
         RETURN
      END IF
C
      IF (DBFZ_FLG(1,FILID) .EQ. 1) THEN
         CALL FZENDO(DBFZ_LUN(FILID),'T')
      ELSE 
         CALL FZENDI (DBFZ_LUN(FILID),'T')
      END IF
      IF (IQUEST(1) .NE. 0) IRET = IQUEST(1)
      CLOSE (DBFZ_LUN(FILID))
      CALL RLUNIT (984,DBFZ_LUN(FILID),IR)
      D0DBL3_CLOSEFZ = .TRUE.
      DBFZ_LUN(FILID) = 0
      FILID = 0
      DBFZ_NFZ = DBFZ_NFZ - 1
C
      RETURN
      END
