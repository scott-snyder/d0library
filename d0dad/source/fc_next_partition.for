      LOGICAL FUNCTION FC_NEXT_PARTITION(IFCLUN,NFILES,SELECT,IFID)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Get the FID in the file catalog pointed to
C-     by IFCLUN of the next record which matches the selection string
C-     in SELECT.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-JAN-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) SELECT
      INTEGER IFCLUN,NFILES,IFID
C- Temporary locals
      INTEGER MATCH_FID,IERR,I,INAME,IGENE,ITAPE
      CHARACTER*200 MATCH_FILE,MATCH_GENERIC,MATCH_TAPE,MATCH_COMMENT
      CHARACTER*20  SMATCH_FID
      LOGICAL MATCHED
C- External functions
      INTEGER ICLOCU,LENOCC
C- Key words allowed in selection string
      INTEGER NKEYS
      PARAMETER(NKEYS=4)
      CHARACTER*10 KEYWORDS(NKEYS)
C- Long term local state variables
      INTEGER NEXT_FID
      DATA NEXT_FID/1/,KEYWORDS/'FID=','FILE=','GENERIC=','TAPE='/
      SAVE NEXT_FID,KEYWORDS
C-----------------------------------------------------------------------
      FC_NEXT_PARTITION=.TRUE.

C- Set up the match conditions

C-   Scan for file id?      
      SMATCH_FID=' '
      CALL D0DAD_EXTRACT_FIELD(KEYWORDS(1),SELECT,SMATCH_FID)
      IF( SMATCH_FID.NE.' ' ) READ(SMATCH_FID,*) MATCH_FID
C-   Scan for matching file name?
      MATCH_FIle=' '
      CALL D0DAD_EXTRACT_FIELD(KEYWORDS(2),SELECT,MATCH_FILE)
      CALL CLTOU(MATCH_FILE)
C-   Scan for matching generic name?
      MATCH_GENERIC=' '
      CALL D0DAD_EXTRACT_FIELD(KEYWORDS(3),SELECT,MATCH_GENERIC)
      CALL CLTOU(MATCH_GENERIC)
C-   Scan for matching tape?
      MATCH_TAPE=' '
      CALL D0DAD_EXTRACT_FIELD(KEYWORDS(4),SELECT,MATCH_TAPE)
      CALL CLTOU(MATCH_TAPE)

C- If it's only a FID search, this is easy, so set it and return properly

C- Match the next partition

      I=NEXT_FID
      MATCHED=.FALSE.
      DO WHILE ( .NOT.MATCHED .AND. I.LE.NFILES ) 
        CALL FCGET(IFCLUN,I,CFNAME,CGNAME,CTAPE,CFCCOM,IERR)
        INAME=ICLOCU(MATCH_FILE,LENOCC(MATCH_FILE),CFNAMe,1,200)
        IGENE=ICLOCU(MATCH_GENERIC,LENOCC(MATCH_GENERIC),CGNAME,1,200)
        ITAPE=ICLOCU(MATCH_TAPE,LENOCC(MATCH_TAPE),CTAPE,1,40)
        MATCHED=.TRUE.
        IF( MATCH_FID.NE.0 .AND. I.NE.MATCH_FID ) MATCHED=.FALSE.
        IF( MATCH_FILE.NE.' ' .AND. INAME.EQ.0 ) MATCHED=.FALSE.
        IF( MATCH_GENERIC.NE.' ' .AND. IGENE.EQ.0 ) MATCHED=.FALSE.
        IF( MATCH_TAPE.NE.' ' .AND. ITAPE.EQ.0 ) MATCHED=.FALSE.
 10     I=I+1
      ENDDO

C- Setup up return state and state for next call to this routine.

      IF( .NOT.MATCHED ) THEN 
C-    Found all possible matches to the selection string. None left
        FC_NEXT_PARTITION = .FALSE.
        IFID=0
        NEXT_FID = 1
      ELSE
C-     Made a match, return it
        FC_NEXT_PARTITION = .TRUE.
        NEXT_FID=I
        IFID=I-1
      ENDIF

 999  RETURN
      END
