      SUBROUTINE FCEDIT(FILENAME,COMMAND,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Edit a file catalog
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JAN-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) FILENAME,COMMAND,STRING*255
      INTEGER IERR
C- Temporary local storage
      INTEGER I,J,ILUN,IREPL,IFID,LENACT,NFILES
      CHARACTER*255 SELECTION_FIELD,ACTION_FIELD,ACTION
C- External functions
      INTEGER ICFIND,LENOCC
      LOGICAL EDIT_GET_COMMAND,FC_NEXT_PARTITION
C- Keywords for replacement strings
      INTEGER NKEY
      PARAMETER(NKEY=4)
      CHARACTER*8 KEYWORDS(NKEY)
      DATA KEYWORDS/'NAME=','GENERIC=','TAPE=','COMMENT='/
      SAVE KEYWORDS
C-----------------------------------------------------------------------
C
C- Open the file catalog
C
      CALL D0DAD_OPEN(JFFC,FILENAME,'A',ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(*,1001) IERR,FILENAME(1:LENOCC(FILENAME))
 1001   FORMAT(' Error ',I4,' Opening FC: ',A)
        IERR = -1
        RETURN
      ENDIF
      CALL FCHRD(ILUN,NFILES,IERR)
C
C- Process the command(s)
C
      STRING=COMMAND
      DO WHILE(EDIT_GET_COMMAND(STRING,SELECTION_FIELD,ACTION_FIELD))
        LENACT = LENOCC(ACTION_FIELD)
        I=ICFIND('=',ACTION_FIELD,1,LENACT)-1
        ACTION=ACTION_FIELD(1:I)
        CALL CLTOU(ACTION)
C
        DO WHILE( FC_NEXT_PARTITION(ILUN,NFILES,SELECTION_FIELD,IFID) )  
          IF( LDDBG.GT.4 ) THEN
            WRITE(*,1002) ACTION(1:I),IFID
 1002       FORMAT(' Performing ',A', operation on FID ',I6)
          ENDIF
          IF( ACTION(1:I).EQ.'DELETE' ) THEN 
            CALL FCRUB(ILUN,IFID,-1,IERR)
          ELSEIF( ACTION(1:I).EQ.'REPLACE' ) THEN
            IREPL=0
            I=I+1
            IF( I.LT.LENOCC(ACTION_FIELD).AND.ACTION_FIELD(I:I).EQ.'=') 
     >      THEN
              I=I+1
              J=ICFIND(',',ACTION_FIELD,I,LENACT)
              READ(ACTION_FIELD(I:J),*) IREPL
            ENDIF
            CALL FCRUB(ILUN,IFID,IREPL,IERR)
          ELSEIF( ACTION(1:I).EQ.'UNREPLACE' ) THEN
            CALL FCUNRUB(ILUN,IFID,IERR)
          ELSEIF( ACTION(1:I).EQ.'UNDELETE' ) THEN
            CALL FCUNRUB(ILUN,IFID,IERR)
          ELSEIF( ACTION(1:I).EQ.'MODIFY' ) THEN
            CALL FCGET(ILUN,IFID,CFNAME,CGNAME,CTAPE,CFCCOM,IERR)
            CALL D0DAD_EXTRACT_FIELD(KEYWORDS(1),ACTION_FIELD,CFNAME)
            CALL D0DAD_EXTRACT_FIELD(KEYWORDS(2),ACTION_FIELD,CGNAME)
            CALL D0DAD_EXTRACT_FIELD(KEYWORDS(3),ACTION_FIELD,CTAPE)
            CALL D0DAD_EXTRACT_FIELD(KEYWORDS(4),ACTION_FIELD,CFCCOM)
            CALL FCPUT(ILUN,CFNAME,CGNAME,CTAPE,CFCCOM,IFID,IERR)
          ELSE
            IERR = -2
          ENDIF
        ENDDO
C
      ENDDO
C
C- Done
C 
      CALL D0DAD_CLOSE(ILUN,IERR)
C
  999 RETURN
      END
