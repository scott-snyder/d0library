      SUBROUTINE ECEDIT(FILENAME,COMMAND,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Edit an entry in an event catalog.
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED  11-OCT-1996   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      CHARACTER*(*) FILENAME,COMMAND,STRING*255
      INTEGER IERR
C- Temporary local storage
      INTEGER I,J,ILUN,IREPL,LENACT
      INTEGER RUN,EVENT,IFID,MASK(2),ZBO,ZRN
      CHARACTER*255 SELECTION_FIELD,ACTION_FIELD,ACTION,CSTRING*20
C- External functions
      INTEGER ICFIND,LENOCC
      LOGICAL EDIT_GET_COMMAND,EC_NEXT_EVENT
C- Keywords for replacement strings
      INTEGER NKEY
      PARAMETER(NKEY=3)
      CHARACTER*8 KEYWORDS(NKEY)
      DATA KEYWORDS/'FID=','ZEBREC=','ZEBOFF='/
      SAVE KEYWORDS
C-----------------------------------------------------------------------
C
C- Open the file catalog
C
      CALL D0DAD_OPEN(JFEC,FILENAME,'A',ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(*,1001) IERR,FILENAME(1:LENOCC(FILENAME))
 1001   FORMAT(' Error ',I4,' Opening EC: ',A)
        IERR = -1
        RETURN
      ENDIF
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
        DO WHILE( EC_NEXT_EVENT(SELECTION_FIELD,RUN,EVENT) )

          IF( LDDBG.GT.4 ) THEN
            WRITE(*,1002) ACTION(1:I)
 1002       FORMAT(' Performing ',A)
          ENDIF

          CALL ECGET(ILUN,RUN,EVENT,MASK,IFID,ZRN,ZBO,IERR)
          IF( IERR.NE.0 ) THEN
            IF(LDDBG.GT.10) WRITE(*,*) ' Error ',IERR,' getting event'
          ELSEIF( ACTION(1:I).EQ.'DELETE' ) THEN 
            IFID = -ABS(IFID)
          ELSEIF( ACTION(1:I).EQ.'MODIFY' ) THEN
            CALL D0DAD_EXTRACT_IFIELD(KEYWORDS(1),ACTION_FIELD,IFID)
            CALL D0DAD_EXTRACT_IFIELD(KEYWORDS(2),ACTION_FIELD,ZRN)
            CALL D0DAD_EXTRACT_IFIELD(KEYWORDS(3),ACTION_FIELD,ZBO)
          ELSE
            IERR = -2
          ENDIF
          CALL ECPUT(ILUN,RUN,EVENT,MASK,IFID,ZRN,ZBO,IERR)
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
