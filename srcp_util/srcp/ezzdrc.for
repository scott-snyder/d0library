      SUBROUTINE EZZDRC (RECORD,NAME,IINAME,KKNAME,LNAME,IIREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do preliminary decoding of an SRCP record.
C-
C-   Inputs  : RECORD      Record to be decoded. Character*(*)
C-
C-   Outputs : NAME        First word in upper case
C-             IINAME      Start position of First word in record
C-             KKNAME      End position of First word in record
C-             LNAME       Length of first word
C-             IIREM       Start position of remark in record
C-                         ( = LEN(RECORD) + 1 if no comment present ).
C-
C-   Controls: None
C-
C-   Created   9-NOV-1988   Harrison B. Prosper
C-   Updated  15-JUN-1989   Harrison B. Prosper
C-   Simplified code
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RECORD
      CHARACTER*(*) NAME
      INTEGER       IINAME
      INTEGER       KKNAME
      INTEGER       LNAME
      INTEGER       IIREM
C
      INTEGER CHRRCD
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
C
C ****  Extract first word in record
C
      CHRRCD = LEN(RECORD)
      NAME = ' '
      CALL WORD   (RECORD(1:CHRRCD),IINAME,KKNAME,LNAME)
C
      IF ( LNAME .GT. 0 ) THEN
        LNAME = MIN(LNAME,NUMCHR)
        KKNAME= IINAME + LNAME - 1
        CALL UPCASE (RECORD(IINAME:KKNAME),NAME(1:LNAME))
C
C ****  Return start of comment
C
        IIREM = INDEX (RECORD(1:CHRRCD),'!')
        IF ( IIREM .LE. 0 ) IIREM = CHRRCD + 1
      ELSE
        IIREM = CHRRCD + 1
      ENDIF
C
  999 RETURN
      END
