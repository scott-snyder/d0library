      INTEGER FUNCTION SORTBANKS (FIRST, KEY, MODE, DATATYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Takes the linear ZEBRA bank structure which begins
C-   with the bank given by pointer FIRST, and sorts it into ASCENDING or
C-   DESCENDING order, depending on the value of MODE, as determined by they
C-   value of the elements in the word with offset KEY.
C-
C-   Returned value  : the pointer to the new first bank in the chain
C-   Inputs  : FIRST - the pointer to the first bank in the chain to be sorted
C-             KEY   - the offset of the word to be used as the key for sorting
C-             MODE - 'A' or 'a' : sort to ASCENDING order
C-                    'D' or 'd' : sort to DESCENDING order
C-             DATAYPE - 'F' or 'f' : floating point data in key
C-                       'I' or 'i' : integer data in key
C-                       'H' or 'h' : hollerith data in key
C-   Outputs : none
C-   Controls: ZEBRA common block ZEBCOM - contains IXCOM
C-
C-   Created  13-MAY-1991   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C ****  Global variables
C
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER   FIRST, KEY
      CHARACTER*(*) MODE, DATATYPE
C
C ****  Local variables
C
      CHARACTER*31 TYPEMSG
      CHARACTER*34 MODEMSG
      INTEGER      LZLAST
      EXTERNAL     LZLAST
C----------------------------------------------------------------------
C
C ****  First we sort into ASCENDING order
C
      IF ( DATATYPE(1:1) .EQ. 'F' .OR. DATATYPE(1:1) .EQ. 'f') THEN
        CALL ZSORT (IXCOM, FIRST, KEY)
      ELSE IF (DATATYPE(1:1) .EQ. 'I' .OR. DATATYPE(1:1) .EQ. 'i') THEN
        CALL ZSORTI(IXCOM, FIRST, KEY)
      ELSE IF (DATATYPE(1:1) .EQ. 'H' .OR. DATATYPE(1:1) .EQ. 'h') THEN
        CALL ZSORTH(IXCOM, FIRST, KEY)
      ELSE
        TYPEMSG = 'Data type code '//DATATYPE(1:1)//' not recognized'
        CALL ERRMSG ( 'BAD DATATYPE', 'SORTBANKS', TYPEMSG, 'W')
        RETURN
      ENDIF                             ! if datatype(1:1) .eq. ...
C
C ****  Now we reverse to DESCENDING order, if necessary.
C
      IF ( MODE(1:1) .EQ. 'A' .OR. MODE(1:1) .EQ. 'a') THEN
        SORTBANKS = FIRST
        RETURN
      ELSE IF ( MODE(1:1) .EQ. 'D' .OR. MODE(1:1) .EQ. 'd') THEN
        SORTBANKS = LZLAST (IXCOM, FIRST)
        CALL ZTOPSY (IXCOM, FIRST)
        RETURN
      ELSE
        MODEMSG = 'Sorting mode code '//MODE(1:1)//' not recognized'
        CALL ERRMSG ( 'BAD MODE', 'SORTBANKS', MODEMSG, 'W')
        RETURN
      ENDIF

      RETURN
      END
