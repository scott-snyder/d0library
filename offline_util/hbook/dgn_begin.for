      SUBROUTINE DGN_BEGIN(RCPBANK,ARRAY,NTAGNAME,TAGNAME,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define a sub-set of fields from the row-wise
C-   ntuple with identifier ID to be used by DGN.
C-
C-   Inputs:
C-         RCPBANK    [C*]  the name of the RCPBANK containing
C-                          the list of field names to be selected
C-         ARRAY      [C*]  RCP array containing the field names
C-         NTAGNAME   [I]   the number of tagnames in the array TAGNAME
C-         TAGNAME(*) [C*]  tags obtained using HGIVEN.
C-   Outputs:
C-         STATUS   is zero if all is OK
C-
C-   Controls:
C-
C-   Created   4-APR-1994   Harrison B. Prosper
C-   Updated   8-MAR-1995   Harrison B. Prosper
C-   Updated  25-JUN-1995   Harrison B. Prosper   
C-   Updated  10-JUL-1995   Harrison B. Prosper  
C-    Handle all prefixes
C-   Updated  12-JUL-1995   Susan K. Blessing  Allow for multiple prefixes. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPBANK, ARRAY, TAGNAME(*)
      INTEGER NTAGNAME, STATUS, IX, JX
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DGNCOM.INC'
C----------------------------------------------------------------------
      CHARACTER*26 ALPHABET
      PARAMETER( ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
      INTEGER INDEXA
      INTEGER I,J,II,JJ,KK
C
      CHARACTER*32 NAME
      CHARACTER*80 REMARK
C
      LOGICAL EZERROR, FOUND
C----------------------------------------------------------------------
C
C ****  Check number of tags
C
      IF ( NTAGNAME .GT. MAXTAG ) THEN
        STATUS =-90                 ! Too many tags
        GOTO 999
      ENDIF
      NTAG = NTAGNAME
C
C ****  Get list of fields from specified RCP bank
C
      NAME = RCPBANK(1:LEN(RCPBANK))
      CALL EZPICK(NAME)
      IF ( EZERROR(STATUS) ) THEN
        GOTO 999
      ENDIF
C
      NAME = ARRAY(1:LEN(ARRAY))
      CALL EZ_GET_CHARS(NAME,NFIELD,FIELD,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        GOTO 999
      ENDIF
      CALL EZRSET
C
C ****  Make case insensitive and strip off and save prefixes
C
      DO I =  1, NFIELD
        CALL UPCASE(FIELD(I),FIELD(I))
C
C        IF ( INDEX(ALPHABET,FIELD(I)(1:1)).EQ.0 ) THEN
C          PREFIX(I)= FIELD(I)(1:1)
C          FIELD(I) = FIELD(I)(2:)
C        ELSE
C          PREFIX(I)= ' '
C        ENDIF
C
C Find first alphabetic character in FIELD - INDEXA is a CERNLIB routine
        IX = INDEXA(FIELD(I))
        IF ( IX.LE.1 ) THEN
          PREFIX(I)= ' '
        ELSE
          PREFIX(I)= FIELD(I)(1:IX-1)
          FIELD(I) = FIELD(I)(IX:)
        ENDIF
C
        IX = INDEX(FIELD(I),'/')
        IF ( IX.GT.0) THEN
          READ(FIELD(I)(IX+1:32),*) JX 
          XSCALE(I)= JX
          FIELD(I) = FIELD(I)(:IX-1)
        ELSE
          XSCALE(I)= 0.0
        ENDIF
      ENDDO
C
      DO I =  1, NTAG
        CALL UPCASE(TAGNAME(I),TAG(I))
      ENDDO
C
C ****  Sort tag names
C
      DO I =  1, NTAG
        ITAG(I) = I
      ENDDO
      CALL SRTCHR(TAG,NTAG,ITAG)
C
C ****  Check selected inputs
C
      DO I =  1, NFIELD
        CALL LOCSTR(FIELD(I),TAG,NTAG,FOUND,J)
        IF ( .NOT. FOUND ) THEN
          CALL WORD(FIELD(I),II,JJ,KK)
          REMARK = ' The field '//FIELD(I)(II:JJ)//
     &        ' was NOT found amongst list of tags'
          CALL ERRMSG('FIELD_NOT_FOUND','DGN_BEGIN',REMARK,'F')
        ENDIF
      ENDDO
C
      DONE_BEGIN = .TRUE.
  999 RETURN
      END
C
C ****  Block data to initialize common block /DGNCOM/
C
      BLOCK DATA DGNBLOCK
      INCLUDE 'D0$INC:DGNCOM.INC'
      DATA DONE_BEGIN /.FALSE./
      END
