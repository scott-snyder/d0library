      INTEGER FUNCTION LIBLIN(TEXT,ATTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put text on next line of display, possibly with
C-                         scroll. VAX-specific (Uses SMG)
C-
C-   Inputs  : TEXT: String to output
C-             ATTR: Display atttribute (see SMG manual)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    18-DEC-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER ATTR
      INTEGER SMG$M_WRAP_WORD
      PARAMETER(SMG$M_WRAP_WORD=2)
C      INCLUDE '($SMGDEF)' !Useless since it will NOT tkae the IMPLICITE NONE
C      above
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$PUT_LINE,TRULEN
C----------------------------------------------------------------------
      IF(TRULEN(TEXT).EQ.0) THEN
        LIBLIN=SMG$PUT_LINE(MAINID,' ',1,0,
     &                      %VAL(0),%VAL(0),%VAL(0),%VAL(0))
      ELSE
        LIBLIN=SMG$PUT_LINE(MAINID,TEXT,1,ATTR,%VAL(0),SMG$M_WRAP_WORD,
     &                      %VAL(0),%VAL(0))
      ENDIF
      RETURN
      END
