      SUBROUTINE BKJQAN(LJQAN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-JUL-1997   Bob Hirosky
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LJQAN, NDATA, NLINKS, IFORM
      PARAMETER (NDATA   = 50)
      PARAMETER (NLINKS  =  0)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL MZFORM('JQAN', '50F', IFORM)
        FIRST=.FALSE.
      END IF
      CALL MZBOOK(IXMAIN,LJQAN,0,2,'JQAN',NLINKS,0,NDATA,IFORM,0)
  999 RETURN
      END
