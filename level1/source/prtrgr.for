      SUBROUTINE PRTRGR (PRUNIT,LTRGR,NTRGR,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print TRGR bank in the standard way
C-                         Uses S. Tisserant's routine
C-                         LEVEL1_DATA_BLOCK_DUMP (PRUNIT,MODE)
C-                         Mode is 2 character code: H(S/L)
C-                         H - hex output
C-                         S - Short output if IFL = 1
C-                         L - Long output if IFL = 0
C-
C-   Inputs  : PRUNIT - Output unit number
C-             LTRGR  - Pointer to one bank - set to 0 - ignored
C-             NTRGR  - Bank number - set to 0 here    -ignored
C-             CFL    - Character Flag, set to 'ALL'  - ignored
C-             IFL    - Level of output 0 - full, 1 - minimum, rest undef.
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-OCT-1990   Maris Abolins - Michigan State University
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT ,LTRGR ,NTRGR ,IFL
      CHARACTER*(*) CFL
      CHARACTER*2 MODE
      CHARACTER*32 MSG
C----------------------------------------------------------------------
      IF (IFL .EQ. 1) THEN
        MODE = 'HS'
      ELSEIF (IFL .EQ. 0) THEN
        MODE = 'H'
      ELSE
        WRITE (MSG,9000) IFL
 9000   FORMAT (' ILLEGAL FLAG ',I3)
        CALL ERRMSG (' DEF SET HS ','PRTRGR',MSG,'W')
        MODE = 'HS'
      ENDIF
C
      CALL LEVEL1_DATA_BLOCK_DUMP (PRUNIT,MODE)
C      
  999 RETURN
      END
