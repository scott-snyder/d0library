      INTEGER FUNCTION FLAG_MUD1(IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags if MUD1 bank was not present in MC data
C-
C-   Inputs  : IFL  To reset and/or extract flag value
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-MAR-1992   SHAHRIAR ABACHI
C-   Modified 07-APR-92     Atsushi Taketani modified for UNIX
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFL
      INTEGER IFLAG
      SAVE IFLAG
      DATA IFLAG /0/
C
      IF(IFL .EQ. 0) THEN 
        IFLAG = 0
      ELSEIF(IFL .EQ. 1) THEN
        IFLAG = 1
      ENDIF
C
      FLAG_MUD1 = IFLAG
C
C----------------------------------------------------------------------
  999 RETURN
      END
