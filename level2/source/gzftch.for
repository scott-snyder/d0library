      FUNCTION GZFTCH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTCH
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Created  17-MAY-1992 YI-CHENG LIU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFTCH,LFTCH
      INTEGER GZFTMH,LKFTMH
C----------------------------------------------------------------------
        LKFTMH=GZFTMH()
        IF ( LKFTMH .NE. 0 ) LFTCH=LC(LKFTMH-3)
        GZFTCH=LFTCH      ! Link set !
C----------------------------------------------------------------------
  999 RETURN
      END
