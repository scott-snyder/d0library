      SUBROUTINE BKHSUM(LHSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lift HSUM bank if needed
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Updated  18-JAN-1992   James T. Linnemann   
C-
C=======================================================================
      IMPLICIT NONE
      INTEGER LHSUM
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHSUM.LINK'
C----------------------------------------------------------------------
      LHSUM = LQ( LHEAD - IZHSUM)
      IF ( LHSUM .EQ. 0 ) THEN
        CALL MZBOOK(IXMAIN,LHSUM,LHEAD,-IZHSUM,'HSUM',3,3,1,2,0)
        IQ(LHSUM+1) = 1                 ! version number
      END IF
  999 RETURN
      END
