      SUBROUTINE ECDIRT(ILUN,IFLAG,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set/Clear the dirty bit in the event catalog.
C-
C-   Inputs  : ILUN   - Logical unit
C-             IFLAG  - Value to be assigned to dirty field
C-   Outputs : IERR   - Error code. 0==>OK 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IFLAG,IERR,ILUN
C----------------------------------------------------------------------
C
      IF( IQ(LECHD+NDEC+JEC_ISDIRT).EQ.IFLAG ) GOTO 999
      IQ(LECHD+NDEC+JEC_ISDIRT)=IFLAG
      CALL ECHUPD(ILUN,IERR)
C
 999  CONTINUE
      IERR=0
      RETURN
      END
