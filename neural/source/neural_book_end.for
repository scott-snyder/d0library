      SUBROUTINE NEURAL_BOOK_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close output ntuple.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-MAR-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
C----------------------------------------------------------------------
      INTEGER J, STATUS
C----------------------------------------------------------------------
      J = 0
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR('//OUTPUT',' ')
      CALL HROUT(0,J,' ')
      CALL HREND('OUTPUT')
      CLOSE(OUNIT)
      CALL RLUNIT(NETID,OUNIT,STATUS)
  999 RETURN
      END
