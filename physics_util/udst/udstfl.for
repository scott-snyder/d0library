      SUBROUTINE UDSTFL(IDMAX,NWORD,XDATA,IGRP,NGRP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank UDST.
C-
C-   Inputs  :  IDMAX   - highest group ID number that is used  
C-              NWORD   - array of number of words/object for each group
C-              XDATA   - array of data elements
C-              IGRP    - max number of groups
C-              NGRP    - max number of words/group
C-
C-   Created  14-MAR-1993   Balamurali V.
C-   Updated  10-JUL-1994   Ulrich Heintz  bank version = 2 
C-   Updated  23-OCT-1995   Ulrich Heintz  bank version = 5 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LUDST
      INTEGER I,J,K,L,POINTER,ND,IGRP,NGRP
      INTEGER IDMAX,NWORD(IGRP)
      REAL    XDATA(IGRP,NGRP) 
C----------------------------------------------------------------------
C Determine no of data words
C
      ND = 0
      DO J = 1,IDMAX
        DO K = 1,NWORD(J)
          ND = ND + 1
        ENDDO
      ENDDO
      ND = ND + IDMAX + 2
C
C Book Bank
C
      CALL BKUDST(ND,LUDST)
C
C Fill Bank
C
      IQ(LUDST+1) = 5               ! Bank version
      IQ(LUDST+2) = IDMAX

      POINTER = LUDST+2
      DO I = 1,IDMAX
        IQ(POINTER+I) = NWORD(I)
      ENDDO

      L = 0
      POINTER = POINTER+IDMAX
      DO J = 1,IDMAX
        DO K = 1,NWORD(J)
          L = L + 1
          Q(POINTER+L) = XDATA(J,K)
        ENDDO
      ENDDO
C
  999 RETURN
      END
