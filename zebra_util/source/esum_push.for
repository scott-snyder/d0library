      SUBROUTINE ESUM_PUSH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compact ESUM banks down to minimum size
C-
C-   Inputs  : ESUM banks, if any
C-   Outputs : ESUM banks, reduced to minimum size
C-   Controls: none
C-
C-   Created 6-JAN-1992   James T. Linnemann 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LESUM, GZESUM
      INTEGER NUM_OBJ              ! Number of objects held so far
      INTEGER NFIX, NR      ! size of fixed, repeating parts of bank, and total
      INTEGER NSQUEEZE      ! number of data words by which ESUM can be shrunk
C----------------------------------------------------------------------
C
C...loop over linear chain
      LESUM = GZESUM('ANY')
      DO WHILE (LESUM.GT.0) 
        NFIX = IQ( LESUM + 2)
        NR   = IQ( LESUM + 3)
        NUM_OBJ = IQ( LESUM + 4)
        NSQUEEZE =  NFIX + NUM_OBJ*NR  - IQ( LESUM - 1)
        IF (NSQUEEZE.LT.0) CALL MZPUSH(IXCOM,LESUM,0,NSQUEEZE,' ')
        LESUM = LQ(LESUM) ! get link to next bank on chain
      ENDDO
      RETURN
      END
