      SUBROUTINE UCSHFL(CSH_COUNT,CSH_CELL,CSH_ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill UCSH bank
C-
C-   Inputs  :
C-             CSH_COUNT  - number of cells to fill
C-             CSH_CELL   - array of packed addresses
C-             CSH_ENERGY - array of energies (GeV)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-DEC-1993   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LUCSH,J,CSH_COUNT
      INTEGER N_CSH_MAX
      PARAMETER(N_CSH_MAX=2000)
      INTEGER CSH_CELL(N_CSH_MAX)
      REAL    CSH_ENERGY(N_CSH_MAX)      
C----------------------------------------------------------------------
C
C BOOK THE BANK

      CALL BKUCSH(CSH_COUNT,LUCSH)

C FILL THE BANK

      IQ(LUCSH+1) = 1            ! VERSION
      IQ(LUCSH+2) = CSH_COUNT    ! NUMBER OF CELLS

      DO J=1,CSH_COUNT
        IQ(LUCSH+2*J+1)=CSH_CELL(J)
         Q(LUCSH+2*J+2)=CSH_ENERGY(J)
      ENDDO

  999 RETURN
      END
