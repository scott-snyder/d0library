      SUBROUTINE UCWXFL(CWX_COUNT,CWX_CELL,CWX_ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill UCWX bank
C-
C-   Inputs  :
C-             CWX_COUNT  - number of cells to fill
C-             CWX_CELL   - array of packed addresses
C-             CWX_ENERGY - array of energies (GeV)
C-   Outputs :
C-   Controls:
C-
C-   Created   3-DEC-1993   Ian Adam
C-   Updated  14-OCT-1995   Ian Adam  modify UCSH -> UCWX
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LUCWX,J,CWX_COUNT
      INTEGER N_CWX_MAX
      PARAMETER(N_CWX_MAX=2000)
      INTEGER CWX_CELL(N_CWX_MAX)
      REAL    CWX_ENERGY(N_CWX_MAX)
C----------------------------------------------------------------------
C
C BOOK THE BANK

      CALL BKUCWX(CWX_COUNT,LUCWX)

C FILL THE BANK

      IF (LUCWX.GT.0) THEN

        IQ(LUCWX+1) = 1            ! VERSION
        IQ(LUCWX+2) = CWX_COUNT    ! NUMBER OF CELLS
        DO J=1,CWX_COUNT
          IQ(LUCWX+2*J+1) = CWX_CELL(J)
           Q(LUCWX+2*J+2) = CWX_ENERGY(J)
        ENDDO

      ELSE
        CALL ERRMSG('CANNOT BOOK UCWX','UCWXFL',' ','W')
      ENDIF

  999 RETURN
      END
