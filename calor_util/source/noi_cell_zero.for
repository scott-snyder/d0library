      SUBROUTINE NOI_CELL_ZERO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Zero CELL_EN array in NOISY package
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1991   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER ILYR,IPHI,IETA
C----------------------------------------------------------------------
C
C ****  ZERO CELL_ENERGY ARRAY
C
      DO ILYR=NLAYMIN,NLAYMAX
        DO IPHI=NPHIMIN,NPHIMAX
          DO IETA=NETAMIN,NETAMAX
            CELL_EN(IETA,IPHI,ILYR) = 0.0
          ENDDO
        ENDDO
      ENDDO
C
  999 RETURN
      END
