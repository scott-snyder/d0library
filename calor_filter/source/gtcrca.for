      SUBROUTINE GTCRCA(LCRCA,N,IETA,IPHI,ILAY,EHIT)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump contents of CRCA
C-
C-   Inputs  : LCRCA is zebra link to CRCA bank we want to dump.
C-   Outputs : N = number of cells hit. Prepare for up to 36
C-             IETA(N) = array of ietas of hit cells
C-             IPHI(N) =  "   "   iphis "  "    "
C-             ILAY(N) = "    "   ilayers "  "    "
C-             EHIT(N) = "    "   deposited energy of hit cells
C-   Controls:
C-
C-   Created  17-SEP-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N_MAX
      PARAMETER (N_MAX = 36)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N,IETA(N_MAX),IPHI(N_MAX),ILAY(N_MAX),IAD,I,LCRCA
      INTEGER ICONE
C&IF VAXVMS,VAXELN,LINUX
      BYTE JIAD(4)
      EQUIVALENCE (JIAD,IAD)
C&ENDIF
      REAL EHIT(N_MAX)
C----------------------------------------------------------------------
      N = IQ(LCRCA+1)                   ! Number of cells
      ICONE= IQ(LCRCA + 2)               ! Clustered cone size
      IF (N .GT. N_MAX) THEN
        CALL ERRMSG('Not enough room','PRCRCA',
     &    ' Not enough room to store ALL cells','W')
        N = N_MAX
      END IF

      DO I = 1,N                        ! Cycle through cells
        EHIT(I) = Q(LCRCA + 2*(I-1) + 6)    ! Deposited energy
        IAD     =IQ(LCRCA + 2*(I-1) + 5)    ! packed address of cell
C&IF VAXVMS,VAXELN,LINUX
        IETA(I) = JIAD(4)
        IPHI(I) = JIAD(3)
        ILAY(I) = JIAD(2)
C&ELSE
C&
C&ENDIF
      END DO

  999 RETURN
      END
