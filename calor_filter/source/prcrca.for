      SUBROUTINE PRCRCA(PRUNIT,LCRCA,NCRCA,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump contents of CRCA
C-
C       INPUTS: PRUNIT = unit number for print out.
C-              LCRCA  = link to CRCA
C-              CFL    = Character *(*) not applicable
C-              IFL    = 0 for full printout 1 = minimum
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
      INTEGER PRUNIT,IFL,NCRCA
      CHARACTER *(*) CFL
C&IF VAXVMS,VAXELN
      BYTE JIAD(4)
      EQUIVALENCE (JIAD,IAD)
C&ENDIF
      REAL EHIT(N_MAX),DPHI_CUT
      INTEGER ICONE,MUON_TRACK_FLAG
C----------------------------------------------------------------------
      N = IQ(LCRCA+1)                   ! Number of cells
      ICONE = IQ(LCRCA + 2)             ! Clustered cone radius in cells
      MUON_TRACK_FLAG = IQ(LCRCA + 3)
      DPHI_CUT = Q(LCRCA + 4)
      IF (N .GT. N_MAX) THEN
        CALL ERRMSG('L2CR','PRCRCA',
     &    ' Not enough room to store ALL cells','W')
        N = N_MAX
      END IF

      DO I = 1,N                        ! Cycle through cells
        EHIT(I) = Q(LCRCA + 2*(I-1) + 6)    ! Deposited energy
        IAD     =IQ(LCRCA + 2*(I-1) + 5)    ! packed address of cell
C&IF VAXVMS,VAXELN
        IETA(I) = JIAD(4)
        IPHI(I) = JIAD(3)
        ILAY(I) = JIAD(2)
C&ELSE
C&
C&ENDIF
      END DO
C---START THE DUMP
      IF (PRUNIT .GT. 0) THEN
        WRITE(PRUNIT,*)' CRCA: Number of hit cells is ',N
        WRITE(PRUNIT,*)' CRCA: Clustered cone was radius of ',ICONE,
     &    ' cells'
        WRITE(PRUNIT,*)' CRCA: MUON TRACK FLAG is ',MUON_TRACK_FLAG
        WRITE(PRUNIT,*)' CRCA: DPHI_CUT is ',DPHI_CUT

        IF (PRUNIT .EQ. 0) THEN
          WRITE(PRUNIT,98)
          DO I = 1,N
            WRITE(PRUNIT,99)I,IETA(I),IPHI(I),ILAY(I),EHIT(I)
          END DO
        END IF
      END IF
   98 FORMAT(' ','Cell#','IETA ','IPHI ','ILAY ',
     &        ' Energy Deposited')

   99 FORMAT(' ',5I4,F7.2)
  999 RETURN
      END
