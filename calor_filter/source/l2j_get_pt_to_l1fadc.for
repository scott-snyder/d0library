      FUNCTION L2J_GET_PT_TO_L1FADC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Zebra pointer to the start of the
C-                         L1 Calorimeter FADC block
C-
C-   Returned value  : Zebra pointer to first word in TRGR FADC block
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-AUG-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L2J_TRGRUNP.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER L2J_GET_PT_TO_L1FADC
      INTEGER LTRGR, GZFIND_CRATE, IBYTE, IWORD, GZTRGR
C----------------------------------------------------------------------
      L2J_GET_PT_TO_L1FADC = 0
      LTRGR = GZFIND_CRATE('TRGR', GZTRGR(), 11) - 1
      IF ( LTRGR .LE. 0 ) RETURN

      IBYTE = 2*LEN_EM1        ! Byte offset to start of FADC block in crate
      IWORD = (IBYTE-1)/4 + 1  ! Corresponding word offset
      IWORD = IWORD + ( IQ( LTRGR + 1 ) + 2 )  ! Add crate header words
      L2J_GET_PT_TO_L1FADC = LTRGR + IWORD
  999 RETURN
      END
