      SUBROUTINE CDBITT(L1INX,L1ETAC,L1PHIC,ITSOK)  ! Convert L-1 Index to 
C                                                   ! Trigger-Tower index.
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : It is here that that we convert L-1 trigger block
C-                         index to L-1 (L1ETAC,L1PHIC) indices. The routine
C-                         relys heavely on the properties of integer
C-                         arithmatic in the algorithm. See D0 Note 967 on how
C-                         the trigger block index indexes into the ADC tables.
C-                         It is this method of mapping that dictates the
C-                         method of conversion here.
C-
C-   Inverse Routine:      CTTDBI
C-
C-   Inputs  : L1INX is the l-1 trigger block index.
C-                        (range of L1INX is [0,2*NPHIL1*NETAL1-1] )
C-
C-   Outputs : L1ETAC and L1PHIC are the TT index pairs converted to.
C-             ITSOK returns .TRUE. if the conversion went w/o problems.
C-
C-   Controls: None.
C-
C-   Created   8-MAY-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
C     Passed Variables:
*
         INTEGER L1INX  ! ...Level-1 INdeX in the the canadate list in the
                        !  l-1 trigger data block (this is also the
                        !  realtive address to get the adc value in
                        !  the L-1 data block).*
         INTEGER L1ETAC,L1PHIC  ! L-1 (eta,phi) trigger tower index
         LOGICAL ITSOK  !  .True. if conversion ok.
*
C     Local Variables:
*
         INCLUDE 'D0$PARAMS:L1PHP.PARAMS'  ! L-1 to PHysics [this routine]
                                        ! Parameters: contains volitile
C                                       ! parameters
 
                                       ! that strongly affect this routine,
                                        ! if changed.
         INTEGER NETEP,PHII
         PARAMETER (NETEP=NETAL1*NPHIL1)
*
      ITSOK = .TRUE.  !  ...until proven GUILTY...
*
      IF (L1INX.LT.0.OR.L1INX.GE.(NETEP*2)) THEN
        ITSOK = .FALSE.
        GOTO 999
      END IF
*
C     ------ phi index:
*
      PHII = IAND(L1INX,31)
      L1PHIC = PHII/2 + 1
      IF (IAND(L1INX,1).EQ.1) L1PHIC=L1PHIC+16
C     IF (BTEST(L1INX,0)) L1PHIC=L1PHIC+16
*
C     ------ eta index:
*
      IF (L1INX.LT.NETEP) THEN
          L1ETAC = L1INX/NPHIL1+1
        ELSE
          L1ETAC = (L1INX-NETEP)/NPHIL1 +1
      END IF
      IF (L1INX.GE.NETEP) L1ETAC= -L1ETAC
*
*
  999 RETURN
      END
