      SUBROUTINE CL2TEST_FORGET_EVT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset internal memory of cl2 unpacking so this event
C-   is forced to be redone; drop previous results
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:TTDONE.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ADC_LOCATION.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:CL2_LINK.INC'
       CHARACTER*4 OLD_PATH
       INTEGER GZCAEP,GZPNUT
C----------------------------------------------------------------------
      IQ(LHEAD+7) = -IABS(IQ(LHEAD+7)) - 1 ! forget this event
      CL2CAEP_EVT = 0                     ! and avoid any clash with old one
      CL2CAD_EVT = 0
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')
      L2CAEP = GZCAEP()
      IF (L2CAEP.NE.0) CALL MZDROP(IXCOM,L2CAEP,'L')
      L2CAEP = 0                        ! be sure the link area forgets
                                        ! bank
      L2PNUT = GZPNUT(1)
      IF (L2PNUT.NE.0) CALL MZDROP(IXCOM,L2PNUT,'L')
      L2PNUT = 0
      PTCAEP2_ALL_ZEROS = .FALSE.          ! force zeroing unconditionally
      CALL CL2_VZERO_PTCAEP2
      CALL PATHST(OLD_PATH)
  999 RETURN
      END
