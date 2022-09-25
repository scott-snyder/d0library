      SUBROUTINE GT_PED_GNS_LV0(TASK,CRATE,CARD,HEAD,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the values and sigmas for PED/GAINS
C-                          from the CPL0, CGL0 banks  (level-0 channels)
C-
C-   Inputs  : TASK = 1,2 peds, 3 gains
C-             CRATE - ADC crate number
C-             CARD  - ADC card number
C-   Outputs : HEAD(30) - contents of header bank
C-             VAL(480) - mean,sigma of each channel
C-   Controls: none
C-
C-   Created  22-APR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,HEAD(*),LINK
      INTEGER CRATE,CARD
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
      CHARACTER*80 STRING
      INTEGER GZCPL0,GZCGL0
      INTEGER LENGTH
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      DATA LENGTH/48/
C----------------------------------------------------------------------
      LINK = 0
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LINK = GZCPL0()
        IF (LINK.EQ.0) THEN
          WRITE(STRING,10)CRATE
   10     FORMAT(' ERROR in GT_PED_GNS_LV0: ',
     &      'no pedestal bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
      ELSE                       !Gains
        LINK = GZCPL0()
        IF (LINK.EQ.0) THEN
          WRITE(STRING,11)CRATE
   11     FORMAT(' ERROR in GT_PED_GNS_LV0: ',
     &      'no gain bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
      ENDIF
C
      IF (LINK.GT.0) THEN
        CALL UCOPY_i(IC(LINK),HEAD,NHEAD)
        CALL UCOPY(C(LINK+NHEAD+CARD*LENGTH+1),VAL,LENGTH)
      ELSE
        HEAD(1)=-1
      ENDIF
  999 RETURN
      END
