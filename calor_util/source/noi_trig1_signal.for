      SUBROUTINE NOI_TRIG1_SIGNAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add signal to CELL_EN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1991   Allen I. Mincer
C-   Modified 20-SEP-1993   Ian Adam
C-    Add call to CAD_HEADER_CHECK_RESET
C-   Updated   9-JUN-1994   Ian Adam - delete CAD_HEADER_CHECK_RESET calls 
C---------------------------------------------------------------------- 
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      REAL WGT,ENERGY
      INTEGER I,GZCAEP,LCAEP,PT_CAEP,IETA,IPHI,ILYR,NCH,NRP
      LOGICAL OK
C----------------------------------------------------------------------
C
C *** FILL THE CAEP BANKS
C
      CALL NOI_EVENT_SELECT(1)
      LCAEP=GZCAEP()
      IF(LCAEP.LE.0)THEN
        CALL CAEPFL(OK)
        IF(.NOT.OK)THEN
          CALL ERRMSG(' NOISY','NOI_TRIG1_SIGNAL',
     &        ' ERROR IN CAEPFL CALL','W')
          GOTO 999
        ENDIF
      ENDIF
C
C *** ADD THE PRESENT EVENT TO CELL_EN
C
      WGT=WEIGHT(0)
      LCAEP=GZCAEP()
      IF(LCAEP.LE.0) THEN
        CALL ERRMSG(' NOISY','NOI_TRIG1_SIGNAL',
     &    ' NO SIGNAL EVENT CAEP BANKS','W')
        GOTO 999
      ENDIF
      NRP=IQ(LCAEP+2)
      NCH=IQ(LCAEP+3)
      DO I=1,NCH
        PT_CAEP=LCAEP+(I-1)*NRP
        CALL CAEP_INDICES(IQ(PT_CAEP+4),IETA,IPHI,ILYR) ! unpack address
        ENERGY=Q(PT_CAEP+5)
        CELL_EN(IETA,IPHI,ILYR)=
     &        CELL_EN(IETA,IPHI,ILYR)+ENERGY*WGT
      ENDDO
C
C *** DROP THE CAEP BANKS AND ZERO PTCAEP ARRAY FOR TRIG1
C
      CALL CPTCAZ
      CALL MZDROP(IXMAIN,LCAEP,' ')
C----------------------------------------------------------------------
  999 RETURN
      END
