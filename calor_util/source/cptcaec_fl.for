      SUBROUTINE CPTCAEC_FL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill PTCAEC pointers using information in
C-                         CAEC bank.
C-   Inputs  : CAEC bank
C-   Outputs : PTCAEC1 and PTCAEC2 arrays.
C-   Controls: If PTCZFL is false do nothing
C-
C-   Created  10-OCT-1989   Andrew P. White
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCAEC.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER INDCES,LCAEC,LDCAEC,GZCAEC,NCH,NR,I
      INTEGER IETA,IPHI,ILYR
      BYTE BYT(4)
      EQUIVALENCE (BYT(1),INDCES)
C----------------------------------------------------------------------
C
      IF(.NOT.PTCZFL) RETURN   ! don't do it if array not zeroed
      LCAEC=GZCAEC()
      IF(LCAEC.GT.0) THEN
C
        NR=IQ(LCAEC+2)
        NCH=IQ(LCAEC+3)
        DO 1 I=1,NCH
          LDCAEC=LCAEC+(I-1)*NR
          INDCES=IQ(LDCAEC+4)
          IETA=BYT(BYTE4)
          IPHI=BYT(BYTE3)
          ILYR=BYT(BYTE2)
          IF(IETA.LT.0) THEN
            PTCAEC1(IETA,IPHI,ILYR)=I
          ELSE
            PTCAEC2(IETA,IPHI,ILYR)=I
          ENDIF
    1   CONTINUE
        PTCZFL=.FALSE.
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
