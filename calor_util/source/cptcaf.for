      SUBROUTINE CPTCAF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Fill PTCAEP pointers using information in CAEP bank,
C-       try using CAEH if CAEP not there.
C-       If PTZFLG is false do nothing
C-
C-   Created   1-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER INDCES,LCAEP,LDCAEP,GZCAEP,NCH,NR,I
      INTEGER LDCAEH,LCAEH,GZCAEH
      INTEGER IETA,IPHI,ILYR
      BYTE BYT(4)
      EQUIVALENCE (BYT(1),INDCES)
C----------------------------------------------------------------------
C
      IF(.NOT.PTZFLG) RETURN   ! don't do it if array not zeroed
      LCAEP=GZCAEP()
      IF(LCAEP.GT.0) THEN
C
        NR=IQ(LCAEP+2)
        NCH=IQ(LCAEP+3)
        DO 1 I=1,NCH
          LDCAEP=LCAEP+(I-1)*NR
          INDCES=IQ(LDCAEP+4)
          IETA=BYT(BYTE4)
          IPHI=BYT(BYTE3)
          ILYR=BYT(BYTE2)
          PTCAEP(IETA,IPHI,ILYR)=I
    1   CONTINUE
        PTZFLG=.FALSE.
C
      ELSE IF(GZCAEH().GT.0) THEN   ! get pointers using CAEH
        LCAEH=GZCAEH()
        NR=IQ(LCAEH+2)
        NCH=IQ(LCAEH+3)
        DO 2 I=1,NCH
          LDCAEH=LCAEH+(I-1)*NR
          IETA=IQ(LDCAEH+12)
          IPHI=IQ(LDCAEH+13)
          ILYR=IQ(LDCAEH+14)
          PTCAEP(IETA,IPHI,ILYR)=I
   2    CONTINUE
        PTZFLG=.FALSE.
C
      ELSE
        CALL ERRMSG('CAEH and CAEP not available','CPTACF',' ','W')
      ENDIF
C
      RETURN
      END
