      SUBROUTINE CPTCAZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Zero PTCAEP pointers using information in CAEP bank
C-       or CAEH bank if CAEP not there.
C-       If PTZFLG is true do nothing
C-
C-   Modified 15-JUN-1990   Nobu Oshima(Get proper NUMALL.)
C-   Created   1-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER INDEX,LCAEP,LDCAEP,GZCAEP,NCH,NR,I
      INTEGER LDCAEH,LCAEH,GZCAEH
      INTEGER IETA,IPHI,ILYR
      INTEGER NUMALL
      BYTE BYT(4)
      EQUIVALENCE (BYT(1),INDEX)
C----------------------------------------------------------------------
C
      IF(PTZFLG) RETURN      ! don't do it if array is empty
C
      LCAEH=GZCAEH()
      IF(LCAEH.GT.0) THEN   ! zero pointers using CAEH
        NR=IQ(LCAEH+2)
        NCH=IQ(LCAEH+3)
        DO 12 I=1,NCH
          LDCAEH=LCAEH+(I-1)*NR
          IETA=IQ(LDCAEH+12)
          IPHI=IQ(LDCAEH+13)
          ILYR=IQ(LDCAEH+14)
          PTCAEP(IETA,IPHI,ILYR)=0
  12    CONTINUE
C
      ELSE IF(GZCAEP().GT.0) THEN
        LCAEP=GZCAEP()
        NR=IQ(LCAEP+2)
        NCH=IQ(LCAEP+3)
        DO 11 I=1,NCH
          LDCAEP=LCAEP+(I-1)*NR
          INDEX=IQ(LDCAEP+4)
          IETA=BYT(BYTE4)
          IPHI=BYT(BYTE3)
          ILYR=BYT(BYTE2)
        PTCAEP(IETA,IPHI,ILYR)=0
   11   CONTINUE
C
      ELSE
        NUMALL=(2*NETAL+1)*NPHIL*NLYRL
        CALL VZERO(PTCAEP,NUMALL)
C
      ENDIF
      PTZFLG=.TRUE.
C
      RETURN
      END
