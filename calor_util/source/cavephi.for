      SUBROUTINE CAVEPHI(CELL,IBAD,AVPHI,AVHI)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Find the average over phi of the entries in
C-   CELL, skipping cells with IBAD = 1.  Return averages in AVPHI,
C-   and with those from the bimodal cells for eta=36,37 in AVHI:
C-   phi 1,7 in AVHI(ieta,1), and phi 3,5 in AVHI(ieta,2) where
C-   ieta = 1,2,3,4 for eta=-37,-36,36,37.
C-
C-
C-   Stolen from CALHOT package  02-03-1993  RP Smith
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C------------------------------------------------------------------------
      REAL CELL(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL AVPHI(-NETAL:NETAL,1:NLYRL)
      REAL AVHI(1:4,1:2,1:NLYRL)
      INTEGER IBAD(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      INTEGER NUM(-NETAL:NETAL,1:NLYRL)
      INTEGER NUMHI(1:4,1:2,1:NLYRL)
      INTEGER IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND
      INTEGER JETA,JETAL,J8,JN
C
      CALL VZERO(AVPHI,NLYRL*(NETAL*2+1))
      CALL VZERO(AVHI,8*NLYRL)
      CALL VZERO(NUM,NLYRL*(NETAL*2+1))
      CALL VZERO(NUMHI,8*NLYRL)
C
      JETAL  =  NETAL - 2      ! ALL BUT LAST TWO ETAS
      DO IETA = -JETAL,JETAL
        DO ILYR = 1,NLYRL
          DO IPHI = 1,NPHIL
            ICOND =  0
            CALL CPHAD(IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND)
C           SKIP UNUSED CHANNELS, CHANNELS FLAGGED AS BAD BY DEVCUT
            IF(ICOND.LE.0.AND.IBAD(IETA,IPHI,ILYR).EQ.0) THEN
               NUM(IETA,ILYR)  = NUM(IETA,ILYR) + 1
               AVPHI(IETA,ILYR) = AVPHI(IETA,ILYR) + 
     &                            CELL(IETA,IPHI,ILYR)
            ENDIF
          ENDDO
C         NOW FORM AVERAGES...
          IF(NUM(IETA,ILYR).GT.0)
     &    AVPHI(IETA,ILYR) = AVPHI(IETA,ILYR)/NUM(IETA,ILYR)
        ENDDO
      ENDDO
C
C     NOW DO LAST TWO ETAS
      JETAL = NETAL-1
      DO JETA = 1,4
        IF(JETA.EQ.1) IETA = -NETAL
        IF(JETA.EQ.2) IETA = -JETAL
        IF(JETA.EQ.3) IETA = JETAL
        IF(JETA.EQ.4) IETA = NETAL
        DO ILYR = 1,NLYRL
          DO IPHI = 1,NPHIL
            ICOND = 0
            CALL CPHAD(IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND)
C           SKIP UNUSED CHANNELS, CHANNELS FLAGGED AS BAD BY DEVCUT
            IF(ICOND.LE.0.AND.IBAD(IETA,IPHI,ILYR).EQ.0) THEN
               J8 = ((IPHI-1)/8)*8
               IF(IPHI.EQ.(J8+1).OR.IPHI.EQ.(J8+7)) JN = 1
               IF(IPHI.EQ.(J8+3).OR.IPHI.EQ.(J8+5)) JN = 2
               NUMHI(JETA,JN,ILYR)  = NUMHI(JETA,JN,ILYR) + 1
               AVHI(JETA,JN,ILYR) = AVHI(JETA,JN,ILYR) + 
     &                            CELL(IETA,IPHI,ILYR) 
            ENDIF
          ENDDO
C         NOW FORM AVERAGES...
          DO JN = 1,2
            IF(NUMHI(JETA,JN,ILYR).GT.0)
     &      AVHI(JETA,JN,ILYR) = AVHI(JETA,JN,ILYR)/
     &                           NUMHI(JETA,JN,ILYR)
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END

