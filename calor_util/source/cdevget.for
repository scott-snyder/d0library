      SUBROUTINE CDEVGET(CELL,IBAD,AVPHI,AVHI,DIF,STD,NSIG)
C-----------------------------------------------------------------------
C-
C-   Purpose and Method: Find the diffrences DIV of each CELL from the
C-   Phi-averages in AVPHI and AVHI; find the standard deviations STD
C-   of those differences, then the significance (# std devs = DIF/STD
C-   in SIGN) of those differences.  For |SIGN| greater than 3.0 set
C-   bad flag IBAD = 1.
C-   Recall for bimodal cells in eta=36,37, averages in AVHI: 
C-   phi 1,7 in AVHI(ieta,1), and phi 3,5 in AVHI(ieta,2) where
C-   ieta = 1,2,3,4 for eta=-37,-36,36,37.
C-
C-   Stolen from CALHOT package  02-04-1993  RP Smith
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C------------------------------------------------------------------------
      REAL CELL(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL AVPHI(-NETAL:NETAL,1:NLYRL)
      REAL AVHI(1:4,1:2,1:NLYRL)
      REAL DIF(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL STD(-NETAL:NETAL,1:NLYRL)
      REAL NSIG(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL RAT,SUM2
      INTEGER IBAD(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      INTEGER IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND
      INTEGER JETA,JETAL,J8,JN,NPH
C
      CALL VZERO(DIF,NPHIL*NLYRL*(NETAL*2+1))
      CALL VZERO(STD,NLYRL*(NETAL*2+1))
      CALL VZERO(NSIG,NPHIL*NLYRL*(NETAL*2+1))
C
      JETAL  =  NETAL - 2      ! ALL BUT LAST TWO ETAS
      DO IETA = -JETAL,JETAL
        DO ILYR = 1,NLYRL
        NPH = 0
        SUM2 = 0.
          DO IPHI = 1,NPHIL
          ICOND =  0
          CALL CPHAD(IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND)
C         SKIP UNUSED CHANNELS
          IF(ICOND.LE.0) THEN
             NPH = NPH + 1
             DIF(IETA,IPHI,ILYR) = CELL(IETA,IPHI,ILYR) - 
     &                              AVPHI(IETA,ILYR)
C            FIND STD DEVS USING UNFLAGGED CELLS
             IF(IBAD(IETA,IPHI,ILYR).LE.0) SUM2 = SUM2 + 
     1                           DIF(IETA,IPHI,ILYR)**2.
          ENDIF
          ENDDO
        IF(NPH.GT.1) STD(IETA,ILYR) = SQRT(SUM2/(NPH-1))
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
        NPH = 0
        SUM2 = 0.
          DO IPHI = 1,NPHIL
          ICOND =  0
          CALL CPHAD(IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND)
C         SKIP UNUSED CHANNELS
          IF(ICOND.LE.0) THEN
             NPH = NPH + 1
             J8 = ((IPHI-1)/8)*8
             IF(IPHI.EQ.(J8+1).OR.IPHI.EQ.(J8+7)) JN = 1
             IF(IPHI.EQ.(J8+3).OR.IPHI.EQ.(J8+5)) JN = 2
             DIF(IETA,IPHI,ILYR) = CELL(IETA,IPHI,ILYR) - 
     &                              AVHI(JETA,JN,ILYR)
C            FIND STD DEVS USING UNFLAGGED CELLS
             IF(IBAD(IETA,IPHI,ILYR).LE.0) SUM2 = SUM2 + 
     1                           DIF(IETA,IPHI,ILYR)**2.
          ENDIF
          ENDDO
        IF(NPH.GT.1) STD(IETA,ILYR) = SQRT(SUM2/(NPH-1))
        ENDDO
      ENDDO
C
C     NOW FIND SIGNIFICANCE, SET BAD CELL FLAGS (IBAD = 1)
C
      DO IETA = -NETAL,NETAL
        DO ILYR = 1,NLYRL
          DO IPHI = 1,NPHIL
          ICOND =  0
          CALL CPHAD(IETA,IPHI,ILYR,ICRA,IAD,IBLS,IRTW,IDEP,ICOND)
C         SKIP UNUSED CHANNELS
          IF(ICOND.LE.0) THEN
            IF(STD(IETA,ILYR).GT.0.)THEN 
               RAT = DIF(IETA,IPHI,ILYR)/STD(IETA,ILYR)
               IF(ABS(RAT).GT.999.) RAT = SIGN(999.,RAT)
               NSIG(IETA,IPHI,ILYR) = RAT
               IF(ABS(RAT).GT.3.) IBAD(IETA,IPHI,ILYR) = 1
            ENDIF
          ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
