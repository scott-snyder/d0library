      LOGICAL FUNCTION STPMUO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   THIS ROUTINE HANDLES STEPPING THROUGH MUON DRIFT CELLS.
C-
C-   Returned value  : .TRUE.
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   1-NOV-1985   Dave Hedin
C-   Updated   1-APR-1987   S. Kunori 
C-   Updated   2-JUN-1989   Harrison B. Prosper  
C-   Made into a program-builder interface function 
C-   Updated   9-APR-1991   Susumu Igarashi   For digitization
C-   Updated  25-JUN-1992   Susumu Igarashi   Get inefficient region
C-                                            length from the bank MGEH
C-   Updated  26-AUG-1992   Susumu Igarashi   Time of Flight from GCBANK 
C-   Updated   9-APR-1993   Jasbir Singh , Chip Stewart Added GMUH_BUILD,CALEXIT
C-   Updated  27-FEB-1993   Susumu Igarashi   Fix bug of HITSM(5) COSPHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER MPDT
      INTEGER GZMGEH,LH,VSN
C
      REAL CUML,TUBEL,SPDLGT
      REAL HITSM(9),CELSIZ(3),VECLOC(6)
      INTEGER IMOD,IPLN,IWIR,IHIT
      REAL PHI,COSPHI,THETA
      REAL INEFF                          ! Inefficient Length
C
      LOGICAL STPMUO_V1
      EXTERNAL STPMUO_V1
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA SPDLGT/30./                    ! SPEED OF LIGHT
C----------------------------------------------------------------------
********************************************
*     Big Branch to old version,  V1.      *
********************************************
      IF(SMUO(2).LT.1.5) THEN
         STPMUO=STPMUO_V1()
         RETURN
      ENDIF
C
      STPMUO = .TRUE.
      IF ( DMUO .LT. 2 ) GOTO 999
C
      IF (FIRST) THEN
        CALL UCTOH('MPDT',MPDT,4,4)
        FIRST = .FALSE.
        LH=GZMGEH()
        VSN=IC(LH+1)
        IF(VSN.GE.3) INEFF=C(LH+24)
      ENDIF
C
C     -- store track trajectry...
C
      CALL MSTRAK('UPDT')
C
CC   NO MUON CHAMBER HIT IF NO CHARGE
      IF(IHSET.NE.MPDT.OR.INWVOL.NE.1.OR.CHARGE.EQ.0.) GOTO 999
CC   SKIP GEANTINO.
      IF(IPART.EQ.48) GOTO 999
CC
CC    FIGURE OUT MODULE,PLANE AND WIRE HIT
CC
      CALL MSHITS(IHDET,IMOD,IPLN,IWIR)
      HITSM(1)=IMOD
      HITSM(2)=IPLN
      HITSM(3)=IWIR
CCC   CONVERT TO LOCAL COORDINATE SYSTEM
      CALL LOCGEO(VECT,VECLOC,CELSIZ,3)
CC
CC    FIGURE OUT DISTANCE FROM WIRE, DISTANCE FROM END AND INCIDENT ANGLES
CC
CC    CALCULATE INCIDENT ANGLES
      IF(VECLOC(4).NE.0.0.AND.VECLOC(5).NE.0.0)THEN
        COSPHI=VECLOC(4)/SQRT(VECLOC(4)**2+VECLOC(5)**2)
      ELSE
        COSPHI=1.0
      ENDIF
      IF(VECLOC(5).GE.0.0)PHI=ACOS(COSPHI)
      IF(VECLOC(5).LT.0.0)PHI=-ACOS(COSPHI)
      HITSM(7)=PHI*180./3.1415
      THETA=ACOS(VECLOC(6))
      HITSM(8)=THETA*180./3.1415
CC    CALCULATE DISTANCE FROM WIRE
      HITSM(4)=-VECLOC(1)*SIN(PHI)+VECLOC(2)*COSPHI
CC    CALCULATE DISTANCE FROM EACH END
      TUBEL=2.*CELSIZ(3)
      IF(THETA.EQ.0..OR.THETA.EQ.180.) THETA=0.001
      HITSM(5)=VECLOC(3)+CELSIZ(3)+CELSIZ(1)/TAN(THETA)/ABS(COSPHI)
      IF(VSN.LT.3)THEN
        IF(HITSM(5).LT.6.418.OR.HITSM(5).GT.TUBEL-4.513) GOTO 999
      ELSE
        IF(HITSM(5).LT.INEFF.OR.HITSM(5).GT.TUBEL-INEFF) GOTO 999
      ENDIF
      HITSM(6)=TUBEL-HITSM(5)
CC    CALCULATE TIME OF FLIGHT
C      CUML=VECT(1)**2+VECT(2)**2+VECT(3)**2
C      CUML=SQRT(CUML)
C      HITSM(9)=CUML/SPDLGT
      HITSM(9)=TOFG*1.0E9
CC
CC    STORE HIT
CC
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSM,IHIT)
C
C ****  write into MUON hit library if SCAL(4) is not zero
C     -- and punchthrough if SMUO(4) set
      IF((IHIT*SMUO(4)).NE.0) CALL MCALEXIT('UPDT')
C
      IF((IHIT*SMUO(4)).NE.0) CALL GMUH_BUILD(HITSM,IHIT)
CC
  999 RETURN
      END
