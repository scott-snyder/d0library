      LOGICAL FUNCTION STPMUO_V1
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
C-   Updated  25-APR-1991   S. Igarashi 
C-   change the function name for version update
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER MPDT
C
      REAL CUML,TUBEL,MUDRFT,SPDLGT
      REAL HITSM(6),CELSIZ(3),VECLOC(6)
      INTEGER IMOD,IPLN,IWIR,IHIT
      DATA SPDLGT/30./                    ! SPEED OF LIGHT
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      STPMUO_V1 = .TRUE.
      IF ( DMUO .LT. 2 ) GOTO 999
C
      IF (FIRST) THEN
        CALL UCTOH('MPDT',MPDT,4,4)
        FIRST = .FALSE.
      ENDIF
C
C     -- store track trajectry...
C
      CALL MSTRAK('UPDT')
C
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
CC    FIGURE OUT DRIFT TIME AND DISTANCE FROM END
CC
CC    CALCULATE DRIFT TIME TO WIRE
CC    FIRST FIND CUMULATIVE LENGTH
      CUML=(VECT(1)-VERT(1))**2+(VECT(2)-VERT(2))**2+
     A (VECT(3)-VERT(3))**2
      CUML=SQRT(CUML)
      HITSM(4)=MUDRFT(VECLOC,CELSIZ(1))+CUML/SPDLGT
CC    CALCULATE DISTANCE FROM EACH END
      TUBEL=2.*CELSIZ(3)
      HITSM(5)=CELSIZ(3)+VECLOC(3)
      IF(HITSM(5).LT.0.) HITSM(5)=0.
      IF(HITSM(5).GT.TUBEL) HITSM(5)=TUBEL
      HITSM(6)=TUBEL-HITSM(5)
CC
CC    STORE HIT
CC
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSM,IHIT)
CC
  999 RETURN
      END
