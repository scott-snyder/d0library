      SUBROUTINE BMPIPE(TTHET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Puts in Main Ring Beam Pipe Zdivisions
C-
C-   Inputs  : TTHET = Tangent of angle of slope of TRAP
C-   Outputs : None
C-   Controls: None
C-
C-   Created  15-NOV-1988   Rajendran Raja
C-   sign of YY changed 20-jun-1989 N.A. Graf
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL BMP(30),PHI,PI
      INTEGER IBMP(30),NSEG
      EQUIVALENCE (BMP,IBMP)
      REAL CONV,CRACK,RIN,ROUT,DELZ,ZCEN,ZLO,ZHI,CRACKM,Z0,ZLO0,ZHI0
      REAL XXMOTH,YYMOTH,RBMIN,RBMOUT,XPOSBM,YPOSBM,XX,YY,ZZ,XX1,ZZ1
      REAL PAR(15),TTHET
      INTEGER MATBM,NPAR,ICPY2,IROT,KK,MOTHERBM
      INTEGER IRTOFF,IRTOF(2),NAME,MATNO,MOTHER,I,NDIV,IPT,IZ,LEN3,K
      INTEGER LSR(500)
      REAL RSR(500)
      EQUIVALENCE (LSR,RSR)
      CHARACTER*18 MOTHVL(2)
      CHARACTER*24 ZDVNM(2)
      CHARACTER*24 OHEND(2)
      CHARACTER*17 OHMASS(2)
      CHARACTER*15 OHGZDV(2)
      CHARACTER*32 NMSRCP
      CHARACTER*40 ARNAME
      CHARACTER *4 NAMOTH,NAMEZ,ACTION
      DATA MOTHVL/'OH_MOTHER_VOLUME+Z',
     &            'OH_MOTHER_VOLUME-Z'/
      DATA ZDVNM/'BEAM_PIPE_OH_DIVISIONS+Z',
     &           'BEAM_PIPE_OH_DIVISIONS-Z'/
      DATA OHEND/'BEAM_PIPE_OH_ENDPLATE+Z',
     &           'BEAM_PIPE_OH_ENDPLATE-Z'/
      INTEGER IBMROT(30)
      REAL BMROT(30)
      EQUIVALENCE (IBMROT,BMROT)
C----------------------------------------------------------------------
      CALL GTSRCP('MAIN_RING_BEAM_PIPE(1)',BMP(1),1)   !Beam pipe stuff
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP('OH_CRACK',CRACK,1)
      CALL GTSRCP('BEAM_PIPE_ROT_MATRIX(1)',IBMROT(1),1)  !BEAM PIPE
C                                        ! LOCAL ROTATION MATRIX
      CALL GTSRCP('PI',PI,1)
      CALL GTSRCP('OH_PHI_SEGMENTS',NSEG,1)
      PHI =PI/NSEG   !Half angle
C
      CRACK=CRACK*CONV  !in cm.
      CRACKM = 0.0     !CRACK FOR MOTHER VOLUME

      ACTION = 'POS '
C
      DO 200 IZ = 1,2
C
        CALL UCTOH('MCAL',MOTHER,4,4)
        CALL ADDSTR(MOTHVL(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        NAME = LSR(3)
        MATNO = LSR(2)
        RIN = RSR(6)*CONV
        ROUT =RSR(9)*CONV
        DELZ = CONV*(RSR(5)-RSR(4))*0.5
        DELZ = ABS(DELZ)
        ZCEN = CONV*(RSR(4)+RSR(5)+RSR(7)+RSR(8))*0.25
        Z0 = ZCEN   ! Z CO-ORDINATE OF CENTER OF MODULE
        ZLO = CONV*0.5*(RSR(4)+RSR(5))   !Z CO-ORDINATE OF INNER FACE
C                                     ! CENTER
        ZHI = CONV*0.5*(RSR(7)+RSR(8))
        ZLO0 = ZLO
        ZHI0 = ZHI
C
C
C Z divisions
C
        CALL ADDSTR(ZDVNM(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        CALL WRTZDV(20,ZDVNM(IZ),LSR,RSR)   !Write out in CGS system
        NDIV = LSR(1)
        MATNO = LSR(2)
        MOTHER = IBMP(4+IZ)   !HANGING OFF Beam Pipe Mother volume
        XXMOTH = BMP(8+2*(IZ-1))
        YYMOTH = BMP(8+2*(IZ-1)+1)    !Center of Mother volume.
        RBMIN = 0.
        RBMOUT = BMP(1)*CONV
C
        MATBM = IBMP(2)     !RADIUS AND MATERIAL OF BEAM PIPE.
        XPOSBM = BMP(3)*CONV
        YPOSBM = BMP(4)*CONV    !ABSOLUTE POSITIONS OF BEAM PIPE
        XX1 = XPOSBM-XXMOTH
        ZZ1 = YPOSBM-YYMOTH      !RELATIVE TO CENTER OF TRAP
        XX = -(XX1*COS(PHI)+ZZ1*SIN(PHI))
        IF(IZ.EQ.2)XX=-XX   !The -z module is not identical to the +z one.
        ZZ = -XX1*SIN(PHI)+ZZ1*COS(PHI)      !Position of beam pipe
C                                        ! center for 5th segment in lab system
        YY = -ZZ1*TTHET           !Position of center
        NPAR = 3               !Beam pipe segmenT IS A TUBE.
        PAR(1)= RBMIN
        PAR(2) = RBMOUT
C
        IPT = 3
C
        DO 100 I = 1,NDIV
          NAME = LSR(IPT+1)
C
C RIN AND ROUT SAME AS BEFORE
C
          DELZ = CONV*(RSR(IPT+3)-RSR(IPT+2))*0.5
          DELZ = ABS(DELZ)
          PAR(3) = 2*DELZ
          ZLO = CONV*(RSR(IPT+3)+RSR(IPT+2))*0.5
          ZHI = ZHI0-ZLO0+ZLO   !SAME SHIFT AS MOTHER
          ZCEN = 0.5*(ZLO+ZHI)
          ZLO = ZLO-Z0
          ZHI = ZHI-Z0
          ZCEN = ZCEN-Z0  !Relative to mother volume
C
          IF(IZ.EQ.2)THEN
            ZLO = -ZLO
            ZHI = -ZHI
C z reflection is already taken care of by the rotation matrix.
          ENDIF
C
          CALL OH_GEOM(NAME,MOTHER,MATNO,CRACK,RIN,ROUT,DELZ,
     &  ZLO,ZHI,1,2)
          IPT = IPT + 3 + LSR(IPT+4) + 1
          IPT = IPT + LSR(IPT+1) + 1   !eta indices
          IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
C NOW PUT THE TUBE OF THE MR BEAM PIPE IN HANGING OFF THE Z DIVISIONS.
          IROT = IBMROT(2)      !BEAM PIPE ROT MATRIX NUMBER
          ICPY2 = 1
          CALL UHTOC(MOTHER,4,NAMOTH,4)
          MOTHERBM = NAME   !Name of Mother Z division that pipe hangs from
          CALL UHTOC(NAME,4,NAMEZ,4)
          ARNAME = '\ARRAY EC_MAIN_RING_PIPE'//NAMEZ(2:2)   !+/-
          CALL STRINT(ARNAME(1:25),I,ARNAME,LEN3)
          NAMEZ(1:1)='P'
          CALL UCTOH(NAMEZ,NAME,4,4)   !NAME OF PIPE
          WRITE(20,1)ARNAME,NAME,MATBM,
     &        MOTHERBM,ACTION,
     &        IROT,ICPY2,XX,YY,ZZ,NPAR,
     &        (PAR(KK),KK=1,NPAR)
    1     FORMAT(1X,A/,
     &  2X,'''',A4,'''',5X,'''TUBE''',5X,I2,5X,
     &  '''',A4,'''',5X,'''',A4,'''',/,
     &  I7,2X,I5,3F10.4,2X,I5,/,
     &  2X,3F10.4,/,' \END')
  100   CONTINUE
C
C END plates handled just as Z divisions
C
        CALL ADDSTR(OHEND(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        NDIV = LSR(1)
        MATNO = LSR(2)
        IPT = 2
C
        DO 150 I = 1,NDIV
          NAME = LSR(IPT+1)
C
C RIN AND ROUT SAME AS BEFORE
C
          DELZ = CONV*(RSR(IPT+3)-RSR(IPT+2))*0.5
          DELZ = ABS(DELZ)
          ZLO = CONV*(RSR(IPT+3)+RSR(IPT+2))*0.5
          ZHI = ZHI0-ZLO0+ZLO   !SAME SHIFT AS MOTHER
          ZCEN = 0.5*(ZLO+ZHI)
          ZLO = ZLO-Z0
          ZHI = ZHI-Z0
          ZCEN = ZCEN-Z0  !Relative to mother volume

          CALL OH_GEOM(NAME,MOTHER,MATNO,CRACK,RIN,ROUT,DELZ,
     &  ZLO,ZHI,1,2)
          IPT = IPT + 3 + LSR(IPT+4) + 1
          IPT = IPT + LSR(IPT+1) + 1   !eta indices
          IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
  150   CONTINUE
  200 CONTINUE
      WRITE(20,201)IBMROT(1),IBMROT(2),(BMROT(K),K=3,8)
  201 FORMAT(1X,'\ARRAY BEAM_PIPE_ROT_MATRIX ',/,I5,/,I5,2X,6F8.3,/,
     + '\END')
  999 RETURN
      END
