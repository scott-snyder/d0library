      SUBROUTINE DO_OH_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for OH geometry
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  8-NOV-1988   Rajendran Raja
C-   Modified 20-Jun-1989  N.A. Graf Put in mother volumes OCM+/-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL CONV,CRACK,RIN,ROUT,DELZ,ZCEN,ZLO,ZHI,CRACKM,Z0,ZLO0,ZHI0
      INTEGER IRTOFF,IRTOF(2),NAME,MATNO,MOTHER,I,NDIV,IPT,IZ,LEN3
      INTEGER LSR(1000)
      REAL RSR(1000),TTHET
      EQUIVALENCE (LSR,RSR)
      CHARACTER*18 MOTHVL(2)
      CHARACTER*17 MUTRVL(2)
      CHARACTER*14 ZDVNM(2)
      CHARACTER*13 OHEND(2)
      CHARACTER*17 OHMASS(2)
      CHARACTER*15 OHGZDV(2)
      CHARACTER*32 NMSRCP
      DATA MUTRVL/'OH_BIG_MVOLUME+Z',
     &            'OH_BIG_MVOLUME-Z'/
      DATA MOTHVL/'OH_MOTHER_VOLUME+Z',
     &            'OH_MOTHER_VOLUME-Z'/
      DATA ZDVNM/'OH_DIVISIONS+Z',
     &           'OH_DIVISIONS-Z'/
      DATA OHEND/'OH_ENDPLATE+Z',
     &           'OH_ENDPLATE-Z'/
      DATA OHMASS/'OH_MASSLESS_GAP+Z',
     &            'OH_MASSLESS_GAP-Z'/
      DATA OHGZDV/'OHG_DIVISIONS+Z',    !Massless gap Z divisions
     &            'OHG_DIVISIONS-Z'/
C----------------------------------------------------------------------
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP('OH_FIRST_ROT_MATRIX(1)',IRTOF(1),1)
      CALL GTSRCP('OH_CRACK',CRACK,1)
      CRACK=CRACK*CONV  !in cm.
      CRACKM = 0.0     !CRACK FOR MOTHER VOLUME
C
C::: DO the mother OH volumes
C
       CALL OH_MVOL
C
C now to DO OH mother crack trapezia
C
      DO 200 IZ = 1,2
        CALL ADDSTR(MUTRVL(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        MOTHER = LSR(3)
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
        TTHET = (RSR(4)-RSR(7))*CONV
        TTHET = ABS(TTHET)/(ROUT-RIN)  !Tangent of angle
C
        CALL OH_GEOM(NAME,MOTHER,MATNO,CRACKM,RIN,ROUT,DELZ,
     &  ZLO,ZHI,IRTOF(IZ),1)
C
C Z divisions
C
        CALL ADDSTR(ZDVNM(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        CALL WRTZDV(20,ZDVNM(IZ),LSR,RSR)   !Write out in CGS system
        NDIV = LSR(1)
        MATNO = LSR(2)
        MOTHER = NAME   !HANGING OFF THE PREVIOUS MOTHER VOLUME
        IPT = 3
C
        DO 100 I = 1,NDIV
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
          IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
          IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
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
          IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
          IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
  150   CONTINUE
  200 CONTINUE
C
C NOW TO DO MASSLESS GAPS
C
      DO 600 IZ = 1,2
        CALL UCTOH('MCAL',MOTHER,4,4)
        CALL ADDSTR(OHMASS(IZ),'(1)',NMSRCP,LEN3)
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
C
        CALL OH_GEOM(NAME,MOTHER,MATNO,CRACKM,RIN,ROUT,DELZ,
     &  ZLO,ZHI,IRTOF(IZ),1)
        CALL ADDSTR(OHGZDV(IZ),'(1)',NMSRCP,LEN3)
        CALL GTSRCP(NMSRCP,LSR,1)
        CALL WRTZDV(20,OHGZDV(IZ),LSR,RSR)    !Put out the radial info for
C                                        ! massless gap
  600 CONTINUE
C
      CALL BMPIPE(TTHET)    !PUT IN MAIN RING BEAM PIPE Z DIVISIONS
C
  999 RETURN
      END
