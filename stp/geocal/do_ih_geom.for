      SUBROUTINE DO_IH_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine puts out IH SRCP constants
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  6-NOV-88   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL CONV,RIN,ROUT,DELZ,ZCEN,Z0,CRACKS(2),CRACK,ROUTC
      REAL SUPTHK,RINS
      INTEGER IRTOFF,NAME,MATNO,I,NDIV,IPT,IZ,LEN3,IFC,MOTHER,IROT
      INTEGER LSR(1000)
      REAL RSR(1000)
      EQUIVALENCE (LSR,RSR)
C
      CHARACTER*19 MOTHVL(2,2)
      CHARACTER*15 ZDVNM(2,2)
      CHARACTER*14 IHEND(2,2)
      CHARACTER*18  IHSUP(2,2)
      CHARACTER*32 NMSRCP
      CHARACTER*50 STRING
      DATA MOTHVL/'IFH_MOTHER_VOLUME+Z',
     &            'IFH_MOTHER_VOLUME-Z',
     &            'ICH_MOTHER_VOLUME+Z',
     &            'ICH_MOTHER_VOLUME-Z'/
      DATA ZDVNM/'IFH_DIVISIONS+Z',
     &           'IFH_DIVISIONS-Z',
     &           'ICH_DIVISIONS+Z',
     &           'ICH_DIVISIONS-Z'/
      DATA IHEND/'IFH_ENDPLATE+Z',
     &           'IFH_ENDPLATE-Z',
     &           'ICH_ENDPLATE+Z',
     &           'ICH_ENDPLATE-Z'/
      DATA IHSUP/'IFH_SUPPORT_PIPE+Z',
     &           'IFH_SUPPORT_PIPE-Z',
     &           'ICH_SUPPORT_PIPE+Z',
     &           'ICH_SUPPORT_PIPE-Z'/
C----------------------------------------------------------------------
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP_i('IH_FIRST_ROT_MATRIX',IRTOFF,1)
      CALL GTSRCP('IH_CRACK(1)',CRACKS(1),1)
      CRACKS(1) = CRACKS(1)*CONV
      CRACKS(2) = CRACKS(2)*CONV
      CALL GTSRCP('IH_SUPPORT_PIPE',SUPTHK,1)
      SUPTHK = SUPTHK*CONV
C
      DO 300 IFC = 1,2    !DO FH AND CH
        CRACK = CRACKS(IFC)  !Cracks are slightly different
        DO 200 IZ = 1,2
          CALL UCTOH('MCAL',MOTHER,4,4)
          CALL ADDSTR(MOTHVL(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
          NAME = LSR(3)
          MATNO = LSR(2)
          RIN = RSR(6)*CONV
          ROUT =RSR(7)*CONV
          ROUTC = ROUT-CRACK  !OUTER RADIUS FOR Z DIVISIONS
          RINS = RIN + SUPTHK !INNER RADIUS TO ACCOUNT FOR SUPPORT PIPE
          DELZ = CONV*(RSR(5)-RSR(4))*0.5
          DELZ = ABS(DELZ)
          ZCEN = CONV*(RSR(5)+RSR(4))*0.5
          Z0 = ZCEN
C
          STRING = '\ARRAY '//MOTHVL(IZ,IFC)
          IROT = IRTOFF + IZ -1    !for IZ =2 use Z reflection matrix
C
          CALL IH_GEOM(NAME,STRING,MOTHER,MATNO,RIN,ROUT,
     &        DELZ,ZCEN,IROT,1)
C
C now to do Z divisions
C
          CALL ADDSTR(ZDVNM(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
          CALL WRTZDV(20,ZDVNM(IZ,IFC),LSR,RSR)   !Write out in CGS system
          NDIV = LSR(1)
          MATNO = LSR(2)
          MOTHER = NAME
          IPT = 3
C
          DO 100 I = 1,NDIV
            NAME = LSR(IPT+1)
C
C RIN = RINS:  SAME AS MOTHER PLUS THE SUPPORT PIPE THICKNESS 
C ROUT = ROUTC WITH CRACK ALLOWED FOR
C
            DELZ = CONV*(RSR(IPT+3)-RSR(IPT+2))*0.5
            DELZ = ABS(DELZ)
            ZCEN = CONV*(RSR(IPT+3)+RSR(IPT+2))*0.5-Z0  !relative to
C                                        !  Mother volume
            IF(IZ.EQ.2)THEN
              ZCEN = -ZCEN
C z reflection is already taken care of by the rotation matrix.
            ENDIF
C
            CALL IH_GEOM(NAME,' ',MOTHER,MATNO,RINS,ROUTC,DELZ,ZCEN,1,2)
C
            IPT = IPT + 3 + LSR(IPT+4) + 1
            IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
            IPT = IPT + LSR(IPT+1) + 1   !FOR PHI DIVISIONS
C
  100     CONTINUE
C
C NOW ENDPLATES
C
          CALL ADDSTR(IHEND(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
C
          STRING = '\ARRAY '//IHEND(IZ,IFC)
          NDIV = LSR(1)
          MATNO = LSR(2)
          IPT = 2
C
C ROUT SAME AS MOTHER, RIN INCLUDES SUPPORT PIPE THICKNESS
C
          DO 150 I = 1,NDIV
            NAME = LSR(IPT+1)
            DELZ = CONV*(RSR(IPT+3)-RSR(IPT+2))*0.5
            DELZ = ABS(DELZ)
            ZCEN = CONV*(RSR(IPT+3)+RSR(IPT+2))*0.5-Z0  !relative to
C                                        !  Mother volume
            IF(IZ.EQ.2)THEN
              ZCEN = -ZCEN
C z reflection is already taken care of by the rotation matrix.
            ENDIF
C
            CALL IH_GEOM(NAME,STRING,MOTHER,MATNO,RINS,ROUT,
     &        DELZ,ZCEN,1,1)
            IPT = IPT + 3 + LSR(IPT+4) + 1
            IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
            IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
  150     CONTINUE
C
C NOW Support pipes
C
          CALL ADDSTR(IHSUP(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
C
          STRING = '\ARRAY '//IHSUP(IZ,IFC)
          NDIV = LSR(1)
          MATNO = LSR(2)
          IPT = 2
          RIN = RSR(6)*CONV
          ROUT = RSR(7)*CONV
C
          DO 160 I = 1,NDIV
            NAME = LSR(IPT+1)
            DELZ = CONV*(RSR(IPT+3)-RSR(IPT+2))*0.5
            DELZ = ABS(DELZ)
            ZCEN = CONV*(RSR(IPT+3)+RSR(IPT+2))*0.5-Z0  !relative to
C                                        !  Mother volume
            IF(IZ.EQ.2)THEN
              ZCEN = -ZCEN
C z reflection is already taken care of by the rotation matrix.
            ENDIF
C
            CALL IH_GEOM(NAME,STRING,MOTHER,MATNO,RIN,ROUT,
     &        DELZ,ZCEN,1,1)
            IPT = IPT + 3 + LSR(IPT+4) + 1
  160     CONTINUE
  200   CONTINUE
  300 CONTINUE
C
  999 RETURN
      END
