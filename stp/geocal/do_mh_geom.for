      SUBROUTINE DO_MH_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for MH geometry
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-OCT-1988   Rajendran Raja
C-   MODIFIED 20.6.89       N.A. GRAF PUT IN MH MOTHER VOLUMES
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL CONV,CRACK,RIN,ROUT,DELZ,ZCEN,CRACKM,Z0
      INTEGER IRTOF(2),IRTOFF,NAMES(2),MATNO,MOTHER(2),
     &  I,NDIV,IPT,IZ,LEN3,IFC
      INTEGER LSR(1000)
      REAL RSR(1000)
      EQUIVALENCE (LSR,RSR)
      CHARACTER*19 MOTHVL(2,2)
      CHARACTER*18 MUTRVL(2)
      CHARACTER*15 ZDVNM(2,2)
      CHARACTER*14 MHEND(2,2)
      CHARACTER*17 MHMASS(2)
      CHARACTER*15 MHGZDV(2)
      CHARACTER*32 NMSRCP
      DATA MUTRVL/'MH_MOTHER_VOLUME+Z',
     &            'MH_MOTHER_VOLUME-Z'/
      DATA MOTHVL/'MFH_MOTHER_VOLUME+Z',
     &            'MFH_MOTHER_VOLUME-Z',
     &            'MCH_MOTHER_VOLUME+Z',
     &            'MCH_MOTHER_VOLUME-Z'/
      DATA ZDVNM/'MFH_DIVISIONS+Z',
     &           'MFH_DIVISIONS-Z',
     &           'MCH_DIVISIONS+Z',
     &           'MCH_DIVISIONS-Z'/
      DATA MHEND/'MFH_ENDPLATE+Z',
     &           'MFH_ENDPLATE-Z',
     &           'MCH_ENDPLATE+Z',
     &           'MCH_ENDPLATE-Z'/
      DATA MHMASS/'MH_MASSLESS_GAP+Z',
     &            'MH_MASSLESS_GAP-Z'/
      DATA MHGZDV/'MHG_DIVISIONS+Z',
     &            'MHG_DIVISIONS-Z'/
C----------------------------------------------------------------------
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP_i('MH_FIRST_ROT_MATRIX(1)',IRTOF(1),1)
      CALL GTSRCP('MH_CRACK',CRACK,1)
      CRACK=CRACK*CONV  !in cm.
      CRACKM = 0.0     !CRACK FOR MOTHER VOLUME
C
C::: DO MH mother volumes
C
      CALL MH_MVOL
C
C now to DO MFH,MCH mother crack trapezia
C
      DO 300 IFC = 1,2  !DO FH AND CH
        DO 200 IZ = 1,2 !+ and - Z
          IRTOFF = IRTOF(IZ)
          CALL ADDSTR(MUTRVL(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
          MOTHER(1) = LSR(3)
          MOTHER(2) = LSR(3)
          CALL ADDSTR(MOTHVL(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
          NAMES(1) = LSR(3)
          NAMES(2) = LSR(4)
          MATNO = LSR(2)
          RIN = RSR(7)*CONV
          ROUT =RSR(8)*CONV
          DELZ = CONV*(RSR(6)-RSR(5))*0.5
          DELZ = ABS(DELZ)
          ZCEN = CONV*(RSR(6)+RSR(5))*0.5
          Z0 = ZCEN
C
          CALL MH_GEOM(NAMES,MOTHER,MATNO,CRACKM,RIN,ROUT,DELZ,
     &  ZCEN,IRTOFF,1)
C
C Now to put in the Z divisions
C
          CALL ADDSTR(ZDVNM(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
          CALL WRTZDV1(20,ZDVNM(IZ,IFC),LSR,RSR)   !Write out in CGS system
          NDIV = LSR(1)
          MATNO = LSR(2)
          MOTHER(1) = NAMES(1)
          MOTHER(2) = NAMES(2)   !PREVIOUS NAMES ARE NOW USED AS MOTHER
          IPT = 3
C
          DO 100 I = 1,NDIV
            NAMES(1) = LSR(IPT+1)
            NAMES(2) = LSR(IPT+2)
C
C RIN AND ROUT SAME AS BEFORE
C
            DELZ = CONV*(RSR(IPT+4)-RSR(IPT+3))*0.5
            DELZ = ABS(DELZ)
            ZCEN = CONV*(RSR(IPT+4)+RSR(IPT+3))*0.5-Z0  !relative to
C                                        !  Mother volume
            IF(IZ.EQ.2)THEN
              ZCEN = -ZCEN
C z reflection is already taken care of by the rotation matrix.
            ENDIF
C
            CALL MH_GEOM(NAMES,MOTHER,MATNO,CRACK,RIN,ROUT,DELZ,
     &  ZCEN,1,2)
            IPT = IPT + 4 + LSR(IPT+5) + 1
            IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
            IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
  100     CONTINUE
C
C now do the ENDPLATES
C
          CALL ADDSTR(MHEND(IZ,IFC),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP_i(NMSRCP,LSR(1),1)
          NDIV = LSR(1)
          MATNO = LSR(2)
          IPT = 2
C
          DO 150 I = 1,NDIV
            NAMES(1) = LSR(IPT+1)
            NAMES(2) = LSR(IPT+2)
C
C RIN AND ROUT SAME AS BEFORE
C
            DELZ = CONV*(RSR(IPT+4)-RSR(IPT+3))*0.5
            DELZ = ABS(DELZ)
            ZCEN = CONV*(RSR(IPT+4)+RSR(IPT+3))*0.5-Z0  !relative to
C                                        !  Mother volume
            IF(IZ.EQ.2)THEN
              ZCEN = -ZCEN
C z reflection is already taken care of by the rotation matrix.
            ENDIF
C
            CALL MH_GEOM(NAMES,MOTHER,MATNO,CRACK,RIN,ROUT,DELZ,
     &  ZCEN,1,2)
            IPT = IPT + 4 + LSR(IPT+5) + 1
            IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
            IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
  150     CONTINUE
  200   CONTINUE
  300 CONTINUE
C
C NOW TO DO MASSLESS GAPS
C
      DO 600 IZ = 1,2
        IRTOFF = IRTOF(IZ)
        CALL UCTOH('MCAL',MOTHER(1),4,4)
        CALL UCTOH('MCAL',MOTHER(2),4,4)
        CALL ADDSTR(MHMASS(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP_i(NMSRCP,LSR(1),1)
        NAMES(1) = LSR(3)
        NAMES(2) = LSR(4)
        MATNO = LSR(2)
        RIN = RSR(7)*CONV
        ROUT =RSR(8)*CONV
        DELZ = CONV*(RSR(6)-RSR(5))*0.5
        DELZ = ABS(DELZ)
        ZCEN = CONV*(RSR(6)+RSR(5))*0.5
C
        CALL MH_GEOM(NAMES,MOTHER,MATNO,CRACKM,RIN,ROUT,DELZ,
     &  ZCEN,IRTOFF,1)
C
C write out Massless gap Z division
C
        CALL ADDSTR(MHGZDV(IZ),'(1)',NMSRCP,LEN3)
        CALL GTSRCP_i(NMSRCP,LSR(1),1)
        CALL WRTZDV(20,MHGZDV(IZ),LSR,RSR)    !Put out the radial info for
C                                             ! massless gap
  600 CONTINUE
C
  999 RETURN
      END
