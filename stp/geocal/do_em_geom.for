      SUBROUTINE DO_EM_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine puts out EM SRCP constants
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  6-NOV-88   Rajendran Raja
C-   Updated   7-APR-1989   Steve Kahn
C-                          Add code to write out undivided module
C-                          volumes for layers 3 and 4
C-   Updated  28-APR-1989   Chip Stewart
C-                          EM_SUPPORT_PLATE IPT BUG FIXED
C----------------------------------------------------------------------

      IMPLICIT NONE
      REAL CONV,RIN,ROUT,DELZ,ZCEN,Z0
      INTEGER IRTOFF,NAME,MATNO,I,NDIV,IPT,IZ,LEN3,IFC,MOTHER,ICPY2
      INTEGER IROT,NPAR,KK
      INTEGER LSR(1000)
      REAL XX,YY,PAR(20)
      REAL RSR(500),RSR1(1000)
      EQUIVALENCE (LSR,RSR)
C
      CHARACTER*4 ACTION
      CHARACTER*18 MOTHVL(2)
      CHARACTER*7 EM_MODULES(4,2)
      CHARACTER*15 ZDVNM(4,2)
      CHARACTER*18 EMPLATE(2)
      CHARACTER*17  EMSUP(2)
      CHARACTER*32 NMSRCP
      CHARACTER*50 STRING
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
      DATA MOTHVL/'EM_MOTHER_VOLUME+Z',
     &            'EM_MOTHER_VOLUME-Z'/
      DATA EM_MODULES/'EC_EM+1',
     &                'EC_EM+2',
     &                'EC_EM+3',
     &                'EC_EM+4',
     &                'EC_EM-1',
     &                'EC_EM-2',
     &                'EC_EM-3',
     &                'EC_EM-4'/
      DATA ZDVNM/'EM1_DIVISIONS+Z',
     &           'EM2_DIVISIONS+Z',
     &           'EM3_DIVISIONS+Z',
     &           'EM4_DIVISIONS+Z',
     &           'EM1_DIVISIONS-Z',
     &           'EM2_DIVISIONS-Z',
     &           'EM3_DIVISIONS-Z',
     &           'EM4_DIVISIONS-Z'/
      DATA EMPLATE/'EM_SUPPORT_PLATE+Z',
     &             'EM_SUPPORT_PLATE-Z'/
      DATA EMSUP/'EM_SUPPORT_PIPE+Z',
     &           'EM_SUPPORT_PIPE-Z'/
C----------------------------------------------------------------------
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP('EM_FIRST_ROT_MATRIX',IRTOFF,1)
C
      DO 200 IZ = 1,2
        CALL UCTOH('MCAL',MOTHER,4,4)
        CALL ADDSTR(MOTHVL(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        NAME = LSR(3)
        MATNO = LSR(2)
        DO 201 IFC = 4,12
          RSR(IFC) = RSR(IFC)*CONV
          PAR(IFC) = RSR(IFC)
  201   CONTINUE
        DELZ = (RSR(10)-RSR(4))*0.5
        DELZ = ABS(DELZ)
        ZCEN = (RSR(10)+RSR(4))*0.5
        Z0 = ZCEN
        PAR(4) = PAR(4) - Z0
        PAR(7) = PAR(7) - Z0
        PAR(10) = PAR(10) -Z0   !relative to center of PCONE
        IF(IZ.EQ.2)THEN
          PAR(4) = -PAR(4)
          PAR(7) = -PAR(7)
          PAR(10) = -PAR(10)   !Z reflection is taken care of by rotation matrix
        ENDIF
C
        CHARR= 'MCAL'
        MOTHER = ICHARR
        ACTION = 'POS '
        IROT = IRTOFF + IZ - 1
        ICPY2 = 1
C
        NPAR = 12
        PAR(1) = 0.0
        PAR(2) = 360.0 !PHI RANGE
        PAR(3) = 3.0   !NUMBER OF CHANGES IN RADII IN PCONE
C
        STRING = '\ARRAY '//MOTHVL(IZ)
        WRITE(20,1)STRING,NAME,MATNO,
     &        MOTHER,ACTION,
     &        IROT,ICPY2,XX,YY,ZCEN,NPAR,
     &        (PAR(KK),KK=1,NPAR)
    1   FORMAT(A,/,
     &  2X,'''',A4,'''',5X,'''PCON''',5X,I2,5X,
     &  '''',A4,'''',5X,'''',A4,'''',/,
     &  I7,2X,I5,3F10.4,2X,I5,/,
     &  2X,7F10.4,/,5F10.4,/,
     &  '\END')
C
C now to do Z divisions
C
        MOTHER = NAME
C
        DO 300 IFC = 1,4
          CALL ADDSTR(ZDVNM(IFC,IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP(NMSRCP,LSR,1)
          CALL WRTZDV(20,ZDVNM(IFC,IZ),LSR,RSR)   !Write out in CGS system
          NDIV = LSR(1)
          MATNO = LSR(2)
          CALL ADDSTR(EM_MODULES(IFC,IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
          CALL GTSRCP(NMSRCP,RSR1,1)
C We are only using the EM_Module information for getting RIN and ROUT
C We are not going to define Geant modules. Rather we will let the Z
C divisions hang off the PCON mother volume
C
          RIN = CONV*RSR1(6)
          ROUT = CONV*RSR1(7)
C
C ****    PUT UNDIVIDED VOLUME FOR FLOORS 3 AND 4 OUT -- S. KAHN  7-APR-89
C
          IF(NDIV .GT. 1) THEN
             CALL UCTOH(EM_MODULES(IFC,IZ)(4:7),NAME,4,4)
             DELZ = 0.5*(RSR1(5)-RSR1(4))*CONV
             ZCEN = 0.5*(RSR1(5)+RSR1(4))*CONV - Z0
             CALL EM_GEOM(NAME,' ',MOTHER,MATNO,RIN,ROUT,DELZ,ZCEN,1,2)
          END IF
C

C
          IPT = 3
C
          DO 100 I = 1,NDIV
            NAME = LSR(IPT+1)
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
            CALL EM_GEOM(NAME,' ',MOTHER,MATNO,RIN,ROUT,DELZ,ZCEN,1,2)
            IPT = IPT + 3 + LSR(IPT+4) + 1
            IPT = IPT + LSR(IPT+1) + 1   !FOR eta indices
            IPT = IPT + LSR(IPT+1) + 1   !FOR PHI DIVISIONS
  100     CONTINUE
  300   CONTINUE
C
C NOW ENDPLATES
C
        CALL ADDSTR(EMPLATE(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
C
        STRING = '\ARRAY '//EMPLATE(IZ)
        NDIV = LSR(1)
        MATNO = LSR(2)
        IPT = 2
        RIN = CONV*RSR(6)
        ROUT = CONV*RSR(7)
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
          CALL EM_GEOM(NAME,STRING,MOTHER,MATNO,RIN,ROUT,
     &        DELZ,ZCEN,1,1)
          IPT = IPT + 3 + LSR(IPT+4)+ 1
  150   CONTINUE
C
C NOW Support pipes
C
        CALL ADDSTR(EMSUP(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
C
        STRING = '\ARRAY '//EMSUP(IZ)
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
          CALL EM_GEOM(NAME,STRING,MOTHER,MATNO,RIN,ROUT,
     &        DELZ,ZCEN,1,1)
          IPT = IPT + 3 + LSR(IPT+4)+ 1
  160   CONTINUE
  200 CONTINUE
C
  999 RETURN
      END
