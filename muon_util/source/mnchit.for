      SUBROUTINE MNCHIT( ITRAK, IPTR, XYZ, NPTR, ACTIVE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Muon scintillator matching track scan
C-                         by C-layer hit
C-                         Code support only CF octant 0-3
C-
C-   Inputs  : ITRAK       MUOT track number
C-   Outputs : IPTR(4)     matched MSCT hit pointer
C-             XYZ(3)      track hit postion in global coords.
C-             NPTR        number of matched hits
C-             ACTIVE      1 if track points to active scint., 0 otherwise
C-   Controls:
C-
C-   Created   8-FEB-1994   Atsushi Taketani
C-   Updated  11-Mar-1994   Atsushi Taketani protection against bad track
C-   Updated  12-Jun-1994   R. Markeloff  EZPICK and EZRSET calls removed
C-   Updated  04-Nov-1994   A. Ito   add octants 4,7
C-   Updated  01-Mar-1994   R. Markeloff  Initialize ACTIVE, update call
C-                          to MNACTIVE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C-- Argument
      INTEGER  ITRAK,IPTR(4), NPTR, ACTIVE
      REAL     XYZ(3,4)
C-- Local
      LOGICAL  FIRST
      REAL     WIRE_MAX
      INTEGER  GZMUOT, LMUOT, IFW1
      INTEGER  NPTRAK,IADD(40)
      REAL     X(40), Y(40), Z(40), DT
      INTEGER  I, IEDGE
      INTEGER  NMOD(40),NPLN(0:40),NWIR(40),IER
      INTEGER  IADC
      INTEGER  NPS, NPS_PART, ID
      INTEGER  ICAN(4) ,ICAN_MOD(4),NCAN
      INTEGER  IOCT
      INTEGER  IMOD, ISCN, ICAT
      REAL     CHAMBER_LENGTH(0:7)
      REAL     DTMAX, DTMIN
      REAL     TVEC(3), TPOS(3)
      REAL     XYZPLN(4,3), DEV
      INTEGER K
      INTEGER RINDEX, MUORIENT
      REAL    DTMINS(2,0:7),DTMAXS(2,0:7)
      LOGICAL MNACTIVE
C
      DATA     FIRST/.TRUE./
      DATA CHAMBER_LENGTH/553.,579.,579.,553.,419.,579.,579.,419./
C               data for octants 0-7     5,6 dummy
      DATA DTMINS/0.0,0.5,1.0,0.5,-0.5, 0.0,0.5,1.0,  0.0,-0.5,
     &  0.0,0.0,0.0,0.0, -.5,-1.0/
      DATA DTMAXS/0.5,1.0,0.5,0.0,-1.0,-0.5,0.0,0.5, -0.5,-1.0,
     &  0.0,0.0,0.0,0.0, 0.0,-0.5/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZGET( 'SCINT_C_CELL', IEDGE, IER )
        CALL EZGET( 'SCINT_C_WIRE', WIRE_MAX, IER )
      END IF
C
      NPTR = 0
      ACTIVE = 0
C
      LMUOT = GZMUOT(ITRAK)
      IFW1 = IQ(LMUOT+4)
      IF ( IFW1.EQ.3.OR.IFW1.EQ.5 ) GOTO 999
C
      CALL GTMHOT(1,ITRAK,NPTRAK,X,Y,Z,IADD)
      IF ( NPTRAK.EQ.0 ) GOTO 999
C
      IADC = 0
      NPLN(IADC) = -1
      DO 100 I=1,NPTRAK
        CALL MUADD( IADD(I), NMOD(I), NPLN(I), NWIR(I), IER )
C                           check for octant 0-4,7 PDT's
        IF ( NMOD(I).LT.200.OR.NMOD(I).GT.247 ) GOTO 100  ! CF upper C
        IF ( ABS(X(I)).GE.1000.0 ) GOTO 100
        IF ( ABS(Y(I)).GE.1000.0 ) GOTO 100
        IF ( ABS(Z(I)).GE.1000.0 ) GOTO 100
        IF ( NPLN(IADC).LT.NPLN(I) ) THEN
          IADC = I
        END IF
  100 CONTINUE
C
      IF ( IADC.EQ.0 ) GOTO 999      ! missing C layer
      IOCT = MOD(NMOD(IADC),10)
C                check for octants 5,6
      IF ( IOCT.EQ.5 .OR. IOCT.EQ.6 ) GOTO 999      ! bottom 
C
C  find in 1st scinti
C
      NPS = NWIR(IADC)/6
      NPS_PART = NWIR(IADC) - NPS*6 + 1
      RINDEX = MUORIENT(NMOD(IADC))
C
      IF (RINDEX.EQ.1) THEN
        DT = X(IADC)
      ELSE IF ( RINDEX.EQ.2 ) THEN
        DT = Y(IADC)
      ELSE 
        GOTO 999
      END IF
C
      IF ( ABS(DT).GE.CHAMBER_LENGTH(IOCT)/2.0 ) THEN
        IF ( RINDEX.EQ.1 ) THEN
          ID = 1
        ELSE
          ID = 2
        END IF
      ELSE
        IF ( RINDEX.EQ.1 ) THEN
          ID = 2
        ELSE
          ID = 1
        END IF
      END IF
C
      ISCN = NPS*2 + ID
      IMOD = NMOD(IADC)
C
C look edge of scintillator
C
      ICAT = 0
C-- in drift view
      IF ( NPS_PART.LE.IEDGE ) THEN
        IF (IOCT.LE.1 .OR. IOCT.EQ.4) THEN
          ICAT = IBSET(ICAT,1)
        ELSE
          ICAT = IBSET(ICAT,0)
        END IF
      ELSE IF ( NPS_PART.GT.6-IEDGE ) THEN
        IF (IOCT.LE.1 .OR. IOCT.EQ.4) THEN
          ICAT = IBSET(ICAT,0)
        ELSE
          ICAT = IBSET(ICAT,1)
        END IF
      END IF
C-- in wire direction
      DTMIN = DTMINS(ID,IOCT)*CHAMBER_LENGTH(IOCT) 
      DTMAX = DTMAXS(ID,IOCT)*CHAMBER_LENGTH(IOCT)
      IF      ( ABS(DTMIN-DT).LE.WIRE_MAX ) THEN
        ICAT = IBSET(ICAT,3)
      ELSE IF ( ABS(DTMAX-DT).LE.WIRE_MAX ) THEN
        ICAT = IBSET(ICAT,2)
      END IF
C
C Scan adjacent scintillator
C
      IF ( IMOD.EQ.0.OR.ISCN.EQ.0 ) THEN
        ACTIVE = 0
        NPTR = 0
        GOTO 999
      END IF
      CALL MNMODT(IMOD, ISCN, ICAT, NCAN, ICAN_MOD, ICAN )
      IF ( NCAN.EQ.0 ) THEN
        ACTIVE = 0
        NPTR = 0
        GOTO 999
      END IF
C
C scan MSCT
C
      CALL MNFIND( NCAN, ICAN_MOD, ICAN, NPTR, IPTR, XYZPLN)
C
      ACTIVE = 0
      DO I = 1,NCAN
        IF (MNACTIVE(ICAN_MOD(I))) ACTIVE = 1
      ENDDO
C
      IF ( NPTR.EQ.0 ) GOTO 999
C
C Intersection search      
C
      DO I=1,3                   ! muot BC track segment
        TPOS(I) = Q(LMUOT+10+I)
        TVEC(I) = Q(LMUOT+16+I)
      END DO
C
      DO 200 I=1,NPTR
        IF ( XYZPLN(I,1).NE.0.0 ) THEN
          DEV      = (XYZPLN(I,1) - TPOS(1))/TVEC(1)
        ELSE IF ( XYZPLN(I,2).NE.0.0 ) THEN
          DEV      = (XYZPLN(I,2) - TPOS(2))/TVEC(2)
        END IF
        DO K=1,3
          XYZ(K,I) = TPOS(K) + TVEC(K)*DEV
        END DO
  200 CONTINUE
C
  999 RETURN
      END
