      SUBROUTINE VXY_BEAM(RUNNUM,XIP,DXIP,YIP,DYIP,ISTATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a Run number, it returns the beam
C-            position.
C-
C-   Inputs  :
C-             RUNNUM : run number
C-   Outputs :
C-             XIP    : X position of the beam (in cm)
C-             DXIP   : error of X
C-             YIP    : Y position of the beam
C-             DYIP   : error of Y
C-             ISTATUS: 0, if this run number has exact beam position
C-                      1, beam position is calculated by interpolation
C-                         between existing points
C-                      2, default beam position
C-   Controls:
C-
C-   Created  22-JAN-1993   M. Pang
C-   Updated  23-FEB-1993   Alexandre Zinchenko - many modifications
C-   Updated  15-MAR-1993   A.Zinchenko - add DXB, DYB, DXDZ, DYDZ
C-   Updated  24-AUG-1993   M. Pang give x=0. y=0. for MC
C-   Updated  24-AUG-1993   Liang-ping Chen use XYBEAM_FILE for release,
C-                          eliminate the dummy Z0, DXDZ, DYDZ,
C-                          set minimun of DXB, DYB at 6.E-3
C-   Updated   6-OCT-1993   Ed Oltman CHECK TO SEE IF RUN NUMBER HAS CHANGED..
C-   Updated  14-OCT-1993   Liang-ping Chen  Check existance of VTRAKS_RCP
C_                          before calling EZPICK
C-   Updated  29-OCT-1993   Liang-ping Chen use local varible to preserve the
C-                          information for secodary calls for the same run
C-   Updated  22-APR-1994   Liang-ping Chen  set minimun of DXB, DYB at 0.1
C-                          for runnum.gt.75332, the last run available
C-                          in XYBEAM_1.DAT for RECO V12.11 
C-   Updated  28-JUN-1994   Norman A. Graf  Modify defaults to return
C-                                          (-.3,.2) with errors 1mm.
C-   Updated  26-AUG-1994   Justin Bendich (notes by Liang-ping Chen)   
C-                          return beam XY infor. from VERT if it exists
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER DBBit
      PARAMETER (DBBit = 23)
      LOGICAL OK, FIRST, FSKIP, EOF
      INTEGER MXENTRY, ISHUTD
      PARAMETER (MXENTRY = 2000)
      PARAMETER (ISHUTD = 7) !?
      INTEGER NSHUTD(ISHUTD)
      REAL X(MXENTRY), Y(MXENTRY), DX(MXENTRY), DY(MXENTRY)
      REAL ZPOS(MXENTRY), DXZ(MXENTRY), DYZ(MXENTRY)
      REAL X_BEAM, Y_BEAM, PEAKM, AX, AY, DRUN, DXB, DYB
      REAL XIP,YIP,DXIP,DYIP
      INTEGER RUN(MXENTRY), RUNNUM, NOLDR, NNEWR
      INTEGER I, N_ENTRY, LUN0, LUN1, IP, J, INT, INT1, IZ
      INTEGER STATUS, IUSER, IERR, LRCP
      INTEGER ISTATUS, GZVERT, LVERT
      INTEGER LEN0, LEN1, IER , LAST_RUN
      CHARACTER*80 FILENAME0, FILENAME1, STRING
      CHARACTER*(*) RCPFIL
      PARAMETER( RCPFIL = 'VTRAKS_RCP' )
      DATA IUSER/777/, IP/0/, PEAKM/75./
      DATA FIRST/.TRUE./, EOF/.FALSE./, N_ENTRY/0/
      DATA NSHUTD /43244,47297,51745,54610,54623,59443,60782/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF ( IQ(LHEAD + 1) .GT. 1000 ) THEN     ! MC DATA
          X_BEAM = 0.
	  Y_BEAM = 0.
          DXB= 6.E-3  
          DYB= 6.E-3  
          STATUS = 0
 	  GOTO 1999
        END IF
        LAST_RUN = -1
        CALL EZLOC('VTRAKS_RCP',LRCP)
        IF (LRCP .EQ. 0) THEN
          CALL INRCP(RCPFIL,IER)
        ENDIF
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGETS('XYBEAM_FILE0',1,FILENAME0, LEN0,IER)
        CALL EZGETS('XYBEAM_FILE1',1,FILENAME1, LEN1,IER)
        CALL EZRSET
        CALL GTUNIT(IUSER,  LUN0,IERR)
        CALL GTUNIT(IUSER,  LUN1,IERR)
        CALL D0OPEN(LUN0,FILENAME0(1:LEN0), 'IF',OK)
        CALL D0OPEN(LUN1,FILENAME1(1:LEN1),'IF',OK)
        IF (.NOT.OK) THEN
          CALL ERRMSG('No_beam_position_file','XY_BEAM',
     &                'Beam position file is not found','W')
          X_BEAM = -0.3
          DXB = .1
          Y_BEAM = 0.2
          DYB = .1
          STATUS = 2
          GO TO 1999
        ENDIF
        I = 1
        CALL VZERO(X,MXENTRY)
        CALL VZERO(Y,MXENTRY)
        CALL VZERO(DX,MXENTRY)
        CALL VZERO(DY,MXENTRY)
        CALL VZERO(ZPOS,MXENTRY)
        CALL VZERO(DXZ,MXENTRY)
        CALL VZERO(DYZ,MXENTRY)
C
C ****  Read file with off-line reconstructed beam positions
C
        NOLDR = 0
  100   IF (I.GT.MXENTRY) THEN
          CALL ERRMSG('Too_many_data_points','XY_BEAM',
     &                'The rest of data will be ignored','W')
          GO TO 200
        ENDIF
        READ(LUN0,'(13X,I5,30X,F6.4,9X,F6.4)',END=150)
     &    RUN(I),X(I),Y(I)
        I = I + 1
        GOTO 100
C
C ****  Read file with EXAMINE-found beam positions
C
  150   FSKIP = .FALSE.
        DXB = 6.E-3
        DYB = 6.E-3
  155   READ(LUN1,'(A80)',END=170) STRING
        IF (STRING(14:18).EQ.'Layer') THEN ! old format
          READ(STRING(6:11),'(I6.6)') NNEWR
          READ(STRING(29:41),'(E13.6)') X_BEAM
          READ(STRING(50:62),'(E13.6)') Y_BEAM
          IF (ABS(X_BEAM).LT.0.001.AND.ABS(Y_BEAM).LT.0.001)
     &        FSKIP = .TRUE.
        ELSE IF(STRING(13:16).EQ.'Lay3') THEN ! new format
          READ(STRING(5:10),'(I6.6)') NNEWR
          READ(STRING(23:28),'(F6.1)') AX
          READ(STRING(34:44),'(E11.4)') X_BEAM
          READ(STRING(51:56),'(F6.1)') AY
          READ(STRING(62:72),'(E11.4)') Y_BEAM
          IF (AX.LT.PEAKM.OR.AY.LT.PEAKM) FSKIP = .TRUE.
        ELSE ! newer format
          IF (STRING(2:4).EQ.'Run') THEN ! get run number
            READ(STRING(5:10),'(I6.6)') NNEWR
            GO TO 155                    ! go back to read the second line
                                         ! of the same record with this format
          ELSE IF (STRING(2:3).EQ.'Ax') THEN ! get beam position
            READ(STRING(6:11),'(F6.1)') AX
            READ(STRING(17:27),'(E11.4)') X_BEAM
            READ(STRING(30:39),'(E10.4)') DXB
            READ(STRING(46:51),'(F6.1)') AY
            READ(STRING(57:67),'(E11.4)') Y_BEAM
            READ(STRING(70:79),'(E10.4)') DYB
          ELSE ! get beam slope
            IZ = IZ + 1
            GO TO 155
          ENDIF
          IF (AX.LT.PEAKM.OR.AY.LT.PEAKM) FSKIP = .TRUE.
        ENDIF
        IF (FSKIP) GO TO 150 ! skip runs with low statistics
  160   CONTINUE
C      save data for previous run
        IF ((NNEWR.NE.NOLDR.AND.IP.NE.0).OR.EOF) THEN
          X(I) = X(I)/IP
          Y(I) = Y(I)/IP
          DX(I) = SQRT(DX(I))/IP
          DY(I) = SQRT(DY(I))/IP
          IF (IZ.NE.0) THEN
            ZPOS(I) = ZPOS(I)/IZ
            DXZ(I) = DXZ(I)/IZ
            DYZ(I) = DYZ(I)/IZ
          ENDIF
          RUN(I) = NOLDR
          IP = 0
          IZ = 0
          I = I + 1
        ENDIF
        IF (EOF) GO TO 200
        NOLDR = NNEWR
        IF (I.GT.MXENTRY) THEN
          CALL ERRMSG('Too_many_data_points','XY_BEAM',
     &                'The rest of data will be ignored','W')
          GO TO 200
        ENDIF
        IP = IP + 1
        X(I) = X(I) + X_BEAM
        Y(I) = Y(I) + Y_BEAM
        DX(I) = DX(I) + DXB**2
        DY(I) = DY(I) + DYB**2
        GO TO 150
  170   EOF = .TRUE.
        GO TO 160
  200   CONTINUE
        N_ENTRY = I - 1
        CLOSE(LUN0)
        CLOSE(LUN1)
        CALL RLUNIT(IUSER,LUN0,IERR)
        CALL RLUNIT(IUSER,LUN1,IERR)
      END IF ! end of FIRST pass
C	write(77,'(i6,1x,2g13.7)') (run(i),x(i),y(i),i=1,n_entry)
C
C ****  Find beam position
C
      LVERT = GZVERT(1)                     ! Take vertex #1
      IF(LVERT .GT. 0) THEN
        IF(BTEST(IQ(LVERT + 2), DBBit)) THEN
          X_BEAM = Q(LVERT + 3)
          Y_BEAM = Q(LVERT + 4)
          DXB = Q(LVERT + 6)
          DYB = Q(LVERT + 7)
          GOTO 1999
        ENDIF
      ENDIF
      IF (RUNNUM .EQ. LAST_RUN) GO TO 1999
      LAST_RUN = RUNNUM
      IF (N_ENTRY.EQ.0) GO TO 1999
      DO 10 I = 1, N_ENTRY
        IF ( RUNNUM .EQ. RUN(I) ) THEN ! exact beam position
          X_BEAM = X(I)		! IN cm UNIT
          DXB = DX(I)
          Y_BEAM = Y(I)
          DYB = DY(I)
          STATUS = 0
          GOTO 999
        ENDIF
        IF (RUNNUM.GT.RUN(I) .AND. I.LT.N_ENTRY) GO TO 10
        INT = 1
        INT1 = 0
        IF (I.EQ.1 .OR. RUNNUM.GT.RUN(N_ENTRY)) THEN
          INT = 0
          GO TO 20
        ENDIF
        DO J = 1, ISHUTD ! check if it is a "shutdown" point
          IF (RUN(I).GT.NSHUTD(J).AND.RUN(I-1).LE.NSHUTD(J)) THEN
            INT = 0
            IF (RUNNUM.LE.NSHUTD(J)) INT1 = 1
            GO TO 20
          ENDIF
        END DO
   20   IF (INT.EQ.0) THEN
          DRUN = 0.
        ELSE
          DRUN = (FLOAT(RUNNUM)-RUN(I))/(RUN(I)-RUN(I-INT))
        ENDIF
        X_BEAM = X(I-INT1) + DRUN*(X(I)-X(I-INT))
        DXB = AMAX1 (DX(I-INT1),DX(I),DX(I-INT))
        Y_BEAM = Y(I-INT1) + DRUN*(Y(I)-Y(I-INT))
        DYB = AMAX1 (DY(I-INT1),DY(I),DY(I-INT))
        STATUS = 1
        GOTO 999

   10 CONTINUE
C----------------------------------------------------------------------
  999 CONTINUE

      IF ( RUNNUM .LT. 63375 ) THEN
        X_BEAM = X_BEAM + 0.0535
        Y_BEAM = Y_BEAM + 0.0154
      END IF
      IF(RUNNUM .GT. 75332) THEN
        DXB= AMAX1(.1, DXB)
        DYB= AMAX1(.1, DYB)
      ELSE
        DXB= AMAX1(6.E-3, DXB)
        DYB= AMAX1(6.E-3, DYB)
      ENDIF
 1999 CONTINUE
      XIP=X_BEAM
      YIP=Y_BEAM
      DXIP=DXB
      DYIP=DYB
      ISTATUS=STATUS
C
      RETURN
      END
