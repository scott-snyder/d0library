      SUBROUTINE GETLUM_CORR(VAXTIME,LUM,AGE,ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in ascii luminosity file; using that info,
C-                         return the instantaneous luminosity for a given
C-                         VAX time (available in the head bank).
C-
C-   Inputs  : VAXTIME(2) : 64-bit vax time
C-                If VAXTIME(1) = VAXTIME(2) = 0, the VAXTIME will be taken from
C-                the HEAD bank.
C-   Outputs : LUM(2) : luminosity, corrected for multiple interactions
C-                LUM(1) = last reading before VAXTIME
C-                LUM(2) = next reading after VAXTIME
C-             AGE(2) : time separation between VAXTIME and surrounding readings
C-                      (hours)
C-                AGE(1) = time between last reading and VAXTIME
C-                AGE(2) = time between VAXTIME and next reading
C-             ERR : return code
C-                    0 = ok
C-                   -1 = illegal VAX time, or nonexistent HEAD bank
C-                   -2 = requested time is too early (before run 1a)
C-                   -3 = requested time is too late (after run 1b feb shutdown)
C-                   -4 = requested time is between run1a and run1b
C-
C-   Created  26-MAY-1993   Peter Grudberg based on VTX_GETLUM
C-   Updated  21-FEB-1995   Peter Grudberg  Add run 1b 
C-   Updated  28-MAR-1995   Peter Grudberg  Change LUM_AREA logical to
C-                          D0$PHYSICS_UTIL$DATA
C-   Updated  14-OCT-1995   Jeffrey Bantly  add the D0LUMC to D0LMSD correction 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      REAL LUM(2), AGE(2)
      INTEGER VAXTIME(2), ERR
C
      INTEGER MAX_READINGS, N_PERIOD
      PARAMETER ( MAX_READINGS = 30000 )
      PARAMETER ( N_PERIOD = 2 )
      REAL LUMIN(MAX_READINGS,N_PERIOD), TIME_FRAC, D3UXH
      INTEGER TIME(MAX_READINGS,N_PERIOD)
      INTEGER IR, LUN, IUSER, IER, DBL3TIME, I, VTIME(2)
      INTEGER LAST_READING(N_PERIOD), FIRST_TIME(N_PERIOD) 
      INTEGER LAST_TIME(N_PERIOD), TIME_DIFF(N_PERIOD)
      REAL A0,A1,A2,X,LUMTMP
C
      PARAMETER(A0= 0.89912)
      PARAMETER(A1= 0.64451E-02)
      PARAMETER(A2= -0.60681E-03)
C
      CHARACTER*50 FILENAME(N_PERIOD)
      LOGICAL FIRST, OPENED, D3UPT
      DATA FILENAME 
     &  / 'D0$PHYSICS_UTIL$DATA:LUM_RUN1A.DAT', 
     &    'D0$PHYSICS_UTIL$DATA:LUM_RUN1B_1.DAT' /
      DATA IUSER / 666 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Read in file with luminosity info
C
        CALL GTUNIT(IUSER,LUN,IER)
        DO I = 1, N_PERIOD
          CALL D0OPEN(LUN,FILENAME(I),'IF',OPENED)
          IF ( .NOT. OPENED ) THEN
            CALL ERRMSG('File open error', 'GETLUM',
     &        'Error opening luminosity file','F')
          ENDIF
C
          IR = 1
    1     READ(LUN,*,END=2) TIME(IR,I), LUMIN(IR,I)
          IR = IR + 1
          GO TO 1
    2     CONTINUE
          LAST_READING(I) = IR - 1
          FIRST_TIME(I) = TIME(1,I)
          LAST_TIME(I) = TIME(LAST_READING(I),I)
          TIME_DIFF(I) = LAST_TIME(I) - FIRST_TIME(I)
C
          CLOSE(LUN)
        ENDDO
        CALL RLUNIT(IUSER,LUN,IER)
C
      ENDIF
C
C ****  Normal entry:  first initialize return variables
C
      DO I = 1, 2
        LUM(I) = 0.
        AGE(I) = 0.
      ENDDO
      ERR = 0
C
C ****  Handle VAXTIME:  get it from HEAD bank if inputs are 0, or if the input
C ****  VAXTIME is illegal.  Don't overwrite the VAXTIME input.
C
      IF ( (VAXTIME(1).EQ.0 .AND. VAXTIME(2).EQ.0) .OR.
     &     .NOT. D3UPT(VAXTIME,DBL3TIME) ) THEN
        IF ( LHEAD .GT. 0 ) THEN
          VTIME(1) = IQ(LHEAD+4)
          VTIME(2) = IQ(LHEAD+5)
        ELSE
          ERR = -1
          CALL ERRMSG('No lum value', 'GETLUM',
     &      'No HEAD bank, no VAXTIME', 'W')
          GO TO 999
        ENDIF
        IF ( .NOT. D3UPT(VTIME,DBL3TIME) ) THEN
          ERR = - 1
          CALL ERRMSG('No lum value', 'GETLUM',
     &      'Illegal VAX time', 'W')
          GO TO 999
        ENDIF
      ENDIF
C
      IF ( DBL3TIME .LT. FIRST_TIME(1) ) THEN
        ERR = - 2
        CALL ERRMSG('No lum value', 'GETLUM',
     &    'Input time too early (before run 1a)', 'W')
        GO TO 999
      ELSEIF ( DBL3TIME .GE. FIRST_TIME(1) .AND. 
     &         DBL3TIME .LE. LAST_TIME(1) ) THEN
        I = 1                                 ! Run 1a
      ELSEIF ( DBL3TIME .GT. LAST_TIME(1) .AND. 
     &         DBL3TIME .LT. FIRST_TIME(2) ) THEN
        ERR = - 4
        CALL ERRMSG('No lum value', 'GETLUM',
     &    'Input time between run 1a and 1b', 'W')
        GO TO 999
      ELSEIF ( DBL3TIME .GE. FIRST_TIME(2) .AND. 
     &         DBL3TIME .LE. LAST_TIME(2) ) THEN
        I = 2                                 ! Run 1b, pre feb-1995
      ELSEIF ( DBL3TIME .GT. LAST_TIME(2) ) THEN
        ERR = - 3
        CALL ERRMSG('No lum value', 'GETLUM',
     &    'Input time too late (after run 1b feb sd)', 'W')
        GO TO 999
      ENDIF
C
C ****  Estimate the proper reading by interpolating DBL3TIME between FIRST_TIME
C ****  and LAST_TIME
C
      TIME_FRAC = FLOAT(DBL3TIME - FIRST_TIME(I)) / FLOAT(TIME_DIFF(I))
      IR = TIME_FRAC * LAST_READING(I)
      DO WHILE ( DBL3TIME .GT. TIME(IR,I) )
        IR = MIN(IR+10,LAST_READING(I))
      ENDDO
      DO WHILE ( DBL3TIME .LT. TIME(IR,I) .AND. IR .GT. 0 )
        IR = IR - 1
      ENDDO
      LUM(1) = LUMIN(IR,I)
      LUM(2) = LUMIN(IR+1,I)
      AGE(1) = D3UXH(DBL3TIME,TIME(IR,I))
      AGE(2) = D3UXH(TIME(IR+1,I),DBL3TIME)
C
C **** Correct FASTZ luminosity from D0LUMC to the more accurate D0LMSD.
C **** Correction is flat ~+10% until ~12e30 where it starts to diverge
C **** Everything is in units of E30 (input & output).   See D0LMSD.FOR.
C
      DO I=1,2
        X=LUM(I)
        IF(X.LE.0.0.OR.X.GT.35.0) THEN
          LUMTMP=X
        ELSE
          LUMTMP=(1.0/(A0+A1*X+A2*(X**2)))*X
        ENDIF
        LUM(I)=LUMTMP
      ENDDO
C
  999 RETURN
      END
