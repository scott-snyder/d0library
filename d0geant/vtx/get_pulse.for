      SUBROUTINE GET_PULSE( NREHIT,SHAPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve compressed pulse from memory and expand to
C-               fill SHAPE
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-MAY-1992   Ed Oltman
C-   Updated   3-JUN-1992   K. Wyatt Merritt  Replace LIB$GET_LUN w/ GTUNIT
C-                             We run GEANT on SGI, guys! 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NREHIT
      INTEGER MAX_PULSE,MAX_SIZE
      PARAMETER (MAX_PULSE = 5000)      ! NUMBER OF PULSES IN LIBRARY
      PARAMETER (MAX_SIZE  = 150000)    ! MAXIMUM SIZE OF LIBRARY
      REAL    SHAPE(2500)
      INTEGER LUN, NRFILE, IERR
      INTEGER NUMPULSE,LIBSIZE,INDEX(2,MAX_PULSE),I,J,START,STOP
      REAL SLOPE,T
      REAL TAB1(MAX_SIZE),TAB2(MAX_SIZE)
      LOGICAL FIRST
      LOGICAL OK
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN 
C
C ****  READ IN LIBRARY ON FIRST CALL.  CHECK TO MAKE SURE THERE'S ENOUGH ROOM
C
        FIRST = .FALSE.
        CALL GTUNIT(600,LUN,IERR)
        CALL D0RZOPEN(LUN,'PULSE_PARAM','URI',4096,OK) 
        CALL RZFILE(LUN,'VTX pulses',' ')
        CALL RZVIN(INDEX,2*MAX_PULSE,NRFILE,1,1,'D')
        NUMPULSE = INDEX(1,1)
        LIBSIZE = INDEX(2,1)
        WRITE(6,*) ' NUMBER OF PULSES IN LIBRARY       = ',NUMPULSE
        WRITE(6,*) ' TOTAL NUMBER OF POINTS IN LIBRARY = ',LIBSIZE
        IF (NUMPULSE .GT. MAX_PULSE) THEN
          WRITE(6,*) ' GET_PULSE: TOO MANY PULSES: REQUESTED, MAX = ',
     &      NUMPULSE,MAX_PULSE
          STOP
        ENDIF
        IF (LIBSIZE .GT. MAX_SIZE) THEN
          WRITE(6,*) 
     &      ' GET_PULSE: TOO MUCH SPACE NEEDED: REQUIRED, MAX = ',
     &      LIBSIZE,MAX_SIZE
          STOP
        ENDIF
        CALL RZVIN(INDEX,2*MAX_PULSE,NRFILE,2,1,'D')
        CALL RZVIN(TAB1,MAX_SIZE,NRFILE,3,1,'D')
        CALL RZVIN(TAB2,MAX_SIZE,NRFILE,4,1,'D')
      ENDIF
C
C ****  NORMAL ENTRY
C
      IF (NREHIT .GT. NUMPULSE .OR. NREHIT .LT. 1) THEN
        WRITE(6,*) ' GET_PULSE: ILLEGAL PULSE INDEX: REQUESTED, MAX = ',
     &    NREHIT,NUMPULSE
        STOP
      ENDIF
      START = INDEX(1,NREHIT)
      STOP  = START + INDEX(2,NREHIT) - 1
      SLOPE = (TAB2(START+1)-TAB2(START))/
     &        (TAB1(START+1)-TAB1(START))
      I = 0
      DO WHILE (START .LE. STOP .AND. I .LT. 2500)
        I = I + 1
        T = FLOAT(I)
        SHAPE(I) = TAB2(START) + (T-TAB1(START))*SLOPE
        IF (T .GE. TAB1(START+1)) THEN
          START = START + 1
          SLOPE = (TAB2(START+1)-TAB2(START))/
     &            (TAB1(START+1)-TAB1(START))
        ENDIF
      ENDDO
      IF (I .NE. 2500) THEN
        WRITE(6,*) ' SOMETHING IS WRONG WITH TABLE: LAST BIN = ',I
        STOP
      ENDIF
  999 RETURN
      END
