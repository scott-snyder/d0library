      PROGRAM Q_SOLVE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Queueing Theory Solutions for Farms or Buffers
C-
C-   Created  12-JUN-1993   James T. Linnemann
C-   Updated  16-AUG-1995   James T. Linnemann  extend to M/M/c/K
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER P,B,I,N0,NMAX,LUN,CONINT
      CHARACTER*16 INP
      CHARACTER*1 LOOP,SOLVE
      CHARACTER*21 RU_TAG 
      LOGICAL PER_SERVER
      REAL R,R_OR_U,PDEAD,DPCT_DEAD,DR,CONREA
      PARAMETER (LUN = 20)
      PARAMETER( DPCT_DEAD = 1.0 )
      PARAMETER( DR = .05 )
C----------------------------------------------------------------------
   10 CONTINUE
C...reset defaults each time through
      LOOP = ' '
      SOLVE = ' '
      P = 1
      B = 0
      R_OR_U = 0
      R = 0
      RU_TAG = ' Erlangs/processor = '
      PER_SERVER = .FALSE.
      WRITE(6,*)' ? means solve for, * means do various values of'
C...Processors
      WRITE(6,*)' Give # Processors'
      READ (*,405)INP
  405 FORMAT(A16)
      IF (INP(1:1).EQ.'?') THEN
        SOLVE = 'P'
      ELSEIF (INP(1:1).EQ.'*') THEN
        LOOP = 'P'
      ELSE
        P = CONINT(INP)
      ENDIF
C...Buffers
      WRITE(6,*)
     &  ' Give # Extra Buffers to hold events outside processors'
      READ (*,405)INP
      IF (INP(1:1).EQ.'?') THEN
        SOLVE = 'B'
      ELSEIF (INP(1:1).EQ.'*') THEN
        LOOP = 'B'
      ELSE
        B = MAX(CONINT(INP),0)
      ENDIF
C...traffic
   20 CONTINUE
      WRITE(6,*)' Give traffic intensity:'
      WRITE(6,*)'   ? = solve for R'
      WRITE(6,*)'   * = loop over R'
      WRITE(6,*)'   u = specify total traffic intensity'
      WRITE(6,*)'   R = specify traffic intensity/processor'
      READ (*,405)INP
      IF (INP(1:1).EQ.'?') THEN
        SOLVE = 'R'
      ELSEIF (INP(1:1).EQ.'*') THEN
        LOOP = 'R'
      ELSEIF (INP(1:1).EQ.'U'.OR.INP(1:1).EQ.'u') THEN
        WRITE(6,*)' give u = total traffic intensity (erlangs) '
        WRITE(6,*)'        = Total Input Hz * <Tservice(1 processor)>'
        READ (*,405)INP
        R_OR_U = CONREA(INP)
        R = R_OR_U/P
        PER_SERVER = .FALSE.
        RU_TAG = ' Erlangs  total    = '
      ELSEIF (INP(1:1).EQ.'R'.OR.INP(1:1).EQ.'r') THEN
        WRITE(6,*)
     &    ' give R = traffic intensity/processor (erlangs) '
        WRITE(6,*)
     &    '        = Input Hz/processor * <Tservice(1 processor)>'
        READ (*,405)INP
        R_OR_U = CONREA(INP)
        R = R_OR_U
        PER_SERVER = .TRUE.
        RU_TAG = ' Erlangs/processor = '
      ELSE
        GO TO 20  !command not recognized
      ENDIF
C...Deadtime
      IF ((SOLVE.EQ.' ').OR.(SOLVE.EQ.'D')) THEN
        SOLVE = 'D'
        PDEAD = 0
      ELSE
   35   CONTINUE
        WRITE(6,*)' Give Desired Deadtime in %'
        READ (*,405)INP
        IF (INP(1:1).EQ.'?') THEN
          WRITE(6,*)' Already solving for ',SOLVE
          GO TO 35
        ELSEIF (INP(1:1).EQ.'*') THEN
          LOOP = 'D'
        ELSE
          PDEAD = CONREA(INP)
          PDEAD = PDEAD/100.
        ENDIF
      ENDIF
      WRITE (6,200) SOLVE,LOOP,DPCT_DEAD,DR,P,B,RU_TAG,R,100.*PDEAD
      WRITE (LUN,200)SOLVE,LOOP,DPCT_DEAD,DR,P,B,RU_TAG,R,100.*PDEAD
  200 FORMAT (' Solve For ',A,'.  Loop over ',A,'.'/
     &  ' (P=Processors, B = Buffers, D = Dead %'
     &  ', R = Erlangs/processor)',
     &  /5X,' Loop steps: P,B by 1, D by ',F5.2,'%, R by ',F5.2
     &  /' Inputs:  P = ',I5,' B = ',I5,A21,
     &  F5.2,' Dead % = ',F5.2)
      CALL Q_WRITE_HEADINGS(6)
      CALL Q_WRITE_HEADINGS(LUN)
      IF (P.EQ.0) P = 1
      R = R_OR_U/P
      IF(PER_SERVER) R = R_OR_U !have to redo for each value of P
      IF (LOOP.NE.' ') THEN
        IF ( LOOP.EQ.'P' ) THEN
          N0 = 1
          NMAX = 100
          DO P = N0,NMAX
            R = R_OR_U/P
            IF(PER_SERVER) R = R_OR_U !have to redo for each value of P
            CALL Q_GET_UNKNOWNS(LUN,P,B,R,PDEAD,SOLVE,PER_SERVER)
          ENDDO
        ELSEIF ( LOOP.EQ.'B' ) THEN
          N0 = 0
          NMAX = 100
          DO B = N0,NMAX
            CALL Q_GET_UNKNOWNS(LUN,P,B,R,PDEAD,SOLVE,PER_SERVER)
          ENDDO
        ELSEIF (LOOP.EQ.'R') THEN
          DO I = 1,50
            R = DR*I
            CALL Q_GET_UNKNOWNS(LUN,P,B,R,PDEAD,SOLVE,PER_SERVER)
          ENDDO
        ELSE    !loop over deadtime
          DO I = 1,50
            PDEAD = (DPCT_DEAD*I)/100.
            CALL Q_GET_UNKNOWNS(LUN,P,B,R,PDEAD,SOLVE,PER_SERVER)
          ENDDO
        ENDIF
      ELSE        !solving for single value (no loop)
        CALL Q_GET_UNKNOWNS(LUN,P,B,R,PDEAD,SOLVE,PER_SERVER)
      ENDIF
      CALL Q_WRITE_HEADINGS(6)
      GO TO 10
      END
