      LOGICAL FUNCTION MENCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define Calorimeter Menu 8.
C-                         Called by LUIGET
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created 01-May-1987  Stephan Linn
C-   Updated 13-SEP-1988 Rajendran Raja
C-   Updated   5-JUN-1989   Harrison B. Prosper  
C-   Made into program-builder interface function
C-   Updated  24-JUN-1991   K. Wyatt Merritt  Comment out ZCEDEX calls
C-                          for compatibility with GEANT 3.14
C-                          
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:D0COM.INC'        ! Selected MENU id and command
C
C?      INTEGER IOPT,MMENU,IER
C
C?      CHARACTER*80 TERM_NAME
C?      INTEGER   D1(2),D2
C
C----------------------------------------------------------------------
      MENCAL = .TRUE.
      IF ( NMENU .NE. IDCAL ) GOTO 999
C
      IF ( NCOMD .GT. 1 ) GOTO 999
C
      GOTO ( 100,200,300,400,500 ) NCOMD
C
  100 CONTINUE
      WRITE(LOUT,*)'EM SHOWER PARAMETERIZATION SETUP'
      WRITE(LOUT,*)'LOECUT is the low energy cutoff.'
      WRITE(LOUT,*)'HIECUT is the parameterization cutoff.'
      WRITE(LOUT,*)'X0STEP is the step size for GEANTINOS'
      WRITE(LOUT,*)'LO/HI/STEP = 0,0,0             Normal showering'
      WRITE(LOUT,*)'LO/HI/STEP = .010,.200,1.0     Parameterization'
      WRITE(LOUT,*)'Present values:'
      WRITE(LOUT,*)LOECUT,HIECUT,X0STEP
C     CALL ZCGETR('LOECUT = $',LOECUT)
C     CALL ZCGETR('HIECUT = $',HIECUT)
C     CALL ZCGETR('X0STEP = $',X0STEP)
      LOECUT_DEF = LOECUT
      HIECUT_DEF = HIECUT
      X0STEP_DEF = X0STEP
      GOTO 999
C
  200 CONTINUE
C?      CALL SAVMAP
C?      OPEN(45,FILE='GEOMETRY.DAT',STATUS='NEW',FORM='UNFORMATTED',
C?     +  ERR=201)
C?      CALL GOPEN(45,'O',0,IER)
C?      IF(IER.NE.0)GOTO 202
C?      CALL GSAVE(45,'INIT',-1,0,IER)
C?      IF(IER.NE.0)GOTO 203
C?      CLOSE(45)
C?      WRITE(LOUT,*)'Geometry and digitization map saved'
C?      GOTO 999
C? 201  WRITE(LOUT,*)'OPEN error for unit 45'
C?      GOTO 999
C? 202  WRITE(LOUT,*)'GOPEN error for unit 45'
C?      GOTO 999
C? 203  WRITE(LOUT,*)'GSAVE error for unit 45'
      GOTO 999
C
  300 CONTINUE
C?      CALL GETMAP
C?      WRITE(LOUT,*)'/DCDIGI/ filled from JRUNG'
      GOTO 999
C
  400 CONTINUE
C?      WRITE(LOUT,*)'Open VAX2000 terminal'
C?      CALL UIS$CREATE_TERMINAL('TK','GEANT',D1,TERM_NAME,D2)
C?      OPEN(10,FILE=TERM_NAME,carriage control='list',STATUS='NEW')
      GOTO 999
C
  500 CONTINUE
C?      WRITE(LOUT,*)'Close VAX2000 terminal'
C?      CLOSE(10)
      GOTO 999
C
  999 RETURN
      END
