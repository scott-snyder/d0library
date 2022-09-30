      LOGICAL FUNCTION STPLV0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handles stepping through the LV0 detectors.
C-        HITS saved for each track through each tile:
C-          HITS(1) .... TOF (from GCTRAK's TOFG) in seconds
C-          HITS(2) .... DEVOL (energy loss through LV0 from DESTEP) in GeV
C-        DEVOL summed for each tile by GSCHIT with NHSUM=1, useful in the
C-        event that a track exits a tile and then re-enters (but unlikely
C-        with no magnetic field).
C-        TOF is stored only when a track enters the volume.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated  24-FEB-1989   Chip Stewart, Harrison B. Prosper
C-                          Now does something useful!
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER CELL(2,5,9,4), CHANNEL, CHANNELL
      INTEGER IZ, SLATNUM, DIVNUM, COPYNUM
      CHARACTER*4 DET
      EQUIVALENCE ( NAMES(5),DET  )
      LOGICAL FIRST
      SAVE FIRST
      REAL DEVOL,TOFVOL
c      REAL    HITS(2)
      REAL    ZV,DZ,DIST
      REAL    R,THETA,PHI,ETA
      REAL    TIME, POINT
      REAL    FACTOR1, FACTOR2
      LOGICAL USE
C
      INCLUDE 'D0$INC:LV0PARAM.INC'
      INCLUDE 'D0$INC:GCSETS.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:GCKING.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCVOLU.INC'
c      DATA NHSUM/1/                     ! Sum last hit element only (Energy)
      DATA FIRST /.TRUE./
      IF ( FIRST ) THEN
        FACTOR1 = 0.00000000003         ! will divide time by 30 ps
        FACTOR2 = 100000                ! will multiply energy by 1E5
        CALL VZERO (CELL,360)
        CALL VZERO_i (INFO,216)
        IF ( SLV0(3).EQ.1.0 ) THEN
          CALL L0_RUN1A_SETUP(CELL)
        ELSEIF ( SLV0(3).EQ.2.0) THEN 
          CALL L0_RUN1B_SETUP(CELL)
        ELSE
          CALL ERRMSG('MCLEVEL0 Run 1b Geometry defaulted','STPLV0',
     &      'Defaulting to Run 1b Geometry because none chosen','W')
          CALL L0_RUN1B_SETUP(CELL)
        ENDIF
        FIRST = .FALSE.
      ENDIF
C
C----------------------------------------------------------------------
      STPLV0 = .TRUE.
      IF ( DLV0 .LT. 2 ) GOTO 999
C
C ****  Should we be here?
C
      IF ( DLV0.LE.1 ) GOTO 999
      IF ( INT(IDTYPE/1000).NE.5 ) GOTO 999
C
C ****  NOW DO STEPPING
      USE = (ITRTYP .EQ. 2) .OR.        ! Electrons
     &      (ITRTYP .EQ. 4) .OR.        ! Hadrons
     &      (ITRTYP .EQ. 5)             ! Muons
C
      IF ( INWVOL.EQ.1 ) THEN           ! FIRST STEP IN THIS VOLUME
        DEVOL = 0.                      ! RESET LV0 DEDX
        IF ( USE ) THEN
          IF ( NLEVEL.EQ.5 ) THEN
C
C ****  Fetch iz, copy and division number
C
            IF (INDEX(DET,'+').GT.0) THEN
              IZ = 1
            ELSE
              IZ = 2
            ENDIF
            COPYNUM=NUMBER(4)
            DIVNUM=NUMBER(5)
            IF (INDEX(DET,'T').GT.0) THEN
              READ(DET(3:3),'(I1)') SLATNUM
              CHANNEL=CELL(IZ,SLATNUM,DIVNUM,COPYNUM)
              IF (CHANNEL.NE.0) THEN
C
C ****  ISAJET vertex
C
                CALL ZVERTX(ZV,DZ)
                DIST = SQRT ((VECT(3)-ZV)**2 + (VECT(1))**2 + (VECT(2))
     &            **2
     &            )
                TOFVOL =  DIST / CLIGHT
                IF( TOFVOL .LT. TOFG ) THEN
C
C ****  USE GEANT TOFG ONLY IF IT EXCEEDS THE MINIMUM TOF
C ****  PROTECT AGAINST TOFG BEING TOF SINCE LAST INTERACTION
C
                  TOFVOL = TOFG                 ! SET TOF IN FIRST STEP
                  TOFVOL = TOFVOL
                END IF
C
C ****  Do short counters first
C
                IF ( SLATNUM.LT.4 ) THEN
C
C ****  TIME
C
                  TIME=TOFVOL
C                  TIME= TOFVOL + INFO(CHANNEL,3)
                  IF ( INFO(CHANNEL,2).EQ.0.OR.INFO(CHANNEL,2).GT.TIME)
     &              THEN
                    INFO(CHANNEL,2)=(TIME/FACTOR1)
                  ENDIF
C
C ****  Do long counters now
C
                ELSE IF ( SLATNUM.GE.4 ) THEN
                  IF ( COPYNUM.EQ.1.OR.COPYNUM.EQ.3 ) THEN
                    POINT=VECT(1)   !  Vertical box
                  ELSE IF (COPYNUM.EQ.2.OR.COPYNUM.EQ.4) THEN
                    POINT=VECT(2)   !  Horizontal box
                  ENDIF
C
C ****  Time, do south and then north
C

                  IF ( IZ.EQ.1 ) THEN
                    IF ((CHANNEL.EQ.66).OR.(CHANNEL.EQ.58).OR.
     &                (CHANNEL.EQ.59).OR.(CHANNEL.EQ.67)) THEN
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)-POINT
                      IF ( INFO(CHANNEL,2).EQ.0.OR.INFO(CHANNEL,2).GT.
     &                  TIME ) THEN
                        INFO(CHANNEL,2)=(TIME/FACTOR1)
                      ENDIF
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)+POINT
                      IF ( INFO(CHANNEL+4,2).EQ.0.OR.INFO(CHANNEL+4,2 )
     &                  .GT.TIME ) THEN
                        INFO(CHANNEL+4,2)=(TIME/FACTOR1)
                      ENDIF
                    ELSE IF ((CHANNEL.EQ.60).OR.(CHANNEL.EQ.68).OR.
     &                (CHANNEL.EQ.65).OR.(CHANNEL.EQ.57)) THEN
                      TIME=TOFVOL
C                   TIME=TOFVOL+INFO(CHANNEL,3)+POINT
                      IF ( INFO(CHANNEL,2).EQ.0.OR.INFO(CHANNEL,2).GT.
     &                  TIME ) THEN
                        INFO(CHANNEL,2)=(TIME/FACTOR1)
                      ENDIF
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)-POINT
                      IF ( INFO(CHANNEL+4,2).EQ.0.OR.INFO(CHANNEL+4,2 )
     &                  .GT.TIME ) THEN
                        INFO(CHANNEL+4,2)=(TIME/FACTOR1)
                      ENDIF
                    ENDIF
                  ELSE IF ( IZ.EQ.2) THEN
                    IF ((CHANNEL.EQ.30).OR.(CHANNEL.EQ.22).OR.
     &                (CHANNEL.EQ.23).OR.(CHANNEL.EQ.31)) THEN
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)-POINT
                      IF ( INFO(CHANNEL,2).EQ.0.OR.INFO(CHANNEL,2).GT.
     &                  TIME ) THEN
                        INFO(CHANNEL,2)=(TIME/FACTOR1)
                      ENDIF
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)+POINT
                      IF ( INFO(CHANNEL+4,2).EQ.0.OR.INFO(CHANNEL+4,2 )
     &                  .GT.TIME ) THEN
                        INFO(CHANNEL+4,2)=(TIME/FACTOR1)
                      ENDIF
                    ELSE IF ((CHANNEL.EQ.24).OR.(CHANNEL.EQ.32).OR.
     &                (CHANNEL.EQ.21).OR.(CHANNEL.EQ.29)) THEN
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)+POINT
                      IF ( INFO(CHANNEL,2).EQ.0.OR.INFO(CHANNEL,2).GT.
     &                  TIME ) THEN
                        INFO(CHANNEL,2)=(TIME/FACTOR1)
                      ENDIF
                      TIME=TOFVOL
C                      TIME=TOFVOL+INFO(CHANNEL,3)-POINT
                      IF ( INFO(CHANNEL+4,2).EQ.0.OR.INFO(CHANNEL+4,2 )
     &                  .GT.TIME ) THEN
                        INFO(CHANNEL+4,2)=(TIME/FACTOR1)
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
C
C ****  Get x,y,z,theta, and phi
C
                R=SQRT(VECT(1)**2+VECT(2)**2)
                THETA=ATAN2(R,VECT(3))
                PHI=ATAN2(VECT(2),VECT(1))
                ETA=-LOG(TAN(THETA/2.))
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE                              ! SUBSEQUENT STEPS IN THIS VOLUME
        IF ( USE ) THEN                 ! CHARGED TRACK TYPE
C
C ****  ENERGY
C
          DEVOL = DEVOL + DESTEP
          INFO(CHANNEL,1)=(DEVOL/2)*FACTOR2
          INFO(CHANNEL+4,1)=(DEVOL/2)*FACTOR2
          IF ( CHANNEL.GT.20.AND.CHANNEL.LT.37 ) THEN
            CHANNELL=CHANNEL+4
          ELSE IF (CHANNEL.GT.56) THEN
            CHANNELL=CHANNEL+4
          ENDIF
        ENDIF
      ENDIF
C
C ****  Do following when exiting volume or when stopped in volume
C
      IF ( (INWVOL.EQ.2.OR.ISTOP.NE.0).AND.(DEVOL.NE.0) ) THEN
C        HITS(1) = TOFVOL
C        HITS(2) = DEVOL
C
C        CALL GSCHIT(ISET,IDET,ITRA,NUMBV,HITS,NHSUM,IHIT)  ! Store hit
      ENDIF
C
   90 FORMAT(1X,' Run number is ',I2,'   and event number is ',I4)
  100 FORMAT(1X,'PARTICLE ',I2,'   NLEVEL ',I2,'   COPYNUM ',
     &  I2,'   DIV NUMBER ',I2,'   NAME ', A8,'  CHANNEL ',I4)
  110 FORMAT(1X,'X =',F6.1,', Y =',F6.1,', Z =',F7.1,
     &   ', ZV= ',F7.1) 
C     &  ', Theta =',F6.2,', Phi =',F6.2,' Eta =',F6.2)
  120 FORMAT(1X,' CHANNEL= ',I4,' TIME= ',F20.10,2X,F20.10)
  130 FORMAT(1X,' CHANNEL= ',I4, ' ENERGY= ',F15.5,2X,F15.5)
  999 RETURN
      END
      
      
