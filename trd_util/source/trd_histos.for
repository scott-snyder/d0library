      SUBROUTINE TRD_HISTOS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Provides default histograms for online
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-APR-1992   Jean-Francois Glicenstein
C-   Updated  20-SEP-1993   A. Zylberstejn  Updated for 512 cells in layer 3
C-   Updated   7-DEC-1993   JFG  Added normalized histograms
C-   Updated  12-JAN-1994   A. Zylberstejn
C-   Updated  27-JAN-1995   A. Zylberstejn: rewrite completly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRHITW_512.INC'
      REAL SUM_PED(NTOT_WIRE_TRD)
      INTEGER CHA1,ENE,TCHNB,UBIT,TDATA(130)
      INTEGER I,ID,IL,IH,IK,NTOT,ICH,REAL_WIRE,WIRE,TWIRCOR
      REAL NUMERO,ENERGY,ENERGY_LIMITS(2),PEDES
      INTEGER NORM_FREQ
      INTEGER NORM_ANSWER,IER
      REAL CONTENT(6)
      CHARACTER*1 LAY
      CHARACTER*3 ENMAP_ANSWER,ENDIS_ANSWER
C
      INTEGER NEVT
      DATA NEVT/0/
C
      LOGICAL FIRST
      DATA ENERGY_LIMITS/0.,10./
      DATA FIRST/.TRUE./
C
      NEVT = NEVT + 1
C
C     First, book the right histograms
      IF (FIRST) THEN
        CALL EZPICK('TRDHIT_RCP')
        CALL EZGET_i('ENERGY_MAPS',I,IER)
        CALL UHTOC(I,3,ENMAP_ANSWER,3)
        CALL EZGET_i('ENERGY_DISTRIBUTIONS',I,IER)
        CALL UHTOC(I,3,ENDIS_ANSWER,3)
        CALL EZGET('ENERGY_LIMITS',ENERGY_LIMITS(1),IER)
        CALL EZGET_i('NORM_FREQUENCY',NORM_FREQ,IER)
        CALL EZRSET
C        CALL DHDIR(' ','//PAWC/TRD',IER,' ')
        CALL DHDIR(' ','//PAWC/CD',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('TRD','TRD_HISTOS',
     &        ' ERROR SETTING HBOOK DIRECTORY ','W')
        END IF
C
C     Books wire maps
        DO 10 ID = 1,6
          WRITE(LAY,5) ID
    5     FORMAT(I1)
          CALL HBOOK1(200+ID,'TRD Wire Map Layer '//LAY,
     &      NWIRE_PER_LAYER(ID),1.,FLOAT(NWIRE_PER_LAYER(ID)+1),0.)
C          CALL HBARX(200+ID)
          IF (ENMAP_ANSWER.EQ.'Y') THEN
            CALL HBOOK1(210+ID,'TRD Energy Map Layer '//LAY,
     &        NWIRE_PER_LAYER(ID),1.,FLOAT(NWIRE_PER_LAYER(ID)+1),0.)
C            CALL HBARX(210+ID)
            CALL HBOOK1(230+ID,'normalized TRD Energy Map Layer '//LAY,
     &        NWIRE_PER_LAYER(ID),1.,FLOAT(NWIRE_PER_LAYER(ID)+1),0.)
C            CALL HBARX(230+ID)
          ENDIF
          IF (ENDIS_ANSWER.EQ.'Y') THEN
            CALL HBOOK1(220+ID,
     &        'TRD Energy Distribution Layer '//LAY,100,ENERGY_LIMITS(1)
     &        ,ENERGY_LIMITS(2),0.)
C            CALL HBARX(220+ID)
          ENDIF
   10   CONTINUE
        CALL HBOOK1(250,'TRD Histogram Contents',6,1.,7.,0.)
        DO ICH =  1,  6
          DO REAL_WIRE =  1,  NWIRE_PER_LAYER(ICH)
            WIRE=REAL_WIRE
            CALL TRGPED('BID',WIRE,ICH,PEDES,IER) ! Get pedestals
            IF(IER.NE.0)THEN
              CALL ERRMSG('CANT FIND TRD PEDESTALS','TSETWC',' ','W')
              SUM_PED(TCHNB(WIRE,ICH))=0.
            ELSE
              SUM_PED(TCHNB(WIRE,ICH))=128*PEDES
            END IF
          END DO
        END DO

        FIRST = .FALSE.
      ENDIF
C
C      CALL DHDIR(' ','//PAWC/TRD',IER,' ')
      CALL DHDIR(' ','//PAWC/CD',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('TRD','TRD_HISTOS',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      END IF
C
C      CALL HCDIR('//PAWC/CD/TRD',' ')     !GO TO TOP DIRECTORY
      CALL HUNPAK(250,CONTENT,' ',0)
      DO 30 ICH=1,6
        DO 20 REAL_WIRE=1,NWIRE_PER_LAYER(ICH)
          WIRE=REAL_WIRE
          CALL TCODER(CHA1,ICH-1,WIRE-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
          IF (TDATA(1).LE.0)GO TO 20
          CALL HFILL(200+ICH,FLOAT(WIRE),0.,1.)
          ENE=0
          DO I=1,TDATA(1)
            ENE=ENE+TDATA(I+2)
          END DO
          ENERGY=ENE-SUM_PED(TCHNB(WIRE,ICH))
          IF (ENMAP_ANSWER.EQ.'Y') THEN
            CALL HFILL(210+ICH,FLOAT(WIRE),0.,ENERGY)
          ENDIF
          IF (ENDIS_ANSWER.EQ.'Y') THEN
            CALL HFILL(220+ICH,ENERGY,0.,1.)
          ENDIF
          CONTENT(ICH) = CONTENT(ICH)+1.
   20   CONTINUE
        IF (ENMAP_ANSWER.EQ.'Y'.AND.
C     &        MOD(INT(CONTENT(ICH)),NORM_FREQ).EQ.0) THEN
     &        MOD(NEVT,NORM_FREQ).EQ.0) THEN
          CALL HOPERA(210+ICH,'/',200+ICH,230+ICH,1.,1.)
        ENDIF
   30 CONTINUE
      CALL HPAK(250,CONTENT)
  999 RETURN
      END
