      SUBROUTINE VGNL_TO_ASCII(FILENAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate an ASCII GAIN files for VTX from the VGNH
C-
C-   Inputs  :
C-      FILENAMES(1) : FILE THAT CONTAINS AREA VS DRIFT CORRECTION
C-      FILENAMES(2) : FILE THAT CONTAINS GAIN CORRECTION
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  24-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER NSEC(3),LAYER,SECTOR,WIRE,END
      INTEGER NWFADC,NBFADC,ADC,LUN1,LUN2,IER
      INTEGER GZVGNL,LVGNL,N,ICATG,IDRIFT,IP
      PARAMETER( NWFADC = 1 )
      PARAMETER( NBFADC = 16)
      LOGICAL OK
      CHARACTER*(*) FILENAME(2)
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
      CALL GTUNIT(666,LUN1,IER)
      CALL GTUNIT(666,LUN2,IER)
      CALL D0OPEN(LUN1,FILENAME(1),'OF',OK)
      CALL D0OPEN(LUN2,FILENAME(2),'OF',OK)
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('Open failure','VGNH2ASCII',
     &    'Unable to open file to write gains','W')
        CALL RLUNIT(666,LUN1,IER)
        CALL RLUNIT(666,LUN2,IER)
        GO TO 999
      ENDIF
C
      DO LAYER = 0,2
        LVGNL = GZVGNL(LAYER)
        N = 0
        DO ICATG = 0,2
          DO IDRIFT = -20,20
            N = N + 1
            WRITE(LUN1,500) LAYER,ICATG,IDRIFT,C(LVGNL+5+N)
          END DO
        END DO
        DO SECTOR = 0,NSEC(LAYER+1)
          DO 30 ADC = 0, NBFADC-1
            IP = LVGNL + ( SECTOR*NBFADC + ADC ) * NWFADC + 5 + 3*41
            WIRE = INT(FLOAT(ADC)/2.)
            END = MOD(ADC,2)
            WRITE(LUN2,600) LAYER,SECTOR,WIRE,END,C(IP+1)
   30     CONTINUE
        END DO
      END DO
C
  500 FORMAT(1X,I1,2X,I1,2X,I4,2X,F7.4)
C
  600 FORMAT(1X,I1,2X,I2,2X,I1,2X,I1,2X,F7.4)
C
      CLOSE(LUN1)
      CLOSE(LUN2)
      CALL RLUNIT(666,LUN1,IER)
      CALL RLUNIT(666,LUN2,IER)
  999 RETURN
      END
