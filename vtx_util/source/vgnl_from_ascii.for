      SUBROUTINE VGNL_FROM_ASCII(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VGNL banks from GAINs stored in an ASCII file
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-SEP-1992   M. Pang
C-   Updated   5-NOV-1993   Ed Oltman  Update for new area vs. drift distance 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NSEC(3),LAYER,SECTOR,WIRE,END
      INTEGER NWFADC,NBFADC,ADC,LUN1,LUN2,IER
      INTEGER LVGNL,IDRIFT,IP
      INTEGER LENGTH,GZVGNL
      REAL    DXF(0:2),QDIV1_AVE(0:2),AREAM(0:7,0:2)
      REAL    CAT1(0:50,0:2),CAT2(0:50,0:2)
      INTEGER MAXB(0:7,0:2)
      REAL GAIN(0:2,0:31,0:7,0:1)
      PARAMETER( NWFADC = 1 )
      PARAMETER( NBFADC = 16)
      LOGICAL OK, OK1
      CHARACTER*60 GAINS_FILE, AREAS_FILE
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
      CALL EZPICK('VTWSTP_RCP')
      CALL EZGETS('GAINS_FILE',1,GAINS_FILE,LENGTH,IER)
      CALL EZGETS('AREAS_FILE',1,AREAS_FILE,LENGTH,IER)
      CALL EZRSET
C
      CALL GTUNIT(666,LUN1,IER)
      CALL GTUNIT(666,LUN2,IER)
      CALL D0OPEN(LUN1,AREAS_FILE,'IF',OK)
      CALL D0OPEN(LUN2,GAINS_FILE,'IF',OK1)
      OK = OK.AND.OK1
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('Open failure','ASCII2VGNH',
     &    'Unable to open GAIN files','W')
        CALL RLUNIT(666,LUN1,IER)
        CALL RLUNIT(666,LUN2,IER)
        GO TO 999
      ENDIF
C
C
      DO LAYER = 0,2
        READ(LUN1,*) DXF(LAYER)
        READ(LUN1,*) QDIV1_AVE(LAYER)
        DO WIRE = 0,7
          READ(LUN1,*) MAXB(WIRE,LAYER)
        ENDDO
        DO WIRE = 0,7
          READ(LUN1,*) AREAM(WIRE,LAYER)
        ENDDO
        DO IDRIFT = 0,MAXB(7,LAYER)
          READ(LUN1,*) CAT1(IDRIFT,LAYER)
        ENDDO
        DO IDRIFT = 0,MAXB(6,LAYER)
          READ(LUN1,*) CAT2(IDRIFT,LAYER)
        ENDDO
      ENDDO
  350 READ(LUN2,*,END=450) LAYER,SECTOR,WIRE,END,
     &            GAIN(LAYER,SECTOR,WIRE,END)
      GOTO 350
  450 CONTINUE
C
      DO LAYER = 0,2
        LVGNL = GZVGNL(LAYER)
        C(LVGNL+6) = DXF(LAYER)
        C(LVGNL+7) = QDIV1_AVE(LAYER)
        DO WIRE = 0,7
          IC(LVGNL+8+WIRE) = MAXB(WIRE,LAYER)
        ENDDO
        DO WIRE = 0,7
          C(LVGNL+16+WIRE) = AREAM(WIRE,LAYER)
        ENDDO
        DO IDRIFT = 0,MAXB(7,LAYER)
          C(LVGNL+24+IDRIFT) = CAT1(IDRIFT,LAYER)
        ENDDO
        DO IDRIFT = 0,MAXB(6,LAYER)
          C(LVGNL+24+MAXB(7,LAYER)+1+IDRIFT) = CAT2(IDRIFT,LAYER)
        ENDDO
        DO SECTOR = 0,NSEC(LAYER+1)
          DO 30 ADC = 0, NBFADC-1
            IP = LVGNL + ( SECTOR*NBFADC + ADC ) * NWFADC + 5 + 3*41
            WIRE = INT(FLOAT(ADC)/2.)
            END = MOD(ADC,2)
            C( IP+1 ) = GAIN(LAYER,SECTOR,WIRE,END)
            IF ( C(IP+1) .LE. 0.) C(IP+1) = 1.
   30     CONTINUE
        END DO
      END DO
      CLOSE(LUN1)
      CLOSE(LUN2)
      CALL RLUNIT(666,LUN1,IER)
      CALL RLUNIT(666,LUN2,IER)
C
  999 RETURN
      END
