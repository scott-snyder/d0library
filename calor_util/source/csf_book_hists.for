      SUBROUTINE CSF_BOOK_HISTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK HISTS IN A DIRECTORY
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-NOV-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,IOFF
      CHARACTER*50 TIT
      INTEGER MODMX
      PARAMETER( MODMX = 17 )
      CHARACTER*32 DIRS(MODMX)
      DATA DIRS/'CCEM','CCFH','CCCH',
     &          'NCCMG','NICD',
     &          'NECEM','NECIH','NECMH','NECOH','NECMG',
     &          'PCCMG','PICD',
     &          'PECEM','PECIH','PECMH','PECOH','PECMG'/
*
      REAL     ETA_RANGE(2,MODMX), ETALO,ETAHI
      INTEGER NETA, NETAS(MODMX)
C
      DATA NETAS/27,27,27,
     &  5,6,
     &  24,24,10,7,6,
     &  5,6,
     &  24,24,10,7,6/
C
      DATA ETA_RANGE/-13.5,13.5,-13.5,13.5,-13.5,13.5,
     &  -12.5,-7.5,-14.5,-8.5,
     &  -37.5,-13.5,-37.5,-13.5,-20.5,-10.5,-14.5,-7.5,-13.5,-7.5,
     &  7.5,12.5,8.5,14.5,
     &  13.5,37.5,13.5,37.5,10.5,20.5,7.5,14.5,7.5,13.5/
C
      INTEGER IL,IDEPTH,ID
      REAL    ETMN,ETMX
      DATA ETMN/0.0/
      DATA ETMX/10.0/
      REAL    ET_LIM(MODMX), E_LIM(MODMX) !HIST UPPER LIMITS
      DATA E_LIM/50.0,50.0,20.0,
     &  10.0,10.0,
     &  50.0,50.0,20.0,10.0,10.0,
     &  10.0,10.0,
     &  50.0,50.0,20.0,10.0,10.0/
C
      DATA ET_LIM/50.0,20.0,10.0,
     &  10.0,10.0,
     &  20.0,10.0,10.0,10.0,5.0,
     &  10.0,10.0,
     &  20.0,10.0,10.0,10.0,5.0/
C----------------------------------------------------------------------
      DO I = 1 , MODMX
        IOFF = 100*I
        DO J = 1 , 64
          WRITE(TIT,2)J,ETMN,DIRS(I)(1:6)
    2     FORMAT('ENERGY PHI SEG',I4,' ETMN=',F4.2,2X,A6)
          CALL HBOOK1(IOFF+J,TIT(1:35),100,0.0,E_LIM(I),0.)
        ENDDO
C
        IF ( I.EQ.1.OR.I.EQ.6.OR.I.EQ.13 ) THEN
          IOFF = 100*I+10000
          DO J = 1 ,64
            WRITE(TIT,21)J,ETMN,DIRS(I)(1:6)
   21       FORMAT('UNTRIGGERED ENERGY PHI SEG',I4,' ETMN=',F4.2,2X,
     &          A6)
            CALL HBOOK1(IOFF+J,TIT(1:47),100,0.0,E_LIM(I),0.)
          ENDDO
          DO IL = 1,7
C
C ****  plot eta phi for em depths
C
            IDEPTH= IL
            IF ( IL.GE.3.AND.IL.LE.6 ) THEN
              IDEPTH = 3
            ENDIF
C
            IF(IL.EQ.7)IDEPTH=4
            WRITE(TIT,7)DIRS(I)(1:6),IDEPTH
    7       FORMAT(' ENERGY FLOW LEGO: MODULE ',A6,' DEPTH ',I5)
C
            NETA = NETAS(I)
            ETALO= ETA_RANGE(1,I)
            ETAHI= ETA_RANGE(2,I)
            IOFF = 100*I+10000 + 1000
            IF ( IDEPTH.EQ.3 ) THEN
              IF ( IL.EQ.3 ) THEN
C ONLY ONCE
                CALL HBOOK2(IOFF+IDEPTH,TIT,NETA,ETALO,ETAHI,
     &              64,0.5,64.5,0.)
              ENDIF
            ELSE
              CALL HBOOK2(IOFF+IDEPTH,TIT,NETA,ETALO,ETAHI,
     &            64,0.5,64.5,0.)
            ENDIF
          ENDDO
        ENDIF
*
C
        WRITE(TIT,4)DIRS(I)(1:6)
    4   FORMAT(' ENERGY FLOW LEGO: MODULE ',A6)
        NETA = NETAS(I)
        ETALO= ETA_RANGE(1,I)
        ETAHI= ETA_RANGE(2,I)
        CALL HBOOK2(I,TIT,NETA,ETALO,ETAHI,64,0.5,64.5,0.)
      ENDDO
C
C ****  vertex plots
C
      CALL HBOOK1(41,' X POSITION OF VERTEX',100,-1.0,1.0,0.)
      CALL HBOOK1(42,' Y POSITION OF VERTEX',100,-1.0,1.0,0.)
      CALL HBOOK1(43,' Z POSITION OF VERTEX',100,-50.0,50.0,0.)
      CALL HBOOK1(45,' ENERGY FLOW VS ETA ',75,-37.5,37.5,0.)
C
      CALL HBOOK1(71,'RUN NUMBER ',250,75000.,100000.,0.)
C
  999 RETURN
      END
