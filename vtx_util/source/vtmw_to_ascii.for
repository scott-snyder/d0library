      SUBROUTINE VTMW_TO_ASCII(FILENAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate an ASCII t0 file for VTX from the VTMW
C-   bank
C-
C-   Inputs  : FILENAME for output
C-   Outputs :
C-   Controls:
C-
C-   Created   2-AUG-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      CHARACTER*(*) FILENAME
C
      INTEGER  LAYER,SECTOR,WIRE,WEND,LABEL,ITEMS,NSEC(0:2),LVTMW
      INTEGER  GZVTMW,LUN,IER
      REAL     T0,T0ERR
      LOGICAL OK
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
      CALL GTUNIT(666,LUN,IER)
      CALL D0OPEN(LUN,FILENAME,'OF',OK)
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('Open failure','VTMW2ASCII',
     &    'Unable to open file to write tzeros','W')
        CALL RLUNIT(666,LUN,IER)
        GO TO 999
      ENDIF
      DO WEND = 0,1
        DO LAYER = 0,2
          LVTMW = GZVTMW(LAYER)
          ITEMS = IC(LVTMW + 3)
          DO SECTOR = 0,NSEC(LAYER)
            DO WIRE = 0,7
              LABEL = 512*LAYER + 16*SECTOR + 2*WIRE + WEND
              T0    = C(LVTMW+(SECTOR*8+WIRE)*ITEMS+2*WEND+6)
              T0ERR = C(LVTMW+(SECTOR*8+WIRE)*ITEMS+2*WEND+7)
              WRITE(LUN,'(1X,I4,1X,F6.2,1X,F5.2)')LABEL,T0,T0ERR
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      CLOSE(LUN)
      CALL RLUNIT(666,LUN,IER)
  999 RETURN
      END
