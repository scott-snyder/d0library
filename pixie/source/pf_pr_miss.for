      SUBROUTINE PF_PR_MISS(LAYER,XTRK,YTRK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write message that track in LAYER does
C-   not pass though active volume.
C-
C-   Inputs  : LAYER,XTRK,YTRK
C-   Outputs : none
C-
C-   Created  12-JUN-1991   Robert E. Avery
C-   Updated  17-FEB-1992   Susan K. Blessing  Fix write statement.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PXPARA.INC'
C
C Input:
      INTEGER LAYER 
      REAL    XTRK,YTRK 
C Local:
      INTEGER ISTRQU
      INTEGER IER
C
      REAL    XPOS,YPOS 
C
      CHARACTER*100 TEXT
      CHARACTER*4 TITCLR
C
      LOGICAL EZERROR
C
      REAL    CSIZE
      PARAMETER( CSIZE = 1.5 )
C----------------------------------------------------------------------
C
C Track misses sector, print message.
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF_PR_MISS',
     &      'Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUOPEN
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PXCOLR(TITCLR)
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
      CALL JJUST( 1, 2)
C
      WRITE(TEXT,100) '  Track at position x=',XTRK,
     &                 ' y=',YTRK,' in Layer =', LAYER
  100 FORMAT(A,F9.2,A,F9.2,A,I2)
      YPOS = YWIND2*0.90
      XPOS = -XWIND2
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
C
      TEXT = '  Does not pass through active volume of FDC.'
      YPOS = YPOS-2*CSIZE
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
      CALL JRCLOS
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      CALL EZRSET
  999 RETURN
      END
