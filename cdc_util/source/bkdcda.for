      SUBROUTINE BKDCDA( LAYER, SECTOR, NPULSE, KPDCDA )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank DCDA for choosen sector with room
C-                         for NPULSE pulses
C-
C-   Inputs  : LAYER, SECTOR : cell address
C-             NPULSE : number of pulse to be stored
C-   Outputs : KPDCDA : pointer on bank DCDA ready to be filled
C-
C-   Created  25-AUG-1987   Olivier Callot
C-   Updated  13-JUL-1989   Qizhong Li-Demarteau    put in version # 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
C
      INTEGER LHDCDA, IOCODE, ISETVN
      CHARACTER*20 FMDCDA
      INTEGER KPDSEC, KPDCDA
      INTEGER LAYER, SECTOR, NPULSE
      INTEGER NBFADC, LPULSE, MPDCDA(5)
      LOGICAL INIT
      DATA INIT / .TRUE. /
      DATA MPDCDA / 0, 0, 0, 2, 0 /
C----------------------------------------------------------------------
      IF ( LDSEC( SECTOR, LAYER ) .EQ. 0 ) THEN
        CALL BKDSEC( LAYER, SECTOR, 0, KPDSEC )
      ENDIF
      IF ( INIT ) THEN
        INIT   = .FALSE.
        LPULSE = IQ( LCDCH + 4 )
        NBFADC = IQ( LCDCH + 5 )
        LHDCDA = 3 + 2*NBFADC
        WRITE(FMDCDA,5010) LHDCDA, LPULSE-7
 5010   FORMAT(I2,'I / 1B 6F ',I1,'B')
        CALL MZFORM ('DCDA', FMDCDA, MPDCDA(5))
        CALL UCTOH( 'DCDA', MPDCDA(1), 4, 4 )
      ENDIF
C
C ****  Now, book DCDA....
C
      IF( LDCDA( SECTOR, LAYER ) .EQ. 0 ) THEN
        MPDCDA(4) = LHDCDA + NPULSE * LPULSE
        CALL MZLIFT( IXMAIN, LDCDA( SECTOR, LAYER),
     &                       LDSEC( SECTOR, LAYER), -1, MPDCDA, LHDCDA)
      ENDIF
      KPDCDA = LDCDA( SECTOR, LAYER )
      IQ( KPDCDA - 5 ) = IQ( LDSEC( SECTOR, LAYER ) - 5 )
      IQ(KPDCDA) = ISETVN(IQ(KPDCDA),0)
      IQ( KPDCDA + 1 ) = 0
      IQ( KPDCDA + 2 ) = NBFADC
      IQ( KPDCDA + 3 ) = LPULSE
  999 RETURN
      END
