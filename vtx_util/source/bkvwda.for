      SUBROUTINE BKVWDA( LAYER, SECTOR, NPULSE, KPVWDA )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank VWDA for choosen layer, sector 
C-                         with room for NPULSE pulses
C-
C-   Inputs  : LAYER, SECTOR : cell address
C-             NPULSE : number of pulse to be stored
C-   Outputs : KPVWDA : pointer on bank VWDA ready to be filled
C-
C-   Created  25-AUG-1987   Olivier Callot
C-   Modified 15-FEB-1989   Peter Grudberg  Created BKVWDA from BKDCDA
C-   Updated  25-OCT-1993   Peter Grudberg  Change MZFORM call to correspond to
C-                                          new VWDA format 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'

      INTEGER LHVWDA, IOCODE
      CHARACTER*20 FMVWDA
      INTEGER KPVSEC, KPVWDA
      INTEGER LAYER, SECTOR, NPULSE
      INTEGER NBFADC, LPULSE, MPVWDA(5)
      INTEGER VERSION, ISETVN
      LOGICAL INIT
      DATA INIT / .TRUE. /
      DATA MPVWDA / 0, 0, 0, 2, 0 /
      DATA VERSION / 1 /
C----------------------------------------------------------------------
      IF ( LVSEC( SECTOR, LAYER ) .EQ. 0 ) THEN
        CALL BKVSEC( LAYER, SECTOR, NPULSE, KPVSEC )
      ENDIF
      IF ( INIT ) THEN
        INIT   = .FALSE.
        LPULSE = IQ( LVTXH + 6 )
        NBFADC = IQ( LVTXH + 7 )
        LHVWDA = 3 + 2*NBFADC
        WRITE(FMVWDA,5010) LHVWDA, LPULSE-7
 5010   FORMAT(I2,'I / 1B 4F 1B 1F',I1,'B')
        CALL MZFORM ('VWDA', FMVWDA, MPVWDA(5))
        CALL UCTOH( 'VWDA', MPVWDA(1), 4, 4 )
      ENDIF
C
C ****  Now, book VWDA....
C
      IF( LVWDA( SECTOR, LAYER ) .EQ. 0 ) THEN
        MPVWDA(4) = LHVWDA + NPULSE * LPULSE
        CALL MZLIFT( IXMAIN, LVWDA( SECTOR, LAYER),
     &                       LVSEC( SECTOR, LAYER), -1, MPVWDA, LHVWDA)
      ENDIF
      KPVWDA = LVWDA( SECTOR, LAYER )
      IQ( KPVWDA ) = ISETVN(IQ(KPVWDA),VERSION)
      IQ( KPVWDA - 5 ) = IQ( LVSEC( SECTOR, LAYER ) - 5 )
      IQ( KPVWDA + 1 ) = 0
      IQ( KPVWDA + 2 ) = NBFADC
      IQ( KPVWDA + 3 ) = LPULSE
  999 RETURN
      END
