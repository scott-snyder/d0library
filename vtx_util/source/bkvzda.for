      SUBROUTINE BKVZDA( ZLAYER, NPULSE, KPVZDA )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank VZDA for choosen layer, sector
C-                         with room for NPULSE pulses
C-
C-   Inputs  : ZLAYER : z-strip layer
C-             NPULSE : number of pulses to be stored
C-   Outputs : KPVZDA : pointer on bank VZDA ready to be filled
C-
C-   Created  25-AUG-1987   Olivier Callot
C-   Modified 15-FEB-1989   Peter Grudberg  Created BKVZDA from BKDCDA
C-   Updated  17-JUN-1990   Peter Grudberg  fix MZFORM calls 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'

      INTEGER LHVZDA(0:5), IOCODE
      CHARACTER*20 FMVZDA
      INTEGER KPVZLA, KPVZDA
      INTEGER ZLAYER, NPULSE
      INTEGER NBFADC(0:5), LPULSE, MPVZDA(5)
      INTEGER NSTRIP(0:5), NEND(0:5), FORM(0:5)
      INTEGER ILAY
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA NSTRIP / 0, 0, 160, 192, 192, 128 /
      DATA NEND   / 0, 0, 2, 1, 1, 1 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LPULSE = IQ( LVTXH + 9 )
        CALL UCTOH( 'VZDA', MPVZDA(1), 4, 4 )
        DO ILAY = 0, 5
          NBFADC(ILAY) = NSTRIP(ILAY) * NEND(ILAY)
          LHVZDA(ILAY) = 3 + 2*NBFADC(ILAY)
          WRITE(FMVZDA,5010) LHVZDA(ILAY), LPULSE - 7
 5010     FORMAT(I3,'I / 1B 6F ',I1,'B')
          CALL MZFORM ('VZDA', FMVZDA, FORM(ILAY))
        ENDDO
      ENDIF
      IF ( LVZLA( ZLAYER ) .EQ. 0 ) THEN
        CALL BKVZLA( ZLAYER, NPULSE, KPVZLA )
      ENDIF
C
C ****  Now, book VZDA....
C
      IF( LVZDA( ZLAYER ) .EQ. 0 ) THEN
        MPVZDA(4) = LHVZDA(ZLAYER) + NPULSE * LPULSE
        MPVZDA(5) = FORM(ZLAYER)
        CALL MZLIFT( IXMAIN, LVZDA(ZLAYER), 
     &               LVZLA(ZLAYER), -1, MPVZDA, LHVZDA(ZLAYER))
      ENDIF
      KPVZDA = LVZDA( ZLAYER )
      IQ( KPVZDA - 5 ) = IQ( LVZLA( ZLAYER ) - 5 )
      IQ( KPVZDA + 1 ) = 0
      IQ( KPVZDA + 2 ) = NBFADC(ZLAYER)
      IQ( KPVZDA + 3 ) = LPULSE
  999 RETURN
      END
