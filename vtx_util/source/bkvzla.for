      SUBROUTINE BKVZLA( ZLAYER, NPULSE, KPVZLA)
C-----------------------------------------------------------------------
C-   Purpose and Methods:
C-  Subroutine BKVZLA books the bank "VZLA" for a specified
C-  vertex detector zlayer.
C-   Inputs  :
C-    ZLAYER is the VTX z layer.
C-    NPULSE is the number of hits to book space for.
C-   Outputs :
C-    KPVZLA is the link to the booked "VZLA" bank, 0 if error detected.
C-   Controls: none
C-  T. Trippe, 15 Mar. 1987
C-  Modified 06-DEC-1988 Peter Grudberg - added call to PATHGT, changed
C-      parameter statements to references to geo. banks
C-  Modified 14-APR-1989 Peter Grudberg - added NPULSE argument
C-  Modified 27-JUN-1989 P. Grudberg - remove GCUNIT
C-  Modified 18-NOV-1989 P.Grudberg - update and clean up
C-  Modified 07-JAN-1990 P. G. - fix stupid bug
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$LINKS:IZVZLA.LINK'
C
      INTEGER LHVZLA
      CHARACTER*20 FMVZLA
      INTEGER KPVZLA
      INTEGER ZLAYER, NPULSE
      INTEGER NBSTRP, LPULSE, MPVZLA(5)
      INTEGER NSTRIP(0:5)
      LOGICAL INIT
      DATA INIT / .TRUE. /
      DATA NSTRIP / 0, 0, 160, 192, 192, 128 /
      DATA MPVZLA / 0, 1, 1, 2, 0 /
C----------------------------------------------------------------------
      IF ( LVTXH .EQ. 0 ) THEN
        CALL BKVTXH
      ENDIF
      IF ( INIT ) THEN
        INIT = .FALSE.
        LPULSE = IQ( LVTXH + 8 )
        LHVZLA = 3
        WRITE(FMVZLA,5010) LHVZLA, LPULSE-9
 5010   FORMAT(I1,'I / 1B 8F ',I1,'B')
        CALL MZFORM ('VZLA', FMVZLA, MPVZLA(5))
        CALL UCTOH( 'VZLA', MPVZLA(1), 4, 4 )
      ENDIF
      NBSTRP = NSTRIP(ZLAYER)
C
C ****  Now, book VZLA....
C
      IF( LVZLA( ZLAYER ) .EQ. 0 ) THEN
        MPVZLA(4) = LHVZLA + NPULSE * LPULSE
        CALL MZLIFT( IXMAIN, LVZLA(ZLAYER),
     &                       LVTXH, -IZVZLA-ZLAYER,
     &                       MPVZLA, LHVZLA)
      ENDIF
      KPVZLA = LVZLA( ZLAYER )
      IQ( KPVZLA - 5 ) = 2**12 + ZLAYER * 2**9
      IQ( KPVZLA + 1 ) = 0              ! # of hits
      IQ( KPVZLA + 2 ) = NBSTRP
      IQ( KPVZLA + 3 ) = LPULSE
  999 RETURN
      END
