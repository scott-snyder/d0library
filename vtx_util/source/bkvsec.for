      SUBROUTINE BKVSEC( LAYER, SECTOR, NPULSE, KPVSEC )
C-----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Subroutine BKVSEC books the bank "VSEC" for a specified
C-  vertex detector layer and sector.
C-   Inputs  :
C-    LAYER, SECTOR
C-    NPULSE is the number of hits to book space for.
C-   Outputs :
C-    LKVSEC is the link to the booked "VSEC" bank, 0 if error detected.
C-   Controls: none
C-  T. Trippe, 4 Jan. 1987
C-  Modified 06-DEC-1988 Peter Grudberg-added call to PATHGT
C-  Modified 24-MAY-1989 Peter Grudberg-add argument NPULSE
C-  Modified 27-JUN-1989 P. Grudberg - remove GCUNIT
C-  Modified 18-NOV-1989 P. Grudberg - update, clean up
C-  Modified 07-JAN-1990 P. G. - fix stupid bug
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$LINKS:IZVSEC.LINK'
C
      INTEGER LHVSEC
      CHARACTER*20 FMVSEC
      INTEGER KPVSEC, KPVLAY
      INTEGER LAYER, SECTOR, NPULSE
      INTEGER NBWIRE, LPULSE, MPVSEC(5)
      LOGICAL INIT
      DATA INIT / .TRUE. /
      DATA MPVSEC / 0, 1, 1, 2, 0 /
C----------------------------------------------------------------------
      IF ( LVLAY( LAYER ) .EQ. 0 ) THEN
        CALL BKVLAY( LAYER, KPVLAY )
      ENDIF
      IF ( INIT ) THEN
        INIT   = .FALSE.
        LPULSE = IQ( LVTXH + 4 )
        NBWIRE = IQ( LVTXH + 5 )
        LHVSEC = 3 + 2*NBWIRE
        WRITE(FMVSEC,5010) LHVSEC, LPULSE-9
 5010   FORMAT(I2,'I / 1B 8F ',I1,'B')
        CALL MZFORM ('VSEC', FMVSEC, MPVSEC(5))
        CALL UCTOH( 'VSEC', MPVSEC(1), 4, 4 )
      ENDIF
C
C ****  Now, book VSEC....
C
      IF( LVSEC( SECTOR, LAYER ) .EQ. 0 ) THEN
        MPVSEC(4) = LHVSEC + NPULSE * LPULSE
        CALL MZLIFT( IXMAIN, LVSEC( SECTOR, LAYER),
     &                       LVLAY( LAYER), -IZVSEC-SECTOR, 
     &                       MPVSEC, LHVSEC)
      ENDIF
      KPVSEC = LVSEC( SECTOR, LAYER )
      IQ( KPVSEC - 5 ) = LAYER * 2**9 + SECTOR * 2**4
      IQ( KPVSEC + 1 ) = 0              ! # of hits
      IQ( KPVSEC + 2 ) = NBWIRE
      IQ( KPVSEC + 3 ) = LPULSE
  999 RETURN
      END
