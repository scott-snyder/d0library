      SUBROUTINE PZDUMP( UNIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produces a comprehensive debug of the class system
C-             on specified unit.
C-
C-   Inputs  : UNIT [I] : Fortran Unit Number
C-   Outputs : none
C-
C-   Created  20-JAN-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PZCOMN.INC'
      INTEGER UNIT
      INTEGER ICLA, NSEG, IOFS, NTSEG
      CHARACTER*16 CLANAM
C----------------------------------------------------------------------
      WRITE( UNIT, 1000 ) NMXCLA, MAXCLA
 1000 FORMAT(///' ** PZDUMP **',10X,'Status of the PIXIE Class Segment',
     &       ' System'//5X,'Maximum number of classes =',I5/
     &                  5X,'Maximum class number used =',I5/)
      DO 10 ICLA = 1, MAXCLA
        CALL PZCLNA( ICLA, CLANAM )
        NTSEG = 0
        IOFS  = 0
   20   CALL PZLSEG( ICLA, NSEG, IOFS )
        IF ( IOFS.GT.0 ) THEN
          NTSEG = NTSEG + 1
          GOTO 20
        ENDIF
        WRITE( UNIT, 1100 ) ICLA, CLANAM, NTSEG
 1100   FORMAT(10X,'Class number',I5,' named ',A16,' contains ',I6,
     &         ' active segments')
   10 CONTINUE
  999 RETURN
      END
