      SUBROUTINE PZOPEN( CLANUM, SEGNUM )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a new segment in class CLANUM ( 0 means
C-               the current one ), open it and return his number in SEGNUM.
C-
C-   Inputs  : CLANUM [I] : Class number, 0 = current
C-   Outputs : SEGNUM [I] : New segment number
C-
C-   Created  20-JAN-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PZCOMN.INC'
      INTEGER CLANUM, SEGNUM
      INTEGER LOCLA, ISEG, IARR(19)
C----------------------------------------------------------------------
      LOCLA = CLANUM
      IF( LOCLA .EQ. 0 ) LOCLA = CURCLA
      IF( LOCLA .LE. 0 .OR. LOCLA .GT. MAXCLA ) THEN
        WRITE( 6, 1000 )
 1000   FORMAT('0** PZOPEN **  Call to open a segment without opening',
     &    ' a class first thru PZCLAS. Default class opened'/)
        CALL PZCLAS( LOCLA, ' ')
      ENDIF
C
C ****  Close any opened segment
C
      CALL J1IGET( 1, ISEG )
      IF ( ISEG.GT.0 ) THEN
        CALL JRCLOS
      ELSEIF ( ISEG .EQ. 0 ) THEN
        CALL JCLOSE
      ENDIF
C
C ****  Scan list. If this segment is DI3000 opened, tag and continue
C
      DO 10 ISEG = 1, NMXSEG
        IF ( SEGLIS( ISEG ) .EQ. 0 ) THEN
          SEGLIS( ISEG ) = LOCLA
          CALL JISGMT( ISEG, IARR )
          IF( IARR(1) .EQ. 0 ) THEN       ! Not DI300 opened
C
C ****  OK, this is the segment to be opened
C
            SEGNUM = ISEG
            CALL JROPEN( ISEG )
            GOTO 999
          ENDIF
        ENDIF
   10 CONTINUE
C
C ****  No free segment. Dump and abort
C
      WRITE( 6, 1100 ) LOCLA
 1100 FORMAT('0** PZOPEN ** No more free segments, can not add one',
     &       ' in class',I5/)
      CALL PZDUMP( 6 )
      CALL EXIT
  999 RETURN
      END
