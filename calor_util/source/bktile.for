      SUBROUTINE BKTILE(NETA, NPHI, NR, LTILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book TILE bank and return pointer 
C-
C-   Inputs  : NETA - [I] : # of tiles in eta
C-             NPHI - [I] : # of tiles in phi
C-             NR   - [I] : # of words per tile
C-
C-   Outputs : LTILE  [I] : Zebra pointer to TILE
C-   Controls:
C-
C-   Created  14-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTILE.LINK'
      INTEGER NETA, NR, NPHI,LTILE,LSUP1
      INTEGER IOH
      SAVE IOH
      INTEGER NSIZ                      ! Total number of data words
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NSIZ = NR*NPHI*NETA + 4
      LTILE= 0
      IF ( FIRST ) THEN
        CALL MZFORM('TILE','4I/1I',IOH)
        FIRST = .FALSE.
      END IF
C--- Get supporting link. Call BKPROC. It will return the link to PROC if
C--- one already exists and will expand it if needed.
C
      CALL BKPROC(  LSUP1 )
C---Book it
      IF (LSUP1 .LE. 0) RETURN
      CALL MZBOOK(IXMAIN,LTILE,LSUP1,-IZTILE,'TILE',0,0,NSIZ,IOH,0)
      IQ( LTILE + 1 ) = 1           ! Version number
      IQ( LTILE + 2 ) = NETA        ! # of eta tiles
      IQ( LTILE + 3 ) = NPHI        ! # of phi tiles
      IQ( LTILE + 4 ) = NR          ! # of words per tile
  999 RETURN
      END
