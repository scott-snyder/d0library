      SUBROUTINE BKGMUH(ITRA,LGMUH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank GMUH
C-
C-   Inputs  : None
C-   Outputs : Address of the bank GMUH
C-   Controls: 
C-
C-   Created  8-APR-1993   Jasbir Singh , Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRA,NRPT,NIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGMUH.LINK'
      INTEGER LGHIT,LGMUH,GZGMUH,NH,NR,NL,NS
      PARAMETER( NR = 15 )
      PARAMETER( NH = 25 )
      PARAMETER( NL = 2 )
      PARAMETER( NS = 1 )
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
C        WRITE (FORM,9000) 
C 9000   FORMAT ('3I7F7I7F1I/5I,9F,I')
C 9000   FORMAT  ('3I7F7I7F/1I',I1,'F')
        CALL MZFORM('GMUH','3I7F1I2F5I/5I9F1I',NIO)
        FIRST = .FALSE.
      ENDIF
C
C ****  FIND EXISTING GMUH bank for track ITRA

C
      LGMUH = GZGMUH(ITRA)
      IF ( LGMUH.GT.0 )  GOTO 999
C
C ****  FIND GHIT AND BOOK GHIT IF NOT ALREADY EXISTING
C
      CALL BKGHIT (LGHIT)
C
C ****  BOOK GMUH for ITRA
C
      IF(LGHIT.LE.0) THEN
        CALL ERRMSG(' GHIT NOT THERE','BKGMUH',' OH NO!','W')
        GOTO 999
      END IF
      CALL MZBOOK (IXMAIN,LGMUH,LGHIT,-IZGMUH,'GMUH',NL,NS,NH,NIO,0)
      IF(LGMUH.GT.0) THEN
        IQ(LGMUH-5) = ITRA                ! TRACK NUMBER
        IQ(LGMUH+1) = 1                   ! BANK VERSION NUMBER
        IQ(LGMUH+2) = NH                  ! NUMBER OF HEADER WORDS
        IQ(LGMUH+3) = NR                  ! NUMBER OF REPEATED WORDS
      ELSE
        CALL ERRMSG(' MZBOOK FAILED','BKGMUH',' OH NO!','W')
      ENDIF
C
  999 RETURN
      END
