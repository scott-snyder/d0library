      SUBROUTINE BKCDTK(NTRK,NREP,LCDTK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book CDTK bank 
C-
C-   Inputs  : NTRK [I] - NUMBER OF TRACKS
C-             NREP [I] - REPETITION NUMBER (#WORDS/TRACK)
C-
C-   Outputs : LCDTK [I] - LINK TO BOOKED CDTK BANK
C-
C-   Controls: NONE
C-
C-   Created   2-OCT-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDTK.LINK'
      INTEGER NTRK,NREP,LCDTK
      INTEGER LANLS,GZANLS
      INTEGER NL,NS,ND,IXIO
      LOGICAL FIRST
      SAVE    FIRST,IXIO
      DATA    FIRST/.TRUE./
      INTEGER CDTK_VERSION
      PARAMETER( CDTK_VERSION = 1 )
C----------------------------------------------------------------------
      LCDTK = 0

      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('CDTK','3I/5F2I',IXIO)        ! Describe Bank format
      ENDIF

      LANLS = GZANLS()
      IF (LANLS.LE.0) CALL BKANLS(LANLS)

      IF (IQ(LANLS-3).LT.IZCDTK) THEN
        CALL ERRMSG('WRONG ANLS VERSION','BKCDTK','BOOKING FAILS','W')
        GOTO 999
      ENDIF
      
      NL = 0
      NS = 0
      ND = NREP*NTRK+3

      CALL MZBOOK(IXMAIN,LCDTK,LANLS,-IZCDTK,'CDTK',NL,NS,ND,IXIO,0)
      
      IQ(LCDTK+1) = CDTK_VERSION
      IQ(LCDTK+2) = NREP
      IQ(LCDTK+3) = NTRK  

  999 RETURN
      END
