      SUBROUTINE BKSBPG ( NDATA, VALUE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create bank SBPG and fill it with geometric
C-                         parameters of one GEANT volume necessary for
C-                         description of the collimators, quadrupoles
C-                         and beam-pipe in SAMUS region
C-
C-   Inputs  : NDATA - number of data words
C-             VALUE(NDATA) - array with values for volume description
C-   Outputs : None
C-   Controls:
C-
C-   Created  20-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSBPG.LINK'
      INTEGER LSBPH, GZSBPH, NDATA, VALUE(NDATA)
      INTEGER NFORM, LSBPG, I, LZLAST
      CHARACTER*80 MSGSTR               ! Error message
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Initialization
C
      IF ( FIRST ) THEN
        CALL MZFORM ( 'SBPG','1I 2H 1I 2H 2I 3F 1I -F',NFORM )
        FIRST=.FALSE.
      ENDIF
C
C ****  Check existence of the header bank SBPH
C
      LSBPH = GZSBPH()
      IF (LSBPH.EQ.0) THEN
        MSGSTR = ' *** BKSBPG: supporting bank SBPH does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
C ****  Book bank SBPG
C
      LSBPG=LZLAST(IXSTP,LC(LSBPH-IZSBPG))
      IF (LSBPG.EQ.0) THEN
        CALL MZBOOK ( IDVSTP, LSBPG, LSBPH, -IZSBPG, 'SBPG', 0, 0,
     &  NDATA+1, NFORM, 0 )
        IC(LSBPG-5)=1
      ELSE
        CALL MZBOOK ( IDVSTP, LSBPG, LSBPG, 0, 'SBPG', 0, 0,
     &  NDATA+1, NFORM, 0 )
      ENDIF
C
C ****  Fill bank SBPG with data
C
      IC(LSBPG+1) = 1                   ! Bank version number
      DO 10 I=1,NDATA
        IC(LSBPG+1+I)=VALUE(I)
   10 CONTINUE
C
  999 RETURN
      END
