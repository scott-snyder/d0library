      SUBROUTINE BKSMAG ( NDATA, VALUE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create bank SMAG and fill it with parameters
C-                         necessary for description of the SAMUS magnets
C-
C-   Inputs  : NDATA - number of data words
C-             VALUE(NDATA) - array with values for volume description
C-   Outputs : None
C-   Controls:
C-
C-   Created  22-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMAG.LINK'
      INTEGER LSMAH, GZSMAH, NDATA, VALUE(NDATA)
      INTEGER NFORM, LSMAG, I, LZLAST
      CHARACTER*80 MSGSTR               ! Error message
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Initialization
C
      IF ( FIRST ) THEN
        CALL MZFORM ( 'SMAG','2I 1H 1I 1H 9F 1I 9F 1I',NFORM )
        FIRST=.FALSE.
      ENDIF
C
C ****  Check existence of the header bank SMAH
C
      LSMAH = GZSMAH()
      IF (LSMAH.EQ.0) THEN
        MSGSTR = ' *** BKSMAG: supporting bank SMAH does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
C ****  Book bank SMAG
C
      LSMAG=LZLAST(IXSTP,LC(LSMAH-IZSMAG))
      IF (LSMAG.EQ.0) THEN
        CALL MZBOOK ( IDVSTP, LSMAG, LSMAH, -IZSMAG, 'SMAG', 0, 0,
     &  NDATA+1, NFORM, 0 )
        IC(LSMAG-5)=1
      ELSE
        CALL MZBOOK ( IDVSTP, LSMAG, LSMAG, 0, 'SMAG', 0, 0,
     &  NDATA+1, NFORM, 0 )
      ENDIF
C
C ****  Fill bank SMAG with data
C
      IC(LSMAG+1) = 1                   ! Bank version number
      DO 10 I=1,NDATA
        IC(LSMAG+1+I)=VALUE(I)
   10 CONTINUE
C
  999 RETURN
      END
