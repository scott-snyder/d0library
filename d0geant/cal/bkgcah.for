      SUBROUTINE BKGCAH(ITRA,LGCAH,NRPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Zebra bank GCAH - bank number ITRA
C-
C-   Inputs  : ITRA = Bank (track) number
C-             NRPT = Number of Repeated data words
C-   Outputs : LGCAH = Link to new bank
C-   Controls:
C-
C-   Created   9-FEB-1989   Alan M. Jonckheere
C-   Updated   1-JUN-1989   A.M.Jonckheere  correct MZFORM
C-   Updated  19-SEP-1989   Chip Stewart  SPACE FOR EM /HADRON ENERGY SUMS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRA,NRPT
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGCAH.LINK'
      INTEGER LGCAH,GZGCAH,LGHIT,GZGHIT
      INTEGER NIO
C
      CHARACTER*20 FORM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        WRITE (FORM,9000) NRPT-1
 9000   FORMAT ('3I7F7I14F2I/1I',I1,'F')
        CALL MZFORM('GCAH',FORM,NIO)
        FIRST = .FALSE.
      ENDIF
C
      LGCAH = GZGCAH(ITRA)
      IF ( LGCAH.GT.0 ) GOTO 999
      CALL BKGHIT(LGHIT)                ! Make sure high level banks are
C                                       ! booked.
      LGHIT = GZGHIT()
      IF ( LGHIT.GT.0 ) THEN
        CALL MZBOOK(IXMAIN,LGCAH,LGHIT,-IZGCAH,'GCAH',1,0,5019,NIO,0)
        IQ(LGCAH-5) = ITRA                    ! TRACK NUMBER
        IQ(LGCAH+1) = 2                       ! BANK VERSION NUMBER
        IQ(LGCAH+2) = 33                      ! NUMBER OF HEADER WORDS
        IQ(LGCAH+3) = NRPT                    ! NUMBER OF REPEATED WORDS
        CALL FLGCAH(ITRA,LGCAH)               ! Put in ISAJET info
        IQ(LGCAH+33) = 0                      ! Number of filled cells
      ELSE
        CALL D0_ABORT(' BKGCAH - Support bank GHIT doesn''t exist')
      ENDIF
  999 RETURN
      END
