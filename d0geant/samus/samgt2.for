      SUBROUTINE SAMGT2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Truncate free space in GSAT banks, define total
C-   number of tracks in SAMUS and store this number in bank GMUH
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created   5-APR-1991   Andrei Kiryunin
C-   Updated  24-APR-1991   Andrei Kiryunin: add fifth word in hit 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGSAT.LINK'
      INTEGER LGMUH,GZGMUH,LGSAT,NTRACKS,NHITS,LENGTH,INCND
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      NTRACKS = 0
C
      LGMUH = GZGMUH(0)
      IF ( LGMUH.GT.0 ) THEN
        LGSAT=LQ(LGMUH-IZGSAT)
        IF (LGSAT.EQ.0) GOTO 999
C
C ****  Define number of hits per track
C
  10    NHITS=IQ(LGSAT+10)
        IF ( NHITS.EQ.0 ) THEN
          MSGSTR = ' SAMGT2: bad bank GSAT without hits '
          CALL INTMSG (MSGSTR)
        ENDIF
        NTRACKS = NTRACKS + 1
C
C ****  Decrease size of the bank
C
        LENGTH=10+NHITS*5
        INCND=370-LENGTH
        IF (INCND.GT.0) THEN
          CALL MZPUSH (IXCOM,LGSAT,0,-INCND,' ')
        ENDIF
C
        LGSAT=LQ(LGSAT)
        IF (LGSAT.GT.0) GOTO 10
      ENDIF
C
C ****  Store number of tracks in SAMUS in GMUH bank
C
      IQ(LGMUH+1)=NTRACKS
C
  999 RETURN
      END
