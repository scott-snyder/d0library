      SUBROUTINE BKTRDH(LTRDH)
C======================================================================
C
C   Purpose and Methods :  Books the /ZEBCOM/ event data structure down
C                          through the level of TRDH in preparation for
C                          adding TRD M.C. idealized hits (under HITS)
C
C-
C-   Created   6-NOV-1987   A. ZYLBERSTEJN
C-   Updated  26-JUL-1991   A. Zylberstejn  Version 1 with 4 struc. links
C-                             Previous version had 6 links with 3 which were
C-                             useless
C-   Updated  11-MAY-1992   Alain PLUQUET Add MZFORM
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADDIG.INC/LIST'
C      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C      INCLUDE 'D0$INC:TRDLNK.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRDH.LINK/LIST'
C      INCLUDE 'D0$LINKS:IZGEAN.LINK/LIST'
C      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INCLUDE 'D0$LINKS:IZHITS.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LTOPB,LHITS,N1,N2,GZHITS,GZTRDH,LTRDH,IOTRDH
      CHARACTER*4 LNKNAM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C======================================================================
C ****  Book GEAN or RECO if needed
      CALL PATHGT(LNKNAM)
      IF(LNKNAM.EQ.'GEAN')THEN
        N1=NBITF
        N2=NWORDF
      ELSE IF (LNKNAM.EQ.'RECO')THEN
        N1=32
        N2=1
      ELSE
        CALL INTMSG(' BKTRDH called with wrong argument')
        CALL EXIT
      END IF
      LTRDH = GZTRDH()
      IF ( LTRDH .NE. 0 ) GO TO 999
      LHITS=GZHITS()
      IF(LHITS.LE.0)CALL BKHITS(LHITS)
      IF(LHITS.LE.0)THEN
        CALL ERRMSG('Cant find bank HITS ','in BKTRDH',' ','W')
        GO TO 999
      END IF
      IF(FIRST)THEN
        CALL MZFORM ('TRDH','-I',IOTRDH)
        FIRST=.FALSE.
      END IF
      CALL MZBOOK ( IXMAIN, LTRDH, LHITS, -IZTRDH,'TRDH',
     &     4, 4, 4,IOTRDH,0 )
C          ^  ^  ^  ^     ^
C          |  |  |  |     |
C          |  |  |  |     +- Whole bank is cleared
C          |  |  |  +--- Whole bank is of type integer
C          |  |  +------ Nb. of data words
C          |  +--------- Number of structural links
C          +------------ Total number of links
      IQ(LTRDH+1)=1
      IQ(LTRDH+2)=N1!NB. OF BITS PER FADC BIN
      IQ(LTRDH+3)=N2 !NB. OF FADC BINS PER WORD
  999 CONTINUE
      RETURN
      END
