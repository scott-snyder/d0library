      SUBROUTINE BKGLVH(LGLVH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Zebra bank GLVH
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   6-MAR-1989   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGLVH.LINK'
      INTEGER LGLVH,GZGLVH,LGHIT
      INTEGER NIO
      INTEGER NTLNK,NSLNK,NWRD
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      LGLVH = GZGLVH()
      IF ( LGLVH.LE.0 ) THEN
C
        IF ( FIRST ) THEN
C
C ****  1  LEADING VERSION NUMBER
C       324 FLOATING POINT (162 ENERGY, 162 TOF)
C       162 INTEGER (NUMBER OF TRACKS)
C       8 FLOATING (ENERGY IN SUPPORTS)
C
          CALL MZFORM('GLVH','1I/324F162I8F',NIO)
          FIRST = .FALSE.
        ENDIF
C
        CALL BKGHIT(LGHIT)
C
C ****  BOOK GLVH BANK HANGING TO GHIT BANK
C ****  TOTAL NUMBER OF LINKS = 4 (NOT DETERMINED WHAT TO LINK TO YET)
C ****  NUMBER OF STRUCTURAL LINK = 4
C ****  NUMBER OF WORDS 496 = 162 ENG + 162 TOF + 162 NUMLV0 + 8 ENGSUP + 1 VERS
C
        NTLNK = 4
        NSLNK = NTLNK
        NWRD = 495
        CALL MZBOOK(IXMAIN,LGLVH,LGHIT,-IZGLVH,'GLVH',
     &    NTLNK,NSLNK,NWRD,NIO,0)
        IQ(LGLVH+1) = 1                   ! Bank version #
      ENDIF
  999 RETURN
      END
