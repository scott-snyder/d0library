      SUBROUTINE BKLTRK(LLTRK,NCDC,NFDC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Book packed CDC tracks for micro DST/STA
C-
C-   Inputs  : NCDC, NFDC number of CDC and FDC tracks
C-   Outputs : LLTRK pointer
C-   Controls:
C-
C-   Created   7-JUL-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZLTRK.LINK'
      INTEGER LLTRK, NCDC, NFDC, IOH, LMDST, GZMDST, NR
      LOGICAL FIRST
      DATA FIRST / .TRUE. /, NR /6/
C----------------------------------------------------------------------
C  Initialize
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('LTRK','4I / 3F 3B',IOH)
      ENDIF
C
      LMDST = GZMDST()
      IF ( LMDST.EQ.0 ) THEN
        CALL MDSTFL()
      ENDIF
C
      CALL MZBOOK(IXMAIN,LLTRK,LMDST,-IZLTRK,'LTRK',
     &            0,0,NR*(NCDC+NFDC)+4,IOH,0)
C
      IQ(LLTRK+1) = 1       ! version number
      IQ(LLTRK+2) = NR      ! repetition number
      IQ(LLTRK+3) = NCDC    ! number of CDC tracks
      IQ(LLTRK+4) = NFDC    ! number of FDC tracks
C
  999 RETURN
      END
