      SUBROUTINE D0HINX
C=======================================================================
C
C  Description:  Gives an index of all the histograms booked in blank
C  ============  common.
C
C  Author:
C  ==========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - May 28, 1988
C
C=========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER NMAXH
      INTEGER LIBREP
      PARAMETER(NMAXH=200)                    ! MAXIMUM NO. OF HISTOGRAMS
      INTEGER I,K
      INTEGER N1D,N2D
      INTEGER*4 ITIT(15)
      INTEGER ID1D(NMAXH),ID2D(NMAXH),NX,NY,IAD,NWT,NOENT
      INTEGER CLEN,TRULEN
      REAL XMI,XMA,YMI,YMA
C      CHARACTER*5 CID,ICX,CNOENT
      CHARACTER*5 CNOENT
      CHARACTER*9 ICX,CID
      CHARACTER*80 TIT1D(NMAXH),TIT2D(NMAXH),TIT
      CHARACTER*80 OSTRG
      LOGICAL FLGVAL
      EXTERNAL FLGVAL
C
C  Executable Code:
C  =================
C
      I = LIBREP()
      CALL HID1(ID1D,N1D)
      CALL HID2(ID2D,N2D)
      DO 10 I = 1,N1D
        CALL HGIVE(ID1D(I),TIT1D(I),NX,XMI,XMA,NY,YMI,YMA,NWT,IAD)
        WRITE(ICX,108) ID1D(I)
  108   FORMAT(I9)
        READ(ICX,109) CID
  109   FORMAT(A9)
        CALL HNOENT(ID1D(I),NOENT)
        WRITE(ICX,108) NOENT
        READ(ICX,109) CNOENT
        CLEN = TRULEN(TIT1D(I))
        OSTRG = ' '//CID//' '//TIT1D(I)(1:CLEN)//' N='//CNOENT
        IF (NWT .NE. 0) THEN
          CALL INTMSG(OSTRG)
        ENDIF
   10 CONTINUE
      DO 20 I = 1,N2D
        CALL HGIVE(ID2D(I),TIT2D(I),NX,XMI,XMA,NY,YMI,YMA,NWT,IAD)
        WRITE(ICX,108) ID2D(I)
        READ(ICX,109) CID
        CALL HNOENT(ID2D(I),NOENT)
        WRITE(ICX,108) NOENT
        READ(ICX,109) CNOENT
        CLEN = TRULEN(TIT2D(I))
        OSTRG = CID//' '//TIT2D(I)(1:CLEN)//' N='//CNOENT
        IF (NWT .NE. 0) THEN
          CALL INTMSG(OSTRG)
        ENDIF
   20 CONTINUE
      RETURN
      END

