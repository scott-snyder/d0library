C=======================================================================
      SUBROUTINE D0HSHW(IDNUM)
C=======================================================================
C
C  Description:  Gives the TITLE of Histogram IDNUM
C  ============
C
C  Author:
C  =========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - August 27,1988
C
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER IDNUM
      INTEGER*4 ITIT(15)
      INTEGER NX,NY,NWT,IAD
      INTEGER TLEN,TRULEN
      REAL XMI,XMA,YMI,YMA
      CHARACTER*60 TITLE
      CHARACTER*5 CID
      CHARACTER*80 OSTRG
      LOGICAL LEXIST,HEXIST
      LOGICAL FLGVAL
      EXTERNAL FLGVAL
C
C  Executable Code:
C  =================
C
      LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
        CALL HGIVE(IDNUM,TITLE,NX,XMI,XMA,NY,YMI,YMA,NWT,IAD)
        TLEN = TRULEN(TITLE)
        WRITE(CID,102) IDNUM
  102   FORMAT(I5)
        OSTRG = ' '//CID//' '//TITLE(1:TLEN)
        CALL INTMSG(OSTRG)
      ELSE
        CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
