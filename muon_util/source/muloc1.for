      SUBROUTINE MULOC1(IHMUOH,ITSIGN,IDELT,IPAD,TXYZ,DTXYZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns points on local muon hit
C-           plus errors.
C-
C-   Inputs  : IHMUOH = MUOH point
C              ITSIGN,IDELT,IPAD   time,pad delta time solutions
C-   Outputs : TXYZ,DTXYZ = x,y,z locations and errors of points on segment
C     6/94 David Hedin
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IOR,IWADD
      INTEGER IHMUOH,ITSIGN,IDELT,IPAD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR

      REAL    TXYZ(3),DTXYZ(3)
      REAL    MUDRER,MUDTER,MUPDER,DDIS,PADX,PADER
      REAL    CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,DDIS2,
     &        TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR

      CALL GTMUOH(IHMUOH,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,
     &                  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &                  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)

      IOR=IABS(IORIEN)

      IF(IABS(ITSIGN).EQ.1) THEN
        DDIS=DDIS1*ITSIGN
      ELSE IF(IABS(ITSIGN).EQ.2) THEN
        DDIS=DDIS2*ITSIGN/2
      ENDIF

      PADX=99999.
      PADER=99999.

      IF(IPAD.NE.0) THEN
        PADER=MUPDER(IHMUOH)
        IF(IPAD.LT.0) THEN
          PADX=VERD1+(IABS(IPAD)-1)*60.96
        ELSE
          PADX=VERD2+(IABS(IPAD)-1)*60.96
        ENDIF

      ELSE
        IF(IDELT.NE.0) THEN
          PADER=MUDTER(IHMUOH,IDELT)
          IF(IDELT.EQ.1) PADX=TDIV1
          IF(IDELT.EQ.2) PADX=TDIV2
        ENDIF
      ENDIF

      IF (IOR.EQ.1) THEN
        TXYZ(1)=XCWIR
        TXYZ(2)=YCWIR+PADX
        TXYZ(3)=ZCWIR+DDIS
        DTXYZ(1)=0.
        DTXYZ(2)=PADER
        DTXYZ(3)=MUDRER(IHMUOH,IABS(ITSIGN))   ! drift error
      ENDIF

      IF (IOR.EQ.2) THEN
        TXYZ(1)=XCWIR+PADX
        TXYZ(2)=YCWIR
        TXYZ(3)=ZCWIR+DDIS
        DTXYZ(1)=PADER
        DTXYZ(2)=0.
        DTXYZ(3)=MUDRER(IHMUOH,IABS(ITSIGN))   ! drift error
      ENDIF

      IF (IOR.EQ.3) THEN
        TXYZ(1)=XCWIR+DDIS
        TXYZ(2)=YCWIR+PADX
        TXYZ(3)=ZCWIR
        DTXYZ(1)=MUDRER(IHMUOH,IABS(ITSIGN))
        DTXYZ(2)=PADER
        DTXYZ(3)=0.
      ENDIF

      IF (IOR.EQ.4) THEN
        TXYZ(1)=XCWIR+PADX
        TXYZ(2)=YCWIR+DDIS
        TXYZ(3)=ZCWIR
        DTXYZ(1)=PADER
        DTXYZ(2)=MUDRER(IHMUOH,IABS(ITSIGN))
        DTXYZ(3)=0.
      ENDIF

  999 RETURN
      END
