      SUBROUTINE VHBLOW(ID,IDB,NEWTITLE,TWO_D,XMI,XMA,YMI,YMA,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create histogram IDB with bin limits XMI, XMA (and
C-                         YMI, YMA if TWO_D = .TRUE.) and fill with contents
C-                         from histogram ID.  The new histogram is given the
C-                         title NEWTITLE.
C-
C-   Inputs  : ID: id of source histogram
C-             IDB: id of created histogram
C-             NEWTITLE: title of created histogram
C-             TWO_D [L]: .TRUE. if ID (and IDB) is 2d
C-             XMI, XMA: x limits of created histogram
C-             YMI, YMA: y limits of created histogram
C-   Outputs : Books histogram IDB
C-             OK: .TRUE. if everything ok
C-
C-   Created   3-OCT-1992   Peter M. Grudberg from Ed Oltman's HBLOW
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID,IDB
      REAL    XMI,XMA,YMI,YMA
      REAL    XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,DX,DY
      REAL    XL,YL,X,Y,VAL
      INTEGER NX,NY,N1,N2,IX,IY,NARG
      CHARACTER*(*) NEWTITLE
      CHARACTER*80 TITL
      REAL HXY,HX
      LOGICAL HEXIST,TWO_D, OK
C----------------------------------------------------------------------
      OK = .TRUE.
      IF (.NOT. HEXIST(ID)) THEN
        CALL ERRMSG('ID doesnt exist','VHBLOW',
     &    'Input histogram does not exist','W')
        OK = .FALSE.
        GO TO 999
      ENDIF
      CALL HGIVE(ID,TITL,NX,XMIN,XMAX,NY,YMIN,YMAX,N1,N2)
      IF (TWO_D) THEN
        IF (NY .EQ. 0) THEN
          CALL ERRMSG('Wrong dimension','VHBLOW',
     &      'Input histogram has wrong dimension','W')
          OK = .FALSE.
          GO TO 999
        ENDIF
      ELSE
        IF (NY .NE. 0) THEN
          CALL ERRMSG('Wrong dimension','VHBLOW',
     &      'Input histogram has wrong dimension','W')
          OK = .FALSE.
          GO TO 999
        ENDIF
      ENDIF
      DX = (XMAX-XMIN)/FLOAT(NX)
      IF (TWO_D) DY = (YMAX-YMIN)/FLOAT(NY)
C
C  MAKE SURE REQUEST MAKES SENSE
C
      X1 = AMAX1(XMIN,XMI)
      X2 = AMIN1(XMAX,XMA)
      IF (TWO_D) THEN
        Y1 = AMAX1(YMIN,YMI)
        Y2 = AMIN1(YMAX,YMA)
      ENDIF
      IF (X1 .GT. X2 ) THEN
        CALL ERRMSG('Invalid limits','VHBLOW',
     &    'Requested histogram limits invalid','W')
        OK = .FALSE.
        GO TO 999
      ENDIF
      IF (TWO_D .AND. Y1 .GT. Y2) THEN
        CALL ERRMSG('Invalid limits','VHBLOW',
     &    'Requested histogram limits invalid','W')
        OK = .FALSE.
        GO TO 999
      ENDIF
C
C  NOW LOOK FOR NEW BIN LIMITS WHICH CORRESPOND TO EXISTING BINS
C
      IF (XMI .LT. XMIN) THEN
        X1 = XMIN
      ELSE
        DO IX = 1,NX
          XL = XMIN + DX*FLOAT(IX-1)
          IF (XMI .GE. XL .AND. XMI .LT. XL+DX) GO TO 5
        ENDDO
    5   X1 = XL
      ENDIF
      IF (XMA .GE. XMAX) THEN
        X2 = XMAX
      ELSE
        DO IX = 1,NX
          XL = XMIN + DX*FLOAT(IX-1)
          IF (XMA .GE. XL .AND. XMA .LT. XL+DX) GO TO 6
        ENDDO
    6   X2 = XL + DX
      ENDIF
      IF (TWO_D) THEN
        IF (YMI .LT. YMIN) THEN
          Y1 = YMIN
        ELSE
          DO IY = 1,NY
            YL = YMIN + DY*FLOAT(IY-1)
            IF (YMI .GE. YL .AND. YMI .LT. YL+DY) GO TO 15
          ENDDO
   15     Y1 = YL
        ENDIF
        IF (YMA .GE. YMAX) THEN
          Y2 = YMAX
        ELSE
          DO IY = 1,NY
            YL = YMIN + DY*FLOAT(IY-1)
            IF (YMA .GE. YL .AND. YMA .LT. YL+DY) GO TO 16
          ENDDO
   16     Y2 = YL + DY
        ENDIF
      ENDIF
C
C  NOW BOOK THE NEW HISTO
C
      NX = NINT( (X2-X1)/DX )
      IF (TWO_D) THEN
        NY = NINT( (Y2-Y1)/DY )
        CALL HBOOK2(IDB,NEWTITLE,NX,X1,X2,NY,Y1,Y2,0)
      ELSE
        CALL HBOOK1(IDB,NEWTITLE,NX,X1,X2,0)
      ENDIF
C  AND FILL IT
      DO IX = 1,NX
        X = X1 + DX*(FLOAT(IX)-.5)
        IF (TWO_D) THEN
          DO IY = 1,NY
            Y = Y1 + DY*(FLOAT(IY)-.5)
            VAL = HXY(ID,X,Y)
            CALL HF2(IDB,X,Y,VAL)
          ENDDO
        ELSE
          VAL = HX(ID,X)
          CALL HF1(IDB,X,VAL)
        ENDIF
      ENDDO
  999 RETURN
      END
