      SUBROUTINE PS_FONT(IPSFNT)
C-
C-   Modified  8-FEB-1990   Nobuaki Oshima
C-      Fixed the integer overflow problem at IXSZ*IYSZ.
C-
C  Change the postscript font
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      REAL    XYSCAL
      CHARACTER*21 FNTNAM(13)
      DATA FNTNAM/
     &  'Courier              ',
     &  'Courier-Bold         ',
     &  'Courier-Oblique      ',
     &  'Courier-BoldOblique  ',
     &  'Times-Roman          ',
     &  'Times-Bold           ',
     &  'Times-Italic         ',
     &  'Times-BoldItalic     ',
     &  'Helvetica            ',
     &  'Helvetica-Bold       ',
     &  'Helvetica-Oblique    ',
     &  'Helvetica-BoldOblique',
     &  'Symbol               '/
      SAVE
C-------------------------------------
      CALL PS_FORCE
      IFNOW=IPSFNT
      IF(IFNOW.LT.1.OR.IFNOW.GT.13) IFNOW=1
      CALL PS_CRNS(X1,Y1,X2,Y2)
      DY=Y2-Y1
      DV=VMAX-VMIN
      CALL DEV_TRANSF(0.,YSIZE,0.,XDUM,YDUM)
      CALL DEV_TRANSF(0.,0.,0.,XDUM,YDUM2)
      YSZ=ABS(YDUM-YDUM2)
      XSZ=XSIZE*YSZ/(.6*YSIZE)
C-
      IF (XSZ .GT. YSZ) THEN
        XYSCAL = YSZ/XSZ*10.
        XSZ    = XSZ*XYSCAL
        YSZ    = XSZ*1.14
        IF (XSZ .GT. 100.) THEN
          XSZ = XSZ/9.
          YSZ = XSZ*1.14
        ELSEIF (XSZ .LT. 10) THEN
          XSZ = 36.
          YSZ = 41.
        ENDIF
      ENDIF
C-
C      IF(DY*DV.GT.0.) THEN
C        YSZ=.54*YSIZE*DY/DV
C        TYPE *,' YSZ,YDUM:',YSZ,YDUM
C        XSZ=XSIZE*YSZ/(.6*YSIZE)
C      ELSE
C        XSZ=36.
C        YSZ=60.
C      ENDIF
C-
      IXSZ=XSZ
      IYSZ=YSZ
      IF(IXSZ.EQ.0 .OR. IYSZ.EQ.0) RETURN
      IF(IXSZ.GT.1000) RETURN
      IF(IYSZ.GT.1000) RETURN
C-
      WRITE(IDRUNI,10) FNTNAM(IFNOW),IXSZ,IYSZ
  10  FORMAT(' /',A21,' ff [',I5,' 0 0 ',I5,' 0 0] mf sf')
      END
