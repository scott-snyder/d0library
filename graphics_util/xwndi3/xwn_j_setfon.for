      SUBROUTINE J_SETFON(IFNTC)
      INCLUDE 'SYS$LIBRARY:DECW$XLIBDEF.FOR'
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*38 FNAMES(14)
      DATA FNAMES/
     &  '-ADOBE-COURIER-MEDIUM-R_______________',
     &  '-ADOBE-COURIER-BOLD-R_________________',
     &  '-ADOBE-COURIER-MEDIUM-O_______________',
     &  '-ADOBE-COURIER-BOLD-O_________________',
     &  '-ADOBE-TIMES-MEDIUM-R_________________',
     &  '-ADOBE-TIMES-BOLD-R___________________',
     &  '-ADOBE-TIMES-MEDIUM-I_________________',
     &  '-ADOBE-TIMES-BOLD-I___________________',
     &  '-ADOBE-HELVETICA-MEDIUM-R_____________',
     &  '-ADOBE-HELVETICA-BOLD-R_______________',
     &  '-ADOBE-HELVETICA-MEDIUM-O_____________',
     &  '-ADOBE-HELVETICA-BOLD-O_______________',
     &  '-ADOBE-SYMBOL-MEDIUM-R-NORMAL--*-_____',
     &  '-ADOBE-NEW CENTURY SCHOOLBOOK-MEDIUM-R'/
      CHARACTER*11 STR2
      DATA STR2/'-NORMAL--*-'/
      INTEGER APOINT(6)
      DATA APOINT/8.,10.,12.,14.,18.,24./
      CHARACTER*3 SPOINT(6)
      DATA SPOINT/'80 ','100','120','140','180','240'/
      CHARACTER*18 STR3PM(2)
      DATA STR3PM/'-*-*-P-*-ISO8859-1','-*-*-M-*-ISO8859-1'/
      CHARACTER*27 STRSYM
      DATA STRSYM/'-*-*-P-*-ADOBE-FONTSPECIFIC'/
      INTEGER IS(14)
      DATA IS/4*2,10*1/
      CHARACTER*90 CFNAME
      CHARACTER*1 CH
      CHARACTER*3 SP
      DATA YFSCAL/600./
C--------------------------------------
      DY=VMAX-VMIN
      IF(DY.GT.0.) THEN
        BP=YSIZE*YFSCAL/DY
        IF(BP.LT.APOINT(1)) THEN
          ISZ=1
        ELSEIF(BP.GT.APOINT(6)) THEN
          ISZ=6
        ELSE
          DO I=1,5
            ALO=APOINT(I)
            AHI=APOINT(I+1)
            IF(BP.GE.ALO.AND.BP.LE.AHI) THEN
              IF(ABS(BP-ALO).LT.ABS(BP-AHI)) THEN
                ISZ=I
              ELSE
                ISZ=I+1
              ENDIF
              GO TO 10
            ENDIF
          ENDDO
        ENDIF
  10    CONTINUE
C        TYPE *,' IFC,YSIZE,DY,BP,ISZ'
C        TYPE *,IFC,YSIZE,DY,BP,ISZ
      ELSE
        ISZ=3
      ENDIF
C
C!!!8 point seems to be missing in SYS$LIBRARY
      IF(ISZ.EQ.1) ISZ=2
C
      IFC=IFNTC
      IF(IFC.LT.1.OR.IFC.GT.14) IFC=1
      L=3
      IF(ISZ.EQ.1) L=2
      SP=SPOINT(ISZ)
      IF(IFC.NE.13) THEN
        CFNAME=FNAMES(IFC)//STR2//SP(1:L)//STR3PM(IS(IFC))
      ELSE
        CFNAME=FNAMES(IFC)//SP(1:L)//STRSYM
      ENDIF
      J=0
      DO I=1,70
        CH=CFNAME(I:I)
        IF(CH.NE.'_') THEN
          J=J+1
          CFNAME(J:J)=CH
        ENDIF
      ENDDO
      IF(J+1.LT.70) THEN
        DO I=J+1,70
          CFNAME(I:I)=' '
        ENDDO
      ENDIF
C      TYPE *,' --->',CFNAME(1:J)
      KFONT=X$LOAD_FONT(VD_ID,CFNAME(1:J))
      CALL X$SYNC(VD_ID,DISCARD)
      CALL X$SET_FONT(VD_ID,ATB(IFONT),KFONT)
      CALL X$SYNC(VD_ID,DISCARD)
      END
