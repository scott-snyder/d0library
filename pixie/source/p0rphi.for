      SUBROUTINE P0RPHI(END,PADS_HIT,NLC,LC_HIT_POSITIONS,PADS_FHIT,NSC,
     &  SC_HIT_POSITIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays one LV0 array in R-Phi view
C-                         with hits and projected FTRAKS points,
C-                         if requested.
C-
C-   Inputs  : END = (1=North, 2=South) Level 0 array
C-             PADS_HIT(x pos,y pos,end) = (0=no valid hit in counter,
C-                                          1= valid hit in counter,
C-                                          2= both chan w valid hit
C-                                             long counters)
C-             NLC(1:8=Long counter,1:2=END) = number of FTRAKS passing
C-                                             through the long counter
C-             LC_HIT_POSITIONS(hit no,cntr no,end,dataword) =
C-                           dataword=1 -> FTRAKS x position at long cntr
C-                           dataword=2 -> FTRAKS y position at long cntr
C-                           dataword=3 -> FTRAKS equiv short pad row at LC
C-             PADS_FHIT(x pos,y pos,end) = number of FTRAKS passing through
C-                                          the scintillator pad
C-             NSC(1:2=END) = number of FTRAKS passing through the short
C-                            counters at an end
C-             SC_HIT_POSITIONS(hit no,end,dataword) =
C-                           dataword=1 -> FTRAKS x position in short cntr
C-                           dataword=2 -> FTRAKS y position in short cntr
C-                           dataword=3 -> short counter channel number
C-                           dataword=4 -> FTRAKS equiv short pad num in x
C-                           dataword=5 -> FTRAKS equiv short pad num in y
C-
C-   Outputs : display LV0 scintillator pad hits in R-Phi, one end
C-   Controls:
C-
C-   Created  29-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER VERSION
      INTEGER CHANS_HIT(72)
      INTEGER NLC(8,2),NSC(2)
      INTEGER PADS_HIT(-4:4,-4:4,2,2)
      INTEGER PADS_FHIT(-4:4,-4:4,2)
      INTEGER END
      INTEGER ILVL,IHITS,IFHITS,IFPOS
      INTEGER IX,IY,ICNTR,IHIT
      INTEGER IBIN,ICHHT
      INTEGER TYP,IVAL,IER
      INTEGER PAD_TO_CHAN(-4:4,-4:4)
      INTEGER OVLP_TO_CHAN(-4:4,-4:4)
      INTEGER PAD_TO_CHAN2(-4:4,-4:4)
      INTEGER OFFSET
      INTEGER IFILL,ICHG,IUSE
      INTEGER NLABEL,I
C
      REAL LC_HIT_POSITIONS(10,8,2,3)
      REAL SC_HIT_POSITIONS(100,2,5)
      REAL WIDPADX,WIDPADY
      REAL DELPADX,DELPADY
      REAL DELTAX,DELTAY
      REAL XPOS,YPOS
      REAL L0Z(2)
      REAL XC,YC,DX,DY,XUL,YUL,XLR,YLR
      REAL OFFX,OFFY
      REAL CHARGE_SCALE
C
      LOGICAL L0ONLY
      LOGICAL EZERROR,FIRST
      EXTERNAL EZERROR
C
      CHARACTER*3 PADCLR,PADONCLR,FHITCLR,HITCLR
      CHARACTER*3 COLRSCL(0:5),COL2SCL(0:5),GREYSCL(0:5)
      CHARACTER*4 MESSAGE,MESSAGE1,MESSAGE2
      CHARACTER*4 CVAL,REM
      CHARACTER*3 LEGDCOL(6)
      CHARACTER*15 LABELS(6)
C
      SAVE FIRST,DELPADX,DELPADY,WIDPADX,WIDPADY,DELTAX,DELTAY
      SAVE PAD_TO_CHAN,OVLP_TO_CHAN,PAD_TO_CHAN2,VERSION
      DATA FIRST /.TRUE./
      DATA DELPADX,DELPADY /3.4925,3.4925/
      DATA PAD_TO_CHAN/ 9*32,9*24,
     &                 31,23, 0,18,12,19, 0,21,29,
     &                 31,23,17, 7, 4, 8,20,21,29,
     &                 31,23,11, 3,-1, 1, 9,21,29,
     &                 31,23,16, 6, 2, 5,13,21,29,
     &                 31,23, 0,15,10,14, 0,21,29,
     &                 9*22,9*30/
      DATA OVLP_TO_CHAN/ 31,23,5*0,21,29,
     &                  31,23,5*0,21,29,
     &                  45*0,
     &                  31,23,5*0,21,29,
     &                  31,23,5*0,21,29/
      DATA PAD_TO_CHAN2/ 32, 0, 0, 0,36, 0, 0, 0,29,
     &                    0,24, 0, 0,28, 0, 0,21, 0,
     &                    0, 0, 0,18,12,19, 0, 0, 0,
     &                    0, 0,17, 7, 4, 8,20, 0, 0,
     &                    35,27,11, 3,-1, 1, 9,25,33,
     &                    0, 0,16, 6, 2, 5,13, 0, 0,
     &                    0, 0, 0,15,10,14, 0, 0, 0,
     &                    0,23, 0, 0,26, 0, 0,22, 0,
     &                    31, 0, 0, 0,34, 0, 0, 0,30/
      DATA COLRSCL/'WHI','WHI','RED','YEL','GRE','BLU'/
      DATA COL2SCL/'WHI','DBL','CYA','DGR','DRE','ORA'/
      DATA GREYSCL/'BGR','DRE','CYA','GRE','RED','BLU'/
      DATA LABELS/'   0.<ADC<150.',' 150.<ADC<300.',' 300.<ADC<450.',
     &            ' 450.<ADC<600.',' 600.<ADC<750.',' 750.<ADC'/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('LEVEL0_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE-l0-no-rcp','P0PADS',
     &                              'Can not find LEVEL0_RCP','W')
          L0Z(1)=-142.3
          L0Z(2)= 142.3
        ELSE
          CALL EZGET('L0Z',L0Z,IER)
          CALL EZGET('CHARGE_SCALE',CHARGE_SCALE,IER)
          CALL EZRSET
        ENDIF
        WIDPADX = 2.0*DELPADX
        WIDPADY = 2.0*DELPADY
        DELTAX = 0.5*DELPADX
        DELTAY = 0.5*DELPADY
        IF ( CHARGE_SCALE.LE. 0.0 ) CHARGE_SCALE = 150.
        DO I=1,5
          WRITE(LABELS(I),1002) FLOAT(I-1)*CHARGE_SCALE,
     &                                          FLOAT(I)*CHARGE_SCALE
        ENDDO
        WRITE(LABELS(6),1003) (5.)*CHARGE_SCALE
 1002   FORMAT(1X,F4.0,'<ADC<',F4.0)
 1003   FORMAT(1X,F4.0,'<ADC')
C
        CALL L0_GET_VERSION(VERSION)
        IF ( VERSION.NE.2 ) VERSION = 1
      ENDIF
C
C  Fetch possibly revised values from PX_LV0DIS.RCP.
C
      CALL EZPICK('PX_LV0DIS_RCP')
      IF( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE-l0px-no-rcp','P0PADS',
     &                            'Can not find PX_LV0DIS_RCP','W')
        GOTO 999
      ELSE
        CALL PUGETV('LV0 ONLY',L0ONLY)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW CHAN HIT',1,ICHHT,CVAL,
     &    TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW PAD',1,ILVL,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW HIT',1,IHITS,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW FHIT',1,IFHITS,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW FPOS',1,IFPOS,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 USE COLR',1,IUSE,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 COLR PAD',1,IVAL,PADCLR,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 COLR PADON',1,IVAL,PADONCLR,
     &    TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 COLR FHIT',1,IVAL,FHITCLR,
     &    TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 COLR FPOS',1,IVAL,HITCLR,
     &    TYP,REM,IER)
        CALL EZRSET
      ENDIF
      IF ( PADCLR.EQ.'   ' ) PADCLR='FOR'
      IF ( PADONCLR.EQ.'   ' ) PADONCLR='RED'
      IF ( FHITCLR.EQ.'   ' ) FHITCLR='RED'
      IF ( HITCLR.EQ.'   ' ) HITCLR='GRE'
      IF ( IUSE.LE.1 ) THEN
        DO I=1,6
          LEGDCOL(I)=GREYSCL(I)
        ENDDO
      ELSE
        DO I=1,6
          LEGDCOL(I)=COLRSCL(I)
        ENDDO
      ENDIF
C
C  Draw label and X-Y axes, if L0 only diplayed.
C
      CALL JJUST(2,2)
      IF ( L0ONLY ) THEN
        IF ( END.EQ.1 ) THEN
          CALL PUVSTR( 0.0, 45.0, 3., 1.5, 'Level 0 North Array')
        ELSE
          CALL JBASE(-1.,0.,0.)
          CALL PUVSTR( 0.0, 45.0, 3., 1.5, 'Level 0 South Array')
        ENDIF
        CALL J3MOVE(40.,-40.,L0Z(END))
        CALL J3DRAW(50.,-40.,L0Z(END))
        CALL J3MOVE(55.,-40.,L0Z(END))
        CALL J3STRG('X')
        CALL J3MOVE(40.,-40.,L0Z(END))
        CALL J3DRAW(40.,-30.,L0Z(END))
        CALL J3MOVE(40.,-25.,L0Z(END))
        CALL J3STRG('Y')
      ENDIF
C
C  Draw the Level 0 Array for the chosen end, if requested.
C
      IF ( ILVL.EQ.1 ) THEN
        CALL PXCOLR(PADCLR)
        DO IX=-4,4
          DO IY=-4,4
            IF ( VERSION.EQ.2 ) THEN
              IF ( PAD_TO_CHAN2(IX,IY).GT.0 ) THEN
                XC=IX*WIDPADX
                YC=IY*WIDPADY
                DX=DELPADX
                DY=DELPADY
                XUL=XC+DX
                YUL=YC+DY
                XLR=XC-DX
                YLR=YC-DY
                CALL PURECT(XUL,YUL,L0Z(END),XLR,YLR)
              ENDIF
            ELSEIF ( ABS(IX).LE.2 .AND. ABS(IY).LE.2 ) THEN
              XC=IX*WIDPADX
              YC=IY*WIDPADY
              DX=DELPADX
              DY=DELPADY
              XUL=XC+DX
              YUL=YC+DY
              XLR=XC-DX
              YLR=YC-DY
              CALL PURECT(XUL,YUL,L0Z(END),XLR,YLR)
            ELSE
C              XC=IX*WIDPADX
C              YC=0.0
C              DX=DELPADX
C              DY=WIDPADY*4.5
C              XUL=XC+DX
C              YUL=YC+DY
C              XLR=XC-DX
C              YLR=YC-DY
C              CALL PURECT(XUL,YUL,L0Z(END),XLR,YLR)
            ENDIF
          ENDDO
        ENDDO
        XC=0.0
        YC=0.0
        DX=WIDPADX*4.5
        DY=WIDPADY*4.5
        XUL=XC+DX
        YUL=YC+DY
        XLR=XC-DX
        YLR=YC-DY
        CALL PURECT(XUL,YUL,L0Z(END),XLR,YLR)
C
C  Mark the empty spots in the array with a large X.
C
C        DO IX=-2,2,4
C          DO IY=-2,2,4
C            CALL J3MOVE(IX*WIDPADX-DELPADX,IY*WIDPADY+DELPADY,L0Z(END))
C            CALL J3DRAW(IX*WIDPADX+DELPADX,IY*WIDPADY-DELPADY,L0Z(END))
C            CALL J3MOVE(IX*WIDPADX+DELPADX,IY*WIDPADY+DELPADY,L0Z(END))
C            CALL J3DRAW(IX*WIDPADX-DELPADX,IY*WIDPADY-DELPADY,L0Z(END))
C          ENDDO
C        ENDDO
C        IF ( VERSION.EQ.2 ) THEN 
C          DO IX=-4,4
C            DO IY=-4,4
C              IF ( PAD_TO_CHAN2(IX,IY).EQ.0 ) THEN
C                CALL J3MOVE(IX*WIDPADX-DELPADX,IY*WIDPADY+DELPADY,
C     &                                                   L0Z(END))
C                CALL J3DRAW(IX*WIDPADX+DELPADX,IY*WIDPADY-DELPADY,
C     &                                                   L0Z(END))
C                CALL J3MOVE(IX*WIDPADX+DELPADX,IY*WIDPADY+DELPADY,
C     &                                                   L0Z(END))
C                CALL J3DRAW(IX*WIDPADX-DELPADX,IY*WIDPADY-DELPADY,
C     &                                                   L0Z(END))
C              ENDIF
C            ENDDO
C          ENDDO
C        ENDIF
      ENDIF
C
C  Draw a beampipe in the center of the array and label it with the end
C  direction (North or South).
C
      CALL PXCOLR('BLU')
      CALL JCIRCL(0.0,0.0,L0Z(END),DELPADX*0.75,0)
      CALL JSIZE(1.3,2.)
      CALL J3MOVE(0.0,0.0,L0Z(END))
      IF (END.EQ.1) THEN
        CALL J3STRG('N')
      ELSE
        CALL J3STRG('S')
      ENDIF
      CALL PXCOLR(PADCLR)
C
C  Draw the pads that have been hit.  Label them with their corresponding
C  channel number for short counters and both channels for long counters.
C  Long counter channel numbers are drawn separated to put them closer to
C  the end of the counter to which the corresponding channel is attached.
C  Done if requested.
C
      IF ( IHITS.EQ.1 ) THEN
        CALL PXCOLR(PADONCLR)
        CALL JSIZE(1.,2.)
        OFFSET=(END-1)*36
        DO IX=-4,4
          DO IY=-4,4
            IF ( PADS_HIT(IX,IY,END,1).GT.0 ) THEN
              XC=IX*WIDPADX
              YC=IY*WIDPADY
              IF ( ABS(IY).LE.2 .OR. VERSION.EQ.2 ) THEN
                DX=DELPADX*0.8
              ELSE
                DX=WIDPADX*4.0+DELPADX*0.8
              ENDIF
              IF ( ABS(IX).LE.2.OR. VERSION.EQ.2 ) THEN
                DY=DELPADY*0.8
              ELSE
                DY=WIDPADY*4.0+DELPADY*0.8
              ENDIF
              XUL=XC+DX
              YUL=YC+DY
              XLR=XC-DX
              YLR=YC-DY
              CALL PXCOLR(PADONCLR)
              CALL PURECT(XUL,YUL,L0Z(END),XLR,YLR)
              IF ( (ABS(IX).LE.2.AND.ABS(IY).LE.2).OR.VERSION.EQ.2 ) 
     &                   THEN
                ICHG=PADS_HIT(IX,IY,END,2)
                IF ( ICHG.LT.0 ) ICHG=0
                IF ( ICHG.GT.5 ) ICHG=5
                IF ( IUSE.LE.1 ) THEN
                  CALL PXCOLFILL(GREYSCL(ICHG))
                ELSE
C                  IF ( END.EQ.1 ) THEN
                  CALL PXCOLFILL(COLRSCL(ICHG))
C                    IF ( IY.EQ.1 )  CALL PXCOLFILL(COLRSCL(0))
C                  ELSE
C                    CALL PXCOLFILL(COL2SCL(ICHG))
C                    IF ( IY.EQ.1 )  CALL PXCOLFILL(COL2SCL(0))
C                  ENDIF
                ENDIF
                CALL PURECT(XUL,YUL,L0Z(END),XLR,YLR)
                CALL JPINTR(0)
              ENDIF
              IF ( ICHHT.GE.1 ) THEN
                CALL PXCOLR(PADCLR)
                ICHG=PADS_HIT(IX,IY,END,2)
                IF ( VERSION.EQ.2 ) THEN
                  WRITE(MESSAGE,1001) PAD_TO_CHAN2(IX,IY)+OFFSET
                  CALL J3MOVE(XC,YC,L0Z(END))
                  CALL J3STRG(MESSAGE)
                ELSEIF ( ABS(IY).LE.2.AND.ABS(IX).LE.2 ) THEN
                  WRITE(MESSAGE,1001) PAD_TO_CHAN(IX,IY)+OFFSET
                  CALL J3MOVE(XC,YC,L0Z(END))
                  CALL J3STRG(MESSAGE)
                ELSE
                  WRITE(MESSAGE1,1001) PAD_TO_CHAN(IX,IY)+OFFSET
                  WRITE(MESSAGE2,1001) PAD_TO_CHAN(IX,IY)+OFFSET+4
                  OFFX = 0.0
                  OFFY = 0.0
                  IF ( IX.GE.3 )  OFFY = -DELTAY
                  IF ( IY.GE.3 )  OFFX =  DELTAX
                  IF ( IX.LE.-3 ) OFFY =  DELTAY
                  IF ( IY.LE.-3 ) OFFX = -DELTAX
                  CALL J3MOVE(XC+OFFX,YC+OFFY,L0Z(END))
                  CALL J3STRG(MESSAGE1)
                  CALL J3MOVE(XC-OFFX,YC-OFFY,L0Z(END))
                  CALL J3STRG(MESSAGE2)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        CALL P0LGND(LEGDCOL,LABELS,6)
      ENDIF
C
C   Mark each pad with a number equal to the number of projected FDC
C   tracks that pass through the pad, if requested.
C
      IF ( IFHITS.EQ.1 ) THEN
        CALL PXCOLR(FHITCLR)
        CALL JSIZE(0.75,1.2)
        DO IX=-4,4
          DO IY=-4,4
            IF ( PADS_FHIT(IX,IY,END).GT.0 ) THEN
              CALL J3MOVE(IX*WIDPADX+DELTAX,IY*WIDPADY-DELTAY,L0Z(END))
              IF ( PADS_FHIT(IX,IY,END).LE.99 ) THEN
                WRITE(MESSAGE,1001) PADS_FHIT(IX,IY,END)
              ELSE
                MESSAGE=' 99+'
              ENDIF
              CALL J3STRG(MESSAGE)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
 1001 FORMAT(1X,I2,1X)
C
C   Mark each pad with a '+' at the position of each projected FDC
C   track that passes through the pad, if requested.
C
      IF ( IFPOS.EQ.1 ) THEN
        CALL PXCOLR(HITCLR)
        CALL JSIZE(0.75,1.2)
C
        DO IHIT=1,NSC(END)
          XPOS = SC_HIT_POSITIONS(IHIT,END,1)
          YPOS = SC_HIT_POSITIONS(IHIT,END,2)
          CALL J3MOVE(XPOS,YPOS,L0Z(END))
          CALL J3STRG('+')
        ENDDO
C
        DO ICNTR=1,8
          DO IHIT=1,NLC(ICNTR,END)
            XPOS = LC_HIT_POSITIONS(IHIT,ICNTR,END,1)
            YPOS = LC_HIT_POSITIONS(IHIT,ICNTR,END,2)
            IBIN = LC_HIT_POSITIONS(IHIT,ICNTR,END,3)
            CALL J3MOVE(XPOS,YPOS,L0Z(END))
            CALL J3STRG('+')
          ENDDO
        ENDDO
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
