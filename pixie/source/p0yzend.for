      SUBROUTINE P0YZEND(END,PADS_HIT,NLC,LC_HIT_POSITIONS,PADS_FHIT,
     &  NSC,SC_HIT_POSITIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays one LV0 array in Y-Z view with
C-                         hits and projected FTRAKS points if requested.
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
C-   Outputs : display LV0 scintillator pad hits in Y-Z
C-   Controls: none
C-
C-   Created  29-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER CHANS_HIT(72)
      INTEGER NLC(8,2),NSC(2)
      INTEGER PADS_HIT(-4:4,-4:4,2,2)
      INTEGER PADS_FHIT(-4:4,-4:4,2)
      INTEGER END
      INTEGER ILVL,IHITS,IFHITS,IFPOS
      INTEGER IX,IY,ICNTR,IHIT
      INTEGER IBIN
      INTEGER TYP,IVAL,IER
      INTEGER PAD_TO_CHAN(-4:4,-4:4)
      INTEGER OVLP_TO_CHAN(-4:4,-4:4)
      INTEGER OFFSET
      INTEGER MIFLAG,INTER
      INTEGER MISFLAG,SINTER
      INTEGER ZBIN,NVER,IVER
C
      REAL LC_HIT_POSITIONS(10,8,2,3)
      REAL SC_HIT_POSITIONS(100,2,5)
      REAL WIDPADX,WIDPADY
      REAL DELPADX,DELPADY
      REAL DELTAX,DELTAY
      REAL XPOS,YPOS
      REAL L0Z(2)
      REAL YC,DY
      REAL XUL,YUL,XLR,YLR
      REAL ZOFF
      REAL FASTZ
      REAL SLOWZ,MIQUAL,FULLZ
      REAL SSLOWZ,MISQUAL,FULLSZ
      REAL ZVER(10),DZVER(10)
      REAL CHAN_EFF(72)
C
      LOGICAL L0ONLY
      LOGICAL EZERROR,FIRST,GOODFZ,GOODSZ,GOODSSZ
      EXTERNAL EZERROR
C
      CHARACTER*3 MESSAGE,MESSAGE1,MESSAGE2
      CHARACTER*3 PADCLR,PADONCLR,FHITCLR,HITCLR
      CHARACTER*3 CVER
      CHARACTER*4 MSG
      CHARACTER*4 CVAL,REM
C
      SAVE FIRST,DELPADX,DELPADY,WIDPADX,WIDPADY,DELTAX,DELTAY
      SAVE PAD_TO_CHAN,OVLP_TO_CHAN
      DATA FIRST /.TRUE./
      DATA DELPADX,DELPADY /3.4925,3.4925/
      DATA PAD_TO_CHAN/ 9*32,9*24,
     &                 31,23, 0,18,12,19, 0,21,29,
     &                 31,23,17, 7, 4, 8,20,21,29,
     &                 31,23,11, 3, 0, 1, 9,21,29,
     &                 31,23,16, 6, 2, 5,13,21,29,
     &                 31,23, 0,15,10,14, 0,21,29,
     &                 9*22,9*30/
      DATA OVLP_TO_CHAN/ 31,23,5*0,21,29,
     &                  31,23,5*0,21,29,
     &                  45*0,
     &                  31,23,5*0,21,29,
     &                  31,23,5*0,21,29/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('LEVEL0_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE-l0-no-rcp','P0PADS',
     &                            'Can not find LEVEL0_RCP','W')
          L0Z(1)=-142.3
          L0Z(2)= 142.3
        ELSE
          CALL EZGET('L0Z',L0Z,IER)
          CALL EZRSET
        ENDIF
        WIDPADX = 2.0*DELPADX
        WIDPADY = 2.0*DELPADY
        DELTAX = 0.5*DELPADX
        DELTAY = 0.5*DELPADY
      ENDIF
        CALL EZPICK('PX_LV0DIS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE-l0px-no-rcp','P0PADS',
     &                             'Can not find PX_LV0DIS_RCP','W')
          GOTO 999
        ELSE
        CALL PUGETV('LV0 ONLY',L0ONLY)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW PAD',1,ILVL,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW HIT',1,IHITS,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW FHIT',1,IFHITS,CVAL,TYP,
     &    REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','LV0 DRAW FPOS',1,IFPOS,CVAL,TYP,
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
        IF ( PADCLR.EQ.'   ' ) PADCLR='FOR'
        IF ( PADONCLR.EQ.'   ' ) PADONCLR='RED'
        IF ( FHITCLR.EQ.'   ' ) FHITCLR='RED'
        IF ( HITCLR.EQ.'   ' ) HITCLR='GRE'
        ENDIF
C
C  Draw circles to represent the beampipe at each end and place a
C  end label (N or S) in the center of each circle.
C
      CALL PXCOLR('FOR')
      CALL JJUST(2,2)
      CALL JCIRCL(L0Z(END),0.0,0.0,DELPADX*0.75,0)
      CALL JSIZE(1.3,2.)
      CALL J3MOVE(L0Z(END),0.0,0.0)
      IF (END.EQ.1) THEN
        CALL J3STRG('N')
      ELSE
        CALL J3STRG('S')
      ENDIF
C
C  Draw each pad hit, if requested.
C
      IF ( IHITS.EQ.1 ) THEN
        CALL PXCOLR(PADONCLR)
        OFFSET=(END-1)*36
        DO IX=-4,4
          DO IY=-4,4
            IF ( PADS_HIT(IX,IY,END,1).GT.0 ) THEN
              YC=FLOAT(IY)*WIDPADY
              DY=DELPADY*0.9
              ZOFF=0.0
              IF ( ABS(IY).GT.2 ) ZOFF=-4.0
              IF ( ABS(IX).GT.2 ) ZOFF=4.0
              IF ( END.EQ.1 ) ZOFF=ZOFF*(-1.0)
              XUL=L0Z(END)+ZOFF+1.0
              YUL=YC+DY
              XLR=L0Z(END)+ZOFF-1.0
              YLR=YC-DY
              CALL PURECT(XUL,YUL,0.0,XLR,YLR)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C  Mark the hit positions of FDC projected tracks, if requested.
C
      IF ( IFPOS.EQ.1 ) THEN
        CALL JSIZE(1.3,2.)
        CALL PXCOLR(HITCLR)
C
        DO IHIT=1,NSC(END)
          YPOS=SC_HIT_POSITIONS(IHIT,END,2)
          IY=INT(SC_HIT_POSITIONS(IHIT,END,5))
          ZOFF=0.0
          IF ( ABS(IY).GT.2 ) ZOFF=-4.0
          IF ( ABS(IX).GT.2 ) ZOFF=4.0
          IF ( END.EQ.1 ) ZOFF=ZOFF*(-1.0)
          CALL J3MOVE(L0Z(END)+ZOFF,YPOS,0.0)
          CALL J3STRG('+')
        ENDDO
C
        DO ICNTR=1,8
          DO IHIT=1,NLC(ICNTR,END)
            YPOS = LC_HIT_POSITIONS(IHIT,ICNTR,END,2)
            ZOFF = -4.0
            IF ( (ICNTR/2).EQ.(FLOAT(ICNTR)/2.) ) ZOFF=4.0
            CALL J3MOVE(L0Z(END)+ZOFF,YPOS,0.0)
            CALL J3STRG('+')
          ENDDO
        ENDDO
      ENDIF
C
C  Mark the location of the FASTZ, Slow Z, and Slower Z vertex positions.
C  Mark the Z vertex positions found by CDC reconstruction.
C
      CALL JSIZE(2.6,4.)
      CALL L0_FASTZ_VERTEX(FASTZ,GOODFZ)
      IF ( GOODFZ ) THEN
        CALL J3MOVE(FASTZ,0.0,0.0)
        CALL J3STRG('F')
      ELSE
        CALL J3MOVE(FASTZ,0.0,0.0)
        CALL J3STRG('B')
      ENDIF
C
      CALL L0_SLOW_VERTEX(SLOWZ,MIFLAG,MIQUAL,INTER,GOODSZ,FULLZ,
     &                                                    CHAN_EFF)
      IF ( GOODSZ ) THEN
        IF ( MIFLAG.LT.0 .OR. MIFLAG.GT.3 ) MIFLAG=9
        WRITE(MSG,1002) MIFLAG
 1002   FORMAT(1X,'S',I1,1X)
        CALL J3MOVE(SLOWZ,0.0,0.0)
        CALL J3STRG(MSG)
      ELSE
        CALL J3MOVE(SLOWZ,0.0,0.0)
        CALL J3STRG('SB')
      ENDIF
C
      CALL L0_SLOWER_VERTEX(SSLOWZ,MISFLAG,MISQUAL,SINTER,GOODSSZ,
     &                                              FULLSZ,CHAN_EFF)
      IF ( GOODSSZ ) THEN
        IF ( MISFLAG.LT.0 .OR. MISFLAG.GT.3 ) MISFLAG=9
        WRITE(MSG,1003) MISFLAG
 1003   FORMAT(1X,'T',I1,1X)
        CALL J3MOVE(SSLOWZ,0.0,0.0)
        CALL J3STRG(MSG)
      ELSE
        CALL J3MOVE(SSLOWZ,0.0,0.0)
        CALL J3STRG('TB')
      ENDIF
C
      IF ( L0ONLY ) THEN
        CALL ZVERTE(NVER,ZVER,DZVER)
        IF ( NVER.GT.9 ) NVER=9
        DO IVER=1,NVER
          CALL J3MOVE(ZVER(IVER),0.0,0.0)
          WRITE(CVER,1004) IVER
          CALL J3STRG(CVER)
        ENDDO
        IF ( NVER.EQ.0 ) THEN
          CALL J3MOVE(0.0,0.0,0.0)
          CALL J3STRG('N')
        ENDIF
      ENDIF
 1004 FORMAT(1X,I1,1X)
C----------------------------------------------------------------------
  999 RETURN
      END
