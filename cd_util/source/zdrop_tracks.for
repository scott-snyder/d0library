      FUNCTION ZDROP_TRACKS() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : drop banks ZTRK for tracks not belonging 
C-   to any roads; also drop banks VTXT, DTRK, FDCT pointing to them. 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-FEB-1991   Daria Zieminska
C-   Updated  20-MAR-1991   Qizhong Li-Demarteau  fixed track number in the
C-                         banks, flag in DTRH and added a check for LZTRH
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  30-JUL-1991   Jeffrey Bantly  fix IDONE bug 
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  26-SEP-1991   Qizhong Li-Demarteau  added switches for the
C-                                                track dropping
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  removed machine_block 
C-   Updated  24-SEP-1993   Liang-ping Chen set a bit for VTXT in phi road 
C-                                      so they will not be dropped from DST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$LINKS:IZZTRK.LINK'
      INTEGER IER,ICONT(10),ITRACK,NTRACK,GZZTRK,GZZTRH
      INTEGER LZTRH,LZTRK,LVTXT,LDTRK,LFDCT,LZFIT
      INTEGER LDTRH,GZDTRH,LFTRH,GZFTRH,LVTRH,GZVTRH,LOC 
      LOGICAL ZDROP_TRACKS,INROAD,INPHI
      LOGICAL FIRST, EZERROR
      LOGICAL DROP_ZTRK, DROP_DTRK, DROP_VTXT, DROP_FDCT
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      ZDROP_TRACKS = .TRUE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZDROP_TRACKS',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DROP_ZTRK_OUT_ROADS',DROP_ZTRK,IER)
        CALL EZGET('DROP_DTRK_OUT_ROADS',DROP_DTRK,IER)
        CALL EZGET('DROP_VTXT_OUT_ROADS',DROP_VTXT,IER)
        CALL EZGET('DROP_FDCT_OUT_ROADS',DROP_FDCT,IER)
        CALL EZRSET
      ENDIF
C
      IF (.NOT.DROP_ZTRK) GOTO 200
      CALL GTZTRH(ICONT)
      LZTRH=GZZTRH()
      IF (LZTRH .LE. 0) RETURN
      IQ(LZTRH)=IBCLR(IQ(LZTRH),12)
      NTRACK=ICONT(2)
      IF (NTRACK.EQ.0) GO TO 200
C
C  Drop ZTRK banks outside roads
C
      DO 100 ITRACK=1,NTRACK
        LZTRK=GZZTRK(ITRACK)
        IF (LZTRK.GT.0) THEN
          INROAD=(IBITS(IQ(LZTRK),MUROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LZTRK),ELROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LZTRK),TAUROAD,1).EQ.1) 
          IF (.NOT.INROAD) THEN
            IQ(LZTRH+2) = IQ(LZTRH+2) - 1
            CALL MZDROP(IXCOM,LZTRK,' ')
          ENDIF
        ENDIF
  100 CONTINUE 
C
C  Drop CDC tracks outside roads
C
  200 CONTINUE
      IF (.NOT. DROP_DTRK) GOTO 400
      LDTRH=GZDTRH()
      IF (LDTRH.LE.0) GO TO 400
      IQ(LDTRH) = IBCLR(IQ(LDTRH),IDONED)
      LDTRK=LQ(LDTRH-1)      
      IF (LDTRK.LE.0) GO TO 400
 300  CONTINUE 
      INROAD=(IBITS(IQ(LDTRK),MUROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LDTRK),ELROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LDTRK),TAUROAD,1).EQ.1) 
      LOC=LDTRK
      LDTRK=LQ(LDTRK)
      IF (.NOT.INROAD) THEN
        IQ(LDTRH+2) = IQ(LDTRH+2) - 1
        CALL MZDROP(IXCOM,LOC,' ')
      ENDIF
      IF (LDTRK.LE.0) GOTO 400
      GOTO 300
  400 CONTINUE
C
C  Drop FDC tracks outside roads
C
      IF (.NOT. DROP_FDCT) GOTO 700
      LFTRH=GZFTRH()
      IF (LFTRH.LE.0) GO TO 700
      IQ(LFTRH)=IBCLR(IQ(LFTRH),IDONEF)
      LFDCT=LQ(LFTRH-1)      
      IF (LFDCT.LE.0) GO TO 700
 600  CONTINUE 
      INROAD=(IBITS(IQ(LFDCT),MUROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LFDCT),ELROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LFDCT),TAUROAD,1).EQ.1) 
      LOC=LFDCT
      LFDCT=LQ(LFDCT)
      IF (.NOT.INROAD) THEN
        IQ(LFTRH+2) = IQ(LFTRH+2) - 1
        CALL MZDROP(IXCOM,LOC,' ')
      ENDIF
      IF (LFDCT.LE.0) GOTO 700
      GOTO 600
  700 CONTINUE
C
C  Drop VTX tracks outside roads
C
      IF (.NOT. DROP_VTXT) GOTO 1000
      LVTRH=GZVTRH()
      IF (LVTRH.LE.0) GO TO 1000
      IQ(LVTRH)=IBCLR(IQ(LVTRH),IDONEV)
      LVTXT=LQ(LVTRH-1)      
      IF (LVTXT.LE.0) GO TO 1000
C
C  set a bit for VTXT in PHI roads and do not drop them from DST
C
      CALL VSETPHI 
 900  CONTINUE 
      INROAD=(IBITS(IQ(LVTXT),MUROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LVTXT),ELROAD,1).EQ.1).OR. 
     X   (IBITS(IQ(LVTXT),TAUROAD,1).EQ.1) 
      INPHI=IBITS(IQ(LVTXT+1),VPHIBIT,1).EQ.1
      LOC=LVTXT
      LVTXT=LQ(LVTXT)
      IF (.NOT.INROAD.AND..NOT.INPHI) THEN
        IQ(LVTRH+2) = IQ(LVTRH+2) - 1
        CALL MZDROP(IXCOM,LOC,' ')
      ENDIF
      IF (LVTXT.LE.0) GOTO 1000
      GOTO 900
 1000 CONTINUE
C      LZTRH=GZZTRH()
C      IF (LZTRH.EQ.0) GO TO 999
C      LZTRK = LQ(LZTRH - IZZTRK)
C      IF (LZTRK.EQ.0) GO TO 999
C      CALL ZPRESS(IXCOM,LZTRK)
C      LZTRH=GZZTRH()
C      LZTRK = LQ(LZTRH - 1)
C      LVTXT = LQ(LZTRK - 6)
C      IF (LVTXT.GT.0) CALL ZPRESS(IXCOM,LVTXT)
C      LZTRH=GZZTRH()
C      LZTRK = LQ(LZTRH - 1)
C      LDTRK = LQ(LZTRK - 7)
C      IF (LDTRK.GT.0) CALL ZPRESS(IXCOM,LDTRK)
C      LZTRH=GZZTRH()
C      LZTRK = LQ(LZTRH - 1)
C      LFDCT = LQ(LZTRK - 8)
C      IF (LFDCT.GT.0) CALL ZPRESS(IXCOM,LFDCT)
  999 RETURN
      END
