C VAX/DEC CMS REPLACEMENT HISTORY, Element FTRK_QUALITY.FOR
C *1     9-NOV-1993 17:57:42 AVERY "updates in FDC for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FTRK_QUALITY.FOR
      FUNCTION FTRK_QUALITY(TRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set SOME status flags for FDC track quality.
C-
C-   Returned value  : Status word. 
C-      STATUS = 0      Perfect, clean track.
C                       bit  2: Layer 0 missing
C                       bit  3: Layer 1 missing
C                       bit  4: Layer 2 missing
C                       bit  5: Layer 0 Sector crowded (ave hits/wire > 4.)
C                       bit  6: Layer 1 Sector crowded
C                       bit  7: Layer 2 Sector crowded
C                       bit  8: Layer 0 segment used twice 
C                       bit  9: Layer 1 segment used twice 
C                       bit 10: Layer 2 segment used twice 
C                       bit 11: Layer 0 segment is X-sector
C                       bit 12: Layer 1 segment is X-sector
C                       bit 13: Layer 2 segment is X-sector
C                       bit 14: Layer 0 segment Has large slope ( > 1.0 )
C                       bit 15: Layer 1 segment Has large slope
C                       bit 16: Layer 2 segment Has large slope
C                       bit 17: Track doesn't point to beam line (del phi>0.1)
C                       bit 18: Track impact parameter large (r_impact >3.0 cm)
C                       bit 19: Track doesn't point to vertex (del_z >10. cm)
C-                      
C-   Inputs  : TRACK    Track number in FDCT bank.
C-   Outputs : NONE
C-
C-   Created  30-JUN-1992   Robert E. Avery
C-   Updated   9-NOV-1993   Robert E. Avery  Do not use FxDA banks
C-      (they no longer always exist). Use FxSC instead. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER FTRK_QUALITY
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C  Input:
      INTEGER TRACK
C
C  Local:
      INTEGER STATUS 
      INTEGER LRCP,IER                     
      INTEGER LADDER(0:2)
      INTEGER IADD
      INTEGER MODULE,LAYER              
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE,UBIT
      INTEGER SCTR1, SECT, H
      INTEGER LFDCT, GZFDCT
      INTEGER LFXSC, GZFXSC
      INTEGER SEG_STATUS ,GTFSEG
      INTEGER XHIT,NHIT,HIT
      INTEGER NV
C
      LOGICAL FIRST 
      LOGICAL XSECT 
C
      REAL    SLOPE, SLOPE_CUT 
      REAL    AVE_HIT_WIRE, AVE_HIT_CUT  
      REAL    PHI_Z0, PHI, PHI_DIFF, PHI_DIFF_CUT
      REAL    XC,YC,ZC,RC,RIMP_CUT 
      REAL    ZVER(10), DZVER(10), ZVER_CUT          
C
      INTEGER ICONT(62)                 ! CONTENTS OF FSGx BANKS
      REAL CONT(62)                     
      EQUIVALENCE (CONT,ICONT)
C
      REAL QTRAK(26),QHTRK(3,34)        ! TRACK INFORMATION
      INTEGER IQTRAK(26)
      EQUIVALENCE (QTRAK,IQTRAK)
C
      DATA FIRST /.TRUE./
      DATA AVE_HIT_CUT  /4.0/
      DATA SLOPE_CUT /1.0/
      DATA PHI_DIFF_CUT /0.1/
      DATA RIMP_CUT /5.0/
      DATA ZVER_CUT /10.0/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZLOC('FTRAKS_RCP',LRCP)                        
        IF (LRCP.GT.0) THEN                                   
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('FQ_AVE_HIT_CUT',AVE_HIT_CUT,IER)
          CALL EZGET('FQ_SLOPE_CUT',SLOPE_CUT,IER)
          CALL EZGET('FQ_PHI_DIFF_CUT',PHI_DIFF_CUT,IER)
          CALL EZGET('FQ_RIMP_CUT',RIMP_CUT,IER)
          CALL EZGET('FQ_ZVER_CUT',ZVER_CUT,IER)
          CALL EZRSET
        END IF
        FIRST = .FALSE.
      END IF

      STATUS = 0
      LFDCT = GZFDCT(TRACK)
      IF ( LFDCT.LE.0 ) THEN
        STATUS = -1
        GOTO 900
      ENDIF
C
      CALL GTFDCT(TRACK,QTRAK,QHTRK,LADDER)
      HALF = IAND(IQTRAK(1),1)
      DO LAYER =  0, 2
C
C Segment found in layer
C
        IF ( LADDER(LAYER).EQ.0 ) THEN
          STATUS = IBSET(STATUS,2+LAYER)
        ELSE
          MODULE = 3*HALF + LAYER
          SEG_STATUS = GTFSEG(MODULE,LADDER(LAYER),CONT)
          IADD = ICONT(2)
          CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
          NHIT = ICONT(3)
          XHIT = ABS(ICONT(1)/1000)
          XSECT = XHIT.GT.0
          IF ( XSECT ) THEN
            SCTR1 = ICONT(1)/ABS(ICONT(1)) + SCTR
          ENDIF
          IF (LAYER.LE.1) THEN
            SLOPE = CONT(30)
          ELSE
            SLOPE = CONT(55)
          END IF
C
C Uncrowded sector
C
          AVE_HIT_WIRE = 0.0
          DO HIT =  1, NHIT
            IF ( XSECT.AND.(HIT.GE.XHIT) ) THEN
              SECT = SCTR1
            ELSE
              SECT = SCTR
            ENDIF
            LFXSC = GZFXSC(HALF,UNIT,QDRT,SECT)
            WIRE   = CONT(3+HIT)/2.
            AVE_HIT_WIRE = AVE_HIT_WIRE + IQ(LFXSC+4+WIRE)
          ENDDO
          AVE_HIT_WIRE = AVE_HIT_WIRE /NHIT
          IF ( AVE_HIT_WIRE.GT.AVE_HIT_CUT  ) THEN
            STATUS = IBSET(STATUS,5+LAYER)
          ENDIF
C
C Segment used only once
C
          IF ( BTEST(SEG_STATUS,3) ) THEN
            STATUS = IBSET(STATUS,8+LAYER)
          ENDIF
C
C Segment not X-Sect
C
          IF ( XSECT ) THEN
            STATUS = IBSET(STATUS,11+LAYER)
          ENDIF
C
C Segment slope less than cut (1.0)
C
          IF ( ABS(SLOPE).GT.SLOPE_CUT ) THEN
            STATUS = IBSET(STATUS,14+LAYER)
          ENDIF
        ENDIF
      ENDDO
C
C From beam line
C
        PHI_Z0 = ATAN2(QTRAK(5),QTRAK(4))
        IF (PHI_Z0.LT.0.) PHI_Z0 = PHI_Z0+TWOPI
        PHI = QTRAK(6)
        PHI_DIFF = PHI_Z0 - PHI 
        IF (PHI_DIFF.GT.PI) PHI_DIFF = PHI_DIFF - TWOPI
        IF (PHI_DIFF.LT.-PI) PHI_DIFF = PHI_DIFF + TWOPI
        IF (ABS(PHI_DIFF) .GT. 0.1) THEN
          STATUS = IBSET(STATUS,17)
        ENDIF
C
C Impact parameter cut
C
        CALL FGET_CLOSE(TRACK,XC,YC,ZC,RC)
        IF ( RC.GT.RIMP_CUT  ) THEN
          STATUS = IBSET(STATUS,18)
        ENDIF
C
C Vertex cut
C
        CALL ZVERTE(NV, ZVER, DZVER)
        IF ( NV.GE.1 ) THEN
          IF ( ABS(ZVER(1)-ZC) .GE. ZVER_CUT ) THEN
            STATUS = IBSET(STATUS,19)
          ENDIF
        ENDIF
C
  900 CONTINUE
C
      FTRK_QUALITY = STATUS
  999 RETURN
      END
