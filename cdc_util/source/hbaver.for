      SUBROUTINE HBAVER(ID,TITLE,NBIN,XMIN,XMAX,YLOW,YHIG)
C----------------------------------------------------------------------
C-                                                                     "
C-   Purpose and Methods :                                             "
C       ROUTINE TO COMPUTE THE AVERAGE VALUE OF Y FOR EACH BIN IN A    "
C       HISTOGRAM WHICH IS FILLED USING HBAVFL.  TWO HISTOGRAMS        "
C       AND A SMALL DATA BANK ARE BOOKED FOR EACH ID WHEN HBAVER       "
C       IS CALLED.  HBAVDP DOES THE FINAL PROCESSING FOR THE           "
C       HISTOGRAMS BEFORE THEY ARE PRINTED WITH A CALL TO HISTGO       "
C       OR THE LIKE.  AFTER THIS CALL TO HBAVDP THESE AVERAGE          "
C       HISTOGRAMS CAN NO LONGER BE FILLED.  HISTOGRAMS ARE FILLED     "
C       BY CALLING HBAVFL OR HBAVFW.                                   "
C                                                                      "
C-   Inputs  :                                                         "
C       PARAMETERS TO HBAVER:                                          "
C            ID    = HISTOGRAM ID         --.                          "
C            TITLE = HISTO TITLE            !                          "
C            NBIN  = NUMBER OF BINS         !> AS IN HBOOK1            "
C            XMIN  = LOWER END OF HISTO     !                          "
C            XMAX  = UPPER END OF HISTO   __!                          "
C            YLOW  = Y VALUE LESS THAN YLOW NOT USED IN AVERAGE        "
C            YHIG  = Y VALUE GREATER THAN YHIG NOT USED IN AVERAGE     "
C       ALL OF THE PARAMETERS TO HBAVDP ARE DUMMY.                     "
C                                                                      "
C       THE DATA BANK HAS THE FOLLOWING FORMAT:                        "
C            LOC+1   ID OF HISTO OF X WEIGHTED WITH Y*W                "
C            LOC+2   ID OF HISTO OF X WEIGHTED WITH Y**2*W             "
C            LOC+3   ID OF HISTO OF X WEIGHTED WITH W                  "
C            LOC+4   ID OF HISTO OF X WEIGHTED WITH W**2               "
C            LOC+5   LOWER LIMIT ON Y                                  "
C            LOC+6   UPPER LIMIT ON Y                                  "
C            LOC+7   NUMBER OF BINS                                    "
C                                                                      "
C-   13-AUG-1988   Qizhong Li-Demarteau          adapted from MARKJ    "
C-                                                                     "
C----------------------------------------------------------------------"
      COMMON /    / IB(2)
      DIMENSION B(2)    
      EQUIVALENCE (B(1),IB(1))
      DIMENSION TITLE(3)
      REAL*8 SUMYW,SUMYYW,SUMW,SUMWW
      CHARACTER*40 TITL
      LOGICAL HEXIST,LERR
      DATA IDPTR/0/,IDPMX/0/,IDPOOL/59000/,IDLIST/0/,ISLENG/8/,NBMAX/0/
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
C   OUTPUT ID,TITLE,NBIN,XMIN,XMAX,YLOW,YHIG;
C         (' ID,TITLE,NBIN,XMIN,XMAX,YLOW,YHIG',I9,3A4,I9,4F9.4);"
      IDPTR=IDPTR+1
      IF(IDPTR.GT.IDPMX) THEN     !DO WE NEED MORE ROOM TO STORE LIST OF  
        CALL HNEWID(IDPOOL)       !IDS OF HBAVER.  IF SO EXTEND HBOOK BANK
        IDPMX=IDPMX+300
        CALL HARRAY(IDPOOL,IDPMX+1,IADR)
        IF(IDLIST.NE.0) THEN
          CALL HLOCAT(IDLIST,LOC)
          DO 10 I=1,IDPMX
            IB(IADR+I)=IB(LOC+I)
   10     CONTINUE
          CALL HDELET(IDLIST)
        ENDIF
        IDLIST=IDPOOL
      ENDIF
C
      CALL HLOCAT(IDLIST,LOC)
      IB(LOC+IDPTR)=ID
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
C              BOOK STATISTICS BANK                                    "
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
      IF(NBIN.GT.500) THEN 
        PRINT 1001, NBIN,ID
        NBIN=500
      ENDIF 
C
      IF(NBIN.GT.NBMAX) NBMAX=NBIN
      CALL HNEWID(IDPOOL)                         !GET NEW ID FOR Y  HIST
      CALL HBOOK1(IDPOOL,TITLE,NBIN,XMIN,XMAX)    !BOOK Y  HIST          
      IDP1=IDPOOL
      CALL HNEWID(IDPOOL)                          !GET NEW ID OF # HIST 
      CALL HBOOK1(IDPOOL,'GARBAGE2$',NBIN,XMIN,XMAX) !BOOK # HIST         
      IDP2=IDPOOL
      CALL HNEWID(IDPOOL)                          !GET NEW ID OF # HIST 
      CALL HBOOK1(IDPOOL,'GARBAGE3$',NBIN,XMIN,XMAX) !BOOK # HIST         
      IDP3=IDPOOL
      CALL HNEWID(IDPOOL)                          !GET NEW ID OF # HIST 
      CALL HBOOK1(IDPOOL,'GARBAGE4$',NBIN,XMIN,XMAX) !BOOK Y**2 HIST      
      CALL HARRAY(ID,ISLENG,IADR)            !BOOK ARRAY TO HOLD IDS ETC.
      IB(IADR+1)=IDP1                        !STORE ID OF YW HIST   
      IB(IADR+2)=IDP2                        !STORE ID OF YYW HIST  
      IB(IADR+3)=IDP3                        !STORE ID OF W  HIST   
      IB(IADR+4)=IDPOOL                      !STORE ID OF WW HIST  
      B(IADR+5)=YLOW                         !STORE LOWER Y LIM    
      B(IADR+6)=YHIG                         !STORE UPPER Y LIM    
      IB(IADR+7)=NBIN                        !NUMBER OF BINS       
      RETURN                                 !FINISHED             
 1001 FORMAT(1X,' HBAVER IS LIMITED TO 500 BINS',I9,10X,'ID =',I6)
C
C
      ENTRY HBAVDP(IDD,TITLE,NBIN,XMIN,XMAX,YLOW,YHIG)
C
      CALL HNEWID(IDPOOL)                     !GET NEW ID FOR WT HIST
      IDWS=IDPOOL
      CALL HARRAY(IDWS,4*NBMAX+4,IADRWS)      !BOOK ARRAY TO USE AS WS 
      DO 20 I=1,IDPTR
        CALL HLOCAT(IDLIST,LOC)
        IDX=IB(LOC+I)
        CALL HLOCAT(IDX,IADR)
        ID1=IB(IADR+1)
        ID2=IB(IADR+2)
        ID3=IB(IADR+3)
        ID4=IB(IADR+4)
        NB=IB(IADR+7)
        OFFSET=(B(IADR+5)+B(IADR+6))/2.
        CALL HDELET(IDX)
        CALL HLOCAT(IDWS,IADRWS)
        CALL HUNPAK(ID1,B(IADRWS+1))
        CALL HUNPAK(ID2,B(IADRWS+NB+1))
        CALL HUNPAK(ID3,B(IADRWS+2*NB+1))
        CALL HUNPAK(ID4,B(IADRWS+3*NB+1))
        CALL HLOCAT(IDWS,IADRWS)
        LERR=.FALSE.
        DO 30 J=1,NB 
          SUMYW=B(IADRWS+J)
          SUMYYW=B(IADRWS+NB+J)
          SUMW=B(IADRWS+2*NB+J)
          SUMWW=B(IADRWS+3*NB+J)
          IF(SUMW.GT.0.D0) THEN
            IF(SUMW*SUMW.EQ.SUMWW) THEN 
              B(IADRWS+J)=ABS(SUMYW/SUMW)
              B(IADRWS+NB+J)=0.
              B(IADRWS+3*NB+J)=0.
            ELSE 
              VAR=SUMYYW/SUMW-(SUMYW/SUMW)**2
              IF(VAR.LT.0.) VAR=0.
              B(IADRWS+J)=SQRT(VAR*SUMWW/SUMW**2)
              B(IADRWS+NB+J)=SQRT(VAR)
              B(IADRWS+3*NB+J)=SQRT(VAR*SUMWW/SUMW**2/2.)
              LERR=.TRUE.
            ENDIF
            B(IADRWS+2*NB+J)=SUMYW/SUMW+OFFSET
          ELSE 
            B(IADRWS+J)=0.
            B(IADRWS+NB+J)=0.
            B(IADRWS+2*NB+J)=0.
            B(IADRWS+3*NB+J)=0.
          ENDIF
   30   CONTINUE 
C
        CALL HDELET(ID2)
        CALL HDELET(ID3)
        CALL HDELET(ID4)
        CALL HRESET(ID1)
        IF(LERR) CALL HBARX(ID1)
        CALL HLOCAT(IDWS,IADRWS)
        CALL HPAK(ID1,B(IADRWS+2*NB+1))
        IF(LERR) CALL HPAKE(ID1,B(IADRWS+1))
        CALL HCOPY (ID1,IDX)
        CALL HDELET(ID1)
        CALL HGIVE(IDX,DUM,NX,AL1,AL2)
        WRITE(TITL,1002) IDX 
        TITL=TITL(1:39)//'$'
        IF(LERR) THEN
          CALL HNEWID(IDPOOL)            !GET NEW ID FOR WT HIST
          CALL HBOOK1(IDPOOL,TITL,NB,AL1,AL2)
          CALL HLOCAT(IDWS,IADRWS)
          CALL HPAK(IDPOOL,B(IADRWS+NB+1))
          CALL HPAKE(IDPOOL,B(IADRWS+3*NB+1))
        ENDIF
   20 CONTINUE
C
      IF(IDLIST.NE.0) CALL HDELET(IDLIST)
      CALL HDELET(IDWS)
      IDPTR=0
      IDPMX=0 
      IDPOOL=59000 
      IDLIST=0
      RETURN
 1002 FORMAT(1X,'RESOLUTION ON HISTOGRAM',I6)
C
C
      ENTRY HBAVDPR(IDD,TITLE,NBIN,XMIN,XMAX,YLOW,YHIG)
C
      CALL HNEWID(IDPOOL)                     !GET NEW ID FOR WT HIST
      IDWS=IDPOOL
      CALL HARRAY(IDWS,4*NBMAX+4,IADRWS)      !BOOK ARRAY TO USE AS WS 
      DO 50 I=1,IDPTR
        CALL HLOCAT(IDLIST,LOC)
        IDX=IB(LOC+I)
        CALL HLOCAT(IDX,IADR)
        ID1=IB(IADR+1)
        ID2=IB(IADR+2)
        ID3=IB(IADR+3)
        ID4=IB(IADR+4)
        NB=IB(IADR+7)
        OFFSET=(B(IADR+5)+B(IADR+6))/2.
        CALL HDELET(IDX)
        CALL HLOCAT(IDWS,IADRWS)
        CALL HUNPAK(ID1,B(IADRWS+1))
        CALL HUNPAK(ID2,B(IADRWS+NB+1))
        CALL HUNPAK(ID3,B(IADRWS+2*NB+1))
        CALL HUNPAK(ID4,B(IADRWS+3*NB+1))
        CALL HLOCAT(IDWS,IADRWS)
        LERR=.FALSE.
        DO 60 J=1,NB 
          SUMYW=B(IADRWS+J)
          SUMYYW=B(IADRWS+NB+J)
          SUMW=B(IADRWS+2*NB+J)
          SUMWW=B(IADRWS+3*NB+J)
          IF(SUMW.GT.0.D0) THEN
            IF(SUMW*SUMW.EQ.SUMWW) THEN 
              B(IADRWS+J)=ABS(SUMYW/SUMW)
              B(IADRWS+NB+J)=0.
              B(IADRWS+3*NB+J)=0.
            ELSE 
              VAR=SUMYYW/SUMW-(SUMYW/SUMW)**2
              IF(VAR.LT.0.) VAR=0.
              B(IADRWS+J)=SQRT(VAR*SUMWW/SUMW**2)
              B(IADRWS+NB+J)=SQRT(VAR)
              B(IADRWS+3*NB+J)=SQRT(VAR*SUMWW/SUMW**2/2.)
              LERR=.TRUE.
            ENDIF
            B(IADRWS+2*NB+J)=SUMYW/SUMW+OFFSET
          ELSE 
            B(IADRWS+J)=0.
            B(IADRWS+NB+J)=0.
            B(IADRWS+2*NB+J)=0.
            B(IADRWS+3*NB+J)=0.
          ENDIF
   60   CONTINUE 
C
        CALL HDELET(ID2)
        CALL HDELET(ID3)
        CALL HDELET(ID4)
        CALL HRESET(ID1)
        IF(LERR) CALL HBARX(ID1)
        CALL HLOCAT(IDWS,IADRWS)
        CALL HPAK(ID1,B(IADRWS+2*NB+1))
        IF(LERR) CALL HPAKE(ID1,B(IADRWS+1))
        CALL HCOPY (ID1,IDX)
        CALL HDELET(ID1)
        CALL HGIVE(IDX,DUM,NX,AL1,AL2)
        WRITE(TITL,1003) IDX 
        TITL=TITL(1:39)//'$'
        IF(LERR) THEN
          CALL HNEWID(IDPOOL)            !GET NEW ID FOR WT HIST
          CALL HBOOK1(IDPOOL,TITL,NB,AL1,AL2)
          CALL HLOCAT(IDWS,IADRWS)
          CALL HPAK(IDPOOL,B(IADRWS+NB+1))
          CALL HPAKE(IDPOOL,B(IADRWS+3*NB+1))
        ENDIF
        BSIZE=(AL2-AL1)/NB
        XBIN=AL1+BSIZE/2.
        CALL HLOCAT(IDWS,IADRWS)
        WRITE(88,1004) IDX
        DO 70 K=1,NB
          WRITE(88,1005) XBIN,B(IADRWS+2*NB+K),B(IADRWS+K),
     &                       B(IADRWS+NB+K),B(IADRWS+3*NB+K)
          XBIN=XBIN+BSIZE
   70   CONTINUE
   50 CONTINUE
C
      IF(IDLIST.NE.0) CALL HDELET(IDLIST)
      CALL HDELET(IDWS)
      IDPTR=0
      IDPMX=0 
      IDPOOL=59000 
      IDLIST=0
      RETURN
 1003 FORMAT(1X,'RESOLUTION ON HISTOGRAM',I6)
 1004 FORMAT(1X,'PRINT OF HISTOGRAM',I6,/,
     &       1X,'    XBIN   ','    MEAN   ',' ERROR MEAN','    SIGMA  ',
     &       1X,'ERROR SIGMA') 
 1005 FORMAT(F10.2,3(1X,F10.3),1X,F10.4)
      END
C
C
      SUBROUTINE HBAVFL(ID,X,Y)
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
C       ROUTINE TO FILL HISTOGRAMS BOOKED IN HBAVER.  TWO              "
C       HISTOGRAMS ARE FILLED, ONE WITH THE WEIGHT GIVEN AND THE       "
C       OTHER WITH 1.                                                  "
C       PARAMETERS:                                                    "
C           ID = HISTOGRAM ID AS IN HFILL                              "
C           X  = POSITION ALONG X AXIS (DETERMINES THE BIN)            "
C           Y  = POSITION ALONG Y AXIS (VALUE TO BE AVERAGED)          "
C       AUTHOR JGB                                                     "
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
      COMMON /    / IB(2)
      DIMENSION B(2)
      EQUIVALENCE (B(1),IB(1))
      DATA IMESG/0/
      LOGICAL HEXIST
      CALL HLOCAT(ID,LOC)
      IF(LOC.GT.0.AND.HEXIST(IB(LOC+1))) GOTO 100 
      IMESG=IMESG+1
      IF(IMESG.LT.20) THEN 
        PRINT 1002, ID
      ENDIF
      RETURN
  100 CONTINUE 
C
      IF(Y.LT.B(LOC+5).OR.Y.GT.B(LOC+6)) RETURN
      YP=Y-(B(LOC+5)+B(LOC+6))/2.
      CALL HFILL(IB(LOC+1),X,YP)
      CALL HFILL(IB(LOC+2),X,YP*YP)
      CALL HFILL(IB(LOC+3),X)
      CALL HFILL(IB(LOC+4),X)
      RETURN
 1002 FORMAT(1X,' ID',I6,' NOT FOUND')
      END
C
C
      SUBROUTINE HBAVFW(ID,X,Y,W)
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
C       ROUTINE TO FILL HISTOGRAMS BOOKED IN HBAVER.  TWO              "
C       HISTOGRAMS ARE FILLED, ONE WITH THE WEIGHT GIVEN AND THE       "
C       OTHER WITH 1.                                                  "
C       PARAMETERS:                                                    "
C           ID = HISTOGRAM ID AS IN HFILL                              "
C           X  = POSITION ALONG X AXIS (DETERMINES THE BIN)            "
C           Y  = POSITION ALONG Y AXIS (VALUE TO BE AVERAGED)          "
C           W  = WEIGHT USED TO AVERAGE Y AXIS VALUE                   "
C       AUTHOR JGB                                                     "
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
      COMMON /    / IB(2)
      DIMENSION B(2)
      EQUIVALENCE (B(1),IB(1))
      DATA IMESG/0/
      LOGICAL HEXIST
      CALL HLOCAT(ID,LOC)
      IF(LOC.GT.0.AND.HEXIST(IB(LOC+1))) GOTO 100 
      IMESG=IMESG+1 
      IF(IMESG.LT.20) THEN 
        PRINT 1003, ID
      ENDIF
      RETURN
  100 CONTINUE 
C
      IF(Y.LT.B(LOC+5).OR.Y.GT.B(LOC+6)) RETURN
      YP=Y-(B(LOC+5)+B(LOC+6))/2.
      CALL HFILL(IB(LOC+1),X,YP*W)
      CALL HFILL(IB(LOC+2),X,YP*YP*W)
      CALL HFILL(IB(LOC+3),X,W)
      CALL HFILL(IB(LOC+4),X,W*W)
      RETURN
 1003 FORMAT(' ID',I6,' NOT FOUND')
      END
C
C
      SUBROUTINE HNEWID(IDPOOL)
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
C       ROUTINE TO FIND NEXT FREE ID FOR USE OF HBAVER                 "
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
      LOGICAL HEXIST
   10 IDPOOL=IDPOOL+1
      IF(HEXIST(IDPOOL)) GOTO 10 
      RETURN
      END
