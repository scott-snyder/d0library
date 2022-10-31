      SUBROUTINE VTROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NV,IDV) 
C------------------------------------------------------------------
C 
C  Find VTX tracks in a road 
C 
C-   Inputs  : 
C-        ZVTX : vertex position in Z
C-        PHIMIN: minimum phi of the road
C-        PHIMAX: maximum phi of the road
C-        THEMIN: minimum theta of the road
C-        THEMAX: maximum theta of the road
C-   Outputs : NV:     number of VTX tracks in the road
C-             IDV(I): VTX track ID
C
C  Daria Zieminska JAN.,1989
C-   Updated  19-NOV-1989   Qizhong Li-Demarteau  corrected wrong arguments
C-                                                in the call to VSEGME
C-   Updated  03-JAN-1990   Qizhong Li-Demarteau  removed unused argument
C-   Updated   3-DEC-1990   Qizhong Li-Demarteau  fixed the call to MZBOOK
C-   Updated   6-FEB-1991   Daria Zieminska check to see if full-tracking 
C-                          already done
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added SAVE statement and 
C-                                                call EZRSET and EZERROR
C-   Updated  12-NOV-1991   Peter M. Grudberg  Fix PATH 
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  removed machine_block 
C-   Updated  14-JUL-1992   Qizhong Li-Demarteau  removed 24 non-used variables 
C-   Updated  20-JUL-1992   Qizhong Li-Demarteau  added an input argument ZVTX 
C-   Updated  25-OCT-1992   Peter M. Grudberg  Remove calls to zstrip routines 
C-   Updated  25-DEC-1992   Liang-ping Chen       Add CALL VTX_DYNADJ 
C_                                           and flag DYNSTAT in VTRH
C-   Updated  11-FEB-1993   Liang-ping Chen  add number of hits to VTRH
C-   Updated  11-FEB-1993   Ed Oltman   Save road parameters for VPOINT,FTVTXT
C-   Updated  14-FEB-1993   Liang-ping Chen  add CDD1 bank length to VTRH 
C-   Updated  13-APR-1993   Qizhong Li-Demarteau  fixed overwritting problem
C-   Updated  21-MAY-1993   Liang-ping Chen Indicate full tracking via VRDSAVE
C-   Updated  22-MAY-1993   Ed Oltman   CALL VCLEAN and associated stuff
C-   Updated  27-OCT-1993   Ed Oltman   Replace call to FTVTXT with VTRKFT and
C-                                      add call to VCLNTRK
C-   Updated  20-FEB-1994   Liang-Ping Chen eliminate THEMIN, THEMAX to VRHITS  
C_
C------------------------------------------------------------------
      IMPLICIT NONE  
      INTEGER NLAYER,NV,IDV(*) 
      PARAMETER (NLAYER=2)
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX, ZVTX
      INTEGER NTOT
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER INEFF, HITSEG
      INTEGER LUSER,NTRSG,GZVTRH
      INTEGER LVTRH,IER,IPATH,LZTRH,GZZTRH,FULL
      INTEGER NLADD,MXTRAK,NSEGML(0:NLAYER)
      PARAMETER (MXTRAK = 1000 )
      INTEGER LADDRS(0:NLAYER,MXTRAK),ICALL
      INTEGER RUN, RUNSAV, ID, IDSAV
      LOGICAL EZERROR
      CHARACTER*4 PATH,VPATH
      EQUIVALENCE (IPATH,VPATH)
      INTEGER ENV_STATUS, LUM_STATUS
      INTEGER ENV_COR_BIT, LUM_COR_BIT
      PARAMETER(ENV_COR_BIT=1, LUM_COR_BIT=2)
      LOGICAL NEW_EVENT, FULL_TRACKING
      INTEGER LAYER,LVSGL,LSTTRK,START,LVTXT,GZVTXT
      SAVE ICALL
      SAVE RUNSAV, IDSAV 
      DATA ICALL/0/
      DATA RUNSAV, IDSAV /-1,-1/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VTROAD',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('VPATH',IPATH,IER)
        CALL EZGET_i('INEFF',INEFF,IER)
        CALL EZRSET
C
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VTROAD',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('FULL_TRACKING',FULL_TRACKING,IER)
        CALL EZRSET

C ****  Let VPOINT and FTVTXT know that full tracking is
C ****  being done -- ZVTX = 9999. means don't use road information

        IF (FULL_TRACKING) THEN
          CALL VRDSAVE(9999.,0.,0.,1.5,1.5)
        ENDIF
        HITSEG=8-INEFF          ! minimum # hits in segment
        ICALL=1
      END IF
C  Check if full tracking results already exist
C
      LZTRH=GZZTRH()
      IF(LZTRH.GT.5) THEN
        FULL=IBITS(IQ(LZTRH),12,1)
        IF(FULL.EQ.1) GOTO 800
      ENDIF
C
C  Find hits in sectors containing the road
C
      PATH=VPATH 
      CALL PATHST(PATH)
      NEW_EVENT=.FALSE.
      CALL EVNTID(RUN,ID)
      IF (RUN.NE.RUNSAV .OR. ID.NE.IDSAV) THEN
        NEW_EVENT=.TRUE.
        CALL VTX_DYNADJ
      ENDIF  
      CALL VRHITS(PHIMIN,PHIMAX)
      CALL GTVTXH(NTOT)
      IF (NTOT.LT.HITSEG) GO TO 900
      CALL MZBOOK(IXMAIN,LUSER,LHEAD,-IZUSER,'USER',6,6,10,2,0)
      IF (.NOT. FULL_TRACKING) THEN
        CALL VRDSAVE(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
      ENDIF
      LVTRH=GZVTRH()
      IF (LVTRH.EQ.0) THEN
        CALL BKVTRH(LVTRH)
        LSTTRK = 0
      ELSE
        LSTTRK = IQ(LVTRH+2)
        DO LAYER = 0,2
          LVTRH = GZVTRH()
          LVSGL = LQ(LVTRH-3-LAYER)
          IF (LVSGL .GT. 0) CALL MZDROP(IXCOM,LVSGL,'L')
        ENDDO
      ENDIF
      CALL VSEGME(0,NSEGML) 
C
C Make VTX tracks by matching segments
C
      NTRSG=NSEGML(0)+NSEGML(1)+NSEGML(2)
      IF (NTRSG.EQ.0) GO TO 900
      CALL VZERO(LADDRS,3*MXTRAK)
      CALL LINSEG(NLADD,LADDRS)         ! Link track segments
C
C  Fit linked segments (ladders) 
C
      IF (NLADD.EQ.0) GO TO 800
      LVTXT = GZVTXT(0)
      IF (LVTXT .EQ. 0) THEN
        START = 1
      ELSE
        START = IQ(LVTXT-5) + 1
      ENDIF
      CALL VTRKFT(NLADD,LADDRS)
      CALL VCLNTRK(START)
C
C ****  Now, eliminate duplicate tracks
C
      CALL VCLEAN(LSTTRK)
C
C  Find number of VTX tracks in the road and their ID's
C      
  800 CONTINUE
      CALL NVROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NV,IDV) 
      LUSER=LQ(LHEAD-IZUSER) 
      CALL MZDROP(0,LUSER,'L')  
  900 CONTINUE
C
C   fetch the pointer LVTRH again to avoid overwritting memory
C
      LVTRH = GZVTRH()
C
C  Flag DYNSTAT in VTRH 
C
      IF (NEW_EVENT) THEN 
        CALL VTX_DYNSTAT(ENV_STATUS, LUM_STATUS)
        IQ(LVTRH+3)=0
        IF (ENV_STATUS.EQ.0) IQ(LVTRH+3)=IBSET(IQ(LVTRH+3),
     &    ENV_COR_BIT) 
        IF (LUM_STATUS.EQ.0) IQ(LVTRH+3)=IBSET(IQ(LVTRH+3),
     &    LUM_COR_BIT)          
        RUNSAV=RUN
        IDSAV=ID
      ENDIF

C
C *** store number of VTX hits and CDD1 length in VTRH
C     for TRD DST use,
C
      IQ(LVTRH+4)=IQ(LVTXH+1)
      IQ(LVTRH+6)=IQ(LQ(LHEAD-3)-1)
C
      CALL PATHRS
  999 RETURN 
      END       
