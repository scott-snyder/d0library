      FUNCTION VTRAKS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do VTX trackfinding.  First find hits for all
C-                         sectors, next search for segments within sectors,
C-                         and finally link segments to form tracks.
C-
C-   Returned value  : .TRUE. if processing is successful
C-
C-   Created  JAN-1989   Daria Zieminska
C-   Updated  25-OCT-1991   Peter M. Grudberg  Various updates, cleanup 
C-   Updated  10-DEC-1991   Peter M. Grudberg  Redo logic, add  summary info
C-   Updated   5-OCT-1992   Peter M. Grudberg  remove strips 
C-   Updated   6-NOV-1992   Peter M. Grudberg  Add call to VTXHST 
C-   Updated  20-DEC-1992   Liang-ping Chen    Add switch VTXRECO
C-   Updated  21-DEC-1992   Ed Oltman          Add call VTX_DYNADJ
C-   Updated  25-DEC-1992   Liang-ping Chen    Flag DYNADJ status in VTRH
C-   Updated  11-FEB-1993   Liang-ping Chen    add number of hits to VTRH  
C-   Updated  14-FEB-1993   Liang-ping Chen    add CDD1 bank length to VTRH  
C-   Updated  23-MAR-1993   Ed Oltman   Indicate full tracking via VRDSAVE
C-   Updated   8-NOV-1993   Liang-ping  Chen   Replace call to FTVTXT with
C-                                      VTRKFT and add call to VCLNTRK,
C-                                      similar to Oltman's change in VTROAD  
C-   Updated   8-FEB-1994   Liang-Ping Chen if REDOVTX=.TRUE. and no CDD1,
C-                                      drop all banks, except VCHT,
C-                                      which hang from VTXH.
C-                                      check CDH1 
C-   Updated  11-JUL-1994   liang-ping chen add calls to VTX_HITS, so VCHT 
C-                                     related codes are under test.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL VTRAKS
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      INTEGER NLAYER 
      PARAMETER (NLAYER=2)
      INTEGER NTOT
      INTEGER HITSEG,INEFF
      INTEGER ILAY,LSEC 
      INTEGER LUSER,NTRSG,GZVTRH,LVTXH,GZVTXH
      INTEGER LVTRH,STAT,IDONE 
      INTEGER NLADD,MXTRAK,NSEGML(0:NLAYER)
      PARAMETER ( MXTRAK = 1000 )
      INTEGER LADDRS(0:NLAYER,MXTRAK),USUNIT,PRUNIT,NEV
      REAL TIME,THIT,TTRACK
      LOGICAL PRODUC,PROD,DROP_SEGM,REDOVTX,CALLHST
      INTEGER VTXRECO
      CHARACTER*4 PATH,VPATH
      DATA NEV/0/
      DATA IDONE/2/
      INTEGER IER,IPATH 
      EQUIVALENCE (IPATH,VPATH)
      INTEGER ENV_STATUS, LUM_STATUS
      INTEGER ENV_COR_BIT, LUM_COR_BIT
      PARAMETER(ENV_COR_BIT=1, LUM_COR_BIT=2)
      INTEGER START,LVTXT,GZVTXT 
      INTEGER LCDD1, LVCHT, GZVCHT, LVLAY, GZVLAY
      INTEGER LCDH1, GZCDH1
      INCLUDE 'D0$INC:ZEBSTP.INC'
C---------------------------------------------------------------------
      IF ( NEV .EQ. 0 ) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZGET('DROP_SEGM',DROP_SEGM,IER)
        CALL EZGET('REDOVTX',REDOVTX,IER)
        CALL EZGET('VTXRECO',VTXRECO,IER)
        CALL EZGET('CALLHST',CALLHST,IER)
        CALL EZRSET
        HITSEG = 8 - INEFF          ! minimum # hits in segment
        PROD = PRODUC()
        IF (.NOT.PROD) PRUNIT = USUNIT()
C
C ****  Let VPOINT and FTVTXT know that full tracking is
C ****  being done -- ZVTX = 9999. means don't use road information
C
        CALL VRDSAVE(9999.,0.,0.,1.5,1.5)  
      END IF
C
      VTRAKS = .TRUE.
C
      PATH = VPATH 
      CALL PATHST(PATH)
C
      IF (VTXRECO.LE.0) GOTO 999
C
C
      LCDD1 = LQ(LHEAD - IZCDD1)
      LCDH1 = GZCDH1()
      LVCHT = GZVCHT()
C
      LVTXH = GZVTXH()
      LVTRH = GZVTRH()
C
      IF (LVTXH.LE.0.AND.LVTRH.LE.0) THEN ! It is raw data. LVTRH.LE.0 is added
                                          ! to block the flow for "VTX WZ" files 
        REDOVTX=.FALSE.             ! To prevent a wrong setting for RAW data

        CALL VTX_HITS               ! To build VCHT then drop VLAY etc.
        LCDD1 = LQ(LHEAD - IZCDD1)  ! Refresh all pointers defined above
        LCDH1 = GZCDH1()
        LVCHT = GZVCHT()
        LVTXH = GZVTXH()
        LVTRH = GZVTRH()
        IF (LCDD1.GT.0) CALL MZDROP(IXCOM,LCDD1,' ') 
        IF (LCDH1.GT.0) CALL MZDROP(IXCOM,LCDH1,' ') 
      ENDIF
C
      IF ( REDOVTX ) THEN
C
C ****  Drop old hit, track banks (if they exist) and book new VTRH
C

        IF ( LVTXH .GT. 0 ) THEN 
          IF (LCDD1.GT.0 .OR. LCDH1.GT.0 )  THEN 
            CALL MZDROP(IXCOM,LVTXH,' ')
            CALL VTX_HITS        
            LCDD1 = LQ(LHEAD - IZCDD1)
            LCDH1 = GZCDH1()
            LVCHT = GZVCHT()
            LVTXH = GZVTXH()
            LVTRH = GZVTRH()
            IF (LCDD1.GT.0) CALL MZDROP(IXCOM,LCDD1,' ') 
            IF (LCDH1.GT.0) CALL MZDROP(IXCOM,LCDH1,' ') 
          ELSEIF (LVCHT.GT.0) THEN 
            DO ILAY = 0, 2
              LVLAY = GZVLAY(ILAY)
              IF ( LVLAY .GT. 0 ) THEN
                CALL MZDROP(IXCOM,LVLAY,' ')
              ENDIF
            ENDDO
            CALL VHIT_DROP
            IQ(LVTXH+1)  = 0
            IQ(LVTXH+2)  = 0
            IQ(LVTXH+10) = 0
            IQ(LVTXH+11) = 0
            IQ(LVTXH+12) = 0
          ELSE    
            CALL ERRMSG('VTX-NO-DATA','VTRAKS',
     &      'Neither CDD1 nor VCHT exists','F')
          ENDIF
        ENDIF
C
        IF ( LVTRH .GT. 0 ) THEN 
          CALL MZDROP(IXCOM,LVTRH,' ')
          IF (LVTXH.LE.0) THEN    ! for the special case of "VTX WZ" files
            CALL VTX_HITS  
            LCDD1 = LQ(LHEAD - IZCDD1) 
            LCDH1 = GZCDH1()
            LVCHT = GZVCHT()
            LVTXH = GZVTXH()
            LVTRH = GZVTRH()
            IF (LCDD1.GT.0) CALL MZDROP(IXCOM,LCDD1,' ') 
            IF (LCDH1.GT.0) CALL MZDROP(IXCOM,LCDH1,' ') 
          ENDIF
        ENDIF
C
        CALL BKVTRH(LVTRH)
        STAT = IQ(LVTRH)
        IQ(LVTRH) = IBSET(STAT,IDONE)
      ELSE
C
C ****  If VTRH doesn't exist book it.  If it does exist, then check if
C ****  tracking has already been done.
C
        IF ( LVTRH .EQ. 0 ) THEN
          CALL BKVTRH(LVTRH)
          STAT = IQ(LVTRH)
          IQ(LVTRH) = IBSET(STAT,IDONE)
        ELSE
          STAT = IQ(LVTRH)
          IF ( BTEST(STAT,IDONE) ) THEN
            GO TO 999                     ! Tracking already done
          ELSE
            IQ(LVTRH)=IBSET(STAT,IDONE)   ! Mark done bit and do tracking
          ENDIF
        ENDIF
      ENDIF
C
      NEV = NEV + 1
      CALL VTX_DYNADJ
C
C
C ****  Do hitfinding for all sectors
C
      IF (.NOT.PROD) CALL TIMED(TIME)
      CALL VTHITS                 ! unpack VCHT into VLAY if CDD1 does not exist
      IF (.NOT.PROD) CALL TIMED(THIT)
      CALL GTVTXH(NTOT)
      IF ( NTOT .LT. HITSEG ) THEN
        CALL ERRMSG('VTX-insuf-hits-found','VTRAKS',
     &    'Not enough VTX hits to do tracking in this event','I')
        GO TO 999
      ENDIF
C
      IF (VTXRECO.LE.1) GOTO 999
C
C ****  Book USER bank for use by segment finding routines
C
      CALL MZBOOK(0,LUSER,LHEAD,-IZUSER,'USER',6,6,10,2,0)
C
C ****  Find all segments; 
C
      CALL VSEGME(1,NSEGML) 
      NTRSG=NSEGML(0)+NSEGML(1)+NSEGML(2)
      IF (NTRSG.EQ.0) THEN
        CALL ERRMSG('VTX-no-segments-found','VTRAKS',
     &    'No VTX track segments found in this event','I')
        GO TO 666       ! Clean up before exit
      ENDIF
C
      IF (VTXRECO.LE.2) GOTO 999
C
C **** Make VTX tracks by matching segments
C
      CALL VZERO(LADDRS,3*MXTRAK)
      CALL LINSEG(NLADD,LADDRS)    ! Link track segments
C
C **** Fit linked segments (ladders) 
C
      IF (NLADD.EQ.0) THEN
        CALL ERRMSG('VTX-no-tracks-found','VTRAKS',
     &    'No VTX tracks found in this event','I')
        GO TO 666
      ENDIF
      NLADD=MIN(NLADD,MXTRAK)
      LVTXT = GZVTXT(0)                                                 
      IF (LVTXT .EQ. 0) THEN                                            
        START = 1                                                       
      ELSE                                                              
        START = IQ(LVTXT-5) + 1                                         
      ENDIF                                                             
      CALL VTRKFT(NLADD,LADDRS)                                         
      CALL VCLNTRK(START)                                               
C
C ****  Signal if track found (for summary)
C
      LVTRH = GZVTRH()
      IF ( IQ(LVTRH+2) .GT. 0 ) THEN
        CALL ERRMSG('VTX-track-found','VTRAKS',
     &    'At least one VTX track found in this event','I')
      ENDIF
C
C ****  Flag VTX_DYNADJ status in VTRH bank
C
      CALL VTX_DYNSTAT(ENV_STATUS, LUM_STATUS)
      IQ(LVTRH+3)=0
      IF (ENV_STATUS.EQ.0) THEN
        IQ(LVTRH+3)=IBSET(IQ(LVTRH+3),ENV_COR_BIT)          
      ENDIF
      IF (LUM_STATUS.EQ.0) THEN
        IQ(LVTRH+3)=IBSET(IQ(LVTRH+3),LUM_COR_BIT)          
      ENDIF 
C 
C *** store number of VTX hits and CDD1 length in VTRH
C     for TRD DST use,  
C   
      IQ(LVTRH+4)=IQ(LVTXH+1)      
      IQ(LVTRH+6)=IQ(LQ(LHEAD-3)-1)
C
C ****  Call histogramming routine, if requested
C
      IF ( CALLHST ) CALL VTXHST
C
C ****  Drop USER bank (whole linear chain)  If requested, also drop segment
C ****  banks.
C
  666 CONTINUE
      LUSER=LQ(LHEAD-IZUSER) 
      CALL MZDROP(IXCOM,LUSER,'L')  
      IF (.NOT.PROD) CALL TIMED(TTRACK)
      IF ( DROP_SEGM ) THEN
        DO ILAY = 0, NLAYER
          LVTRH = GZVTRH()
          LSEC = LQ(LVTRH-3-ILAY)
          CALL MZDROP(IXCOM,LSEC,'L')
        ENDDO
      ENDIF
      CALL PATHRS
      IF (PROD) GO TO 999
      IF (NEV.GT.10) GO TO 999
      WRITE (PRUNIT,101) THIT,TTRACK
  101 FORMAT(' CPU time for VTX hits+tracking', 2F7.2)
  999 RETURN
      END       
