      LOGICAL FUNCTION VERTEX_VTX(METHOD) 
C------------------------------------------------------------------
C 
C  Main routine for finding primary vertex using VTX hits. 
C 
C  Daria Zieminska Nov. 1988
C  Modified Dec. 1989: allow for multiple primary vertices 
C  (Needs testing with multiple events).
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR,
C-                                                also added SAVE statement
C-   Updated   5-APR-1994   Liang-ping Chen add CALL VTX_DYNADJ before VTHITS  
C-                                          match CALL EZPICK and CALL EZRSET 
C------------------------------------------------------------------
      IMPLICIT NONE  
      INTEGER NLAYER,LAYER,SECTOR,NHIT,NENTRY,N1,ENOUGH,NV,NVMAX 
      PARAMETER (NLAYER=2)
      PARAMETER (NVMAX=5)
      INTEGER NSEC(0:NLAYER),NTOT,NEL,NWORDS,MAXPRIM,METHOD 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
      INTEGER NGOOD,USUNIT,PRUNIT,LVERH,GZVERH,LVERT,NR,GZHSTR,ISETVN 
      REAL CONT(18),VZ(NVMAX),SIGVZ(NVMAX),HSTATI,Z0AVR
      REAL ZVER(NVMAX),DZVER(NVMAX),SIGMA,SIGMAZ,SINGLEV,ZISAJT,ZDIF
      LOGICAL HSTFLG
      CHARACTER*4 PATH,VPATH
      INTEGER IER,ICALL,IPATH,LISV1,GZISV1
      EQUIVALENCE (IPATH,VPATH)
      LOGICAL EZERROR
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','VERTEX_VTX',
     &       'Unable to find bank VTRAKS_RCP','W')
          GOTO 1000
        ENDIF
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZGET('NSEC',NSEC,IER)
        CALL EZRSET
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('NR',NR,IER)
        CALL EZGET('ENOUGH',ENOUGH,IER)
        CALL EZGET('MAXPRIM',MAXPRIM,IER)
        CALL EZGET('SINGLEV',SINGLEV,IER)
        CALL EZGET('SIGMAZ',SIGMAZ,IER)
        CALL EZGET('HSTFLG',HSTFLG,IER)
        CALL EZRSET
C
C         Create/Set HBOOK directory for VERTEX
C
        CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
        IF (IER.NE.0) THEN
          CALL ERRMSG('VERTEX','VERTEX_VTX',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        IF (HSTFLG) 
     &    CALL HBOOK1(1094,' Z(ISAJET) - Z(VTX) $',100,-15.,15.,0.)
        CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
        ICALL=1
      END IF
      VERTEX_VTX=.FALSE.
      NV=0
      NENTRY=0
      IF (METHOD.EQ.2) THEN
        CALL VERTE2(Z0AVR,NENTRY) 
        GO TO 800
      END IF
      PATH=VPATH 
      CALL PATHST(PATH)
      CALL VTX_DYNADJ
      CALL VTHITS 
      CALL GTVTXH(NTOT)
      IF (NTOT.LT.8) GO TO 900
C
C  Loop over layers and sectors and fill a histogram of z(r=0) 
C  Interrupt if sufficient statistics have been collected.
C                            
      Z0AVR=-1000.
      DO 100 LAYER=0,NLAYER 
        CALL GTVLAY(LAYER,NHIT)
        IF (NHIT.LT.8) GO TO 99
        DO 200 SECTOR=0,NSEC(LAYER)
          CALL GTVSEC(LAYER,SECTOR,'SEC',0,NEL,NWORDS,CONT)
          CALL UCOPY(CONT,NHIT,1)
          IF (NHIT.LT.8) GO TO 200
          CALL VCHEKH(2,LAYER,SECTOR,NHIT,NGOOD)  
          IF (NGOOD.LT.8) GO TO 200
          CALL VERTE1(LAYER,SECTOR,Z0AVR,NENTRY) 
          IF (NENTRY.GT.ENOUGH) GO TO 800
  200   CONTINUE
   99   CONTINUE
  100 CONTINUE                                                       
  800 CONTINUE
      IF (NENTRY.GE.1) THEN
        SIGMA=HSTATI(1,2,' ',0)     
        IF (SIGMA.LT.SINGLEV) THEN  
C
C  Single event; use HSTATI to get z,dz.
C
          NV=1
          VZ(1)=Z0AVR  ! mean value of z(r=0)
          IF (SIGMA.LE.0.) SIGMA=SIGMAZ
          SIGVZ(1)=SIGMA/SQRT(FLOAT(NENTRY))
        ELSE
          IF (MAXPRIM.GT.1.AND.NENTRY.GT.10)   THEN  
C
C  z distribution too wide for a single event, look for more than one vertex
C
            CALL VERMULTI(METHOD,NV,VZ,SIGVZ)
          END IF
        END IF
        CALL HDELET(1)
      END IF
  900 CONTINUE
      CALL VERFIL(NV,VZ,SIGVZ)
      IF (NV.GT.0) VERTEX_VTX=.TRUE.
      CALL PATHRS
C
C  debug 
C
      CALL ZVERTE(NV,ZVER,DZVER)
C
C   get isajet track information for debug purpose, 
C   it should be removed later
C
      IF (HSTFLG) THEN
        LISV1 = GZISV1()
        IF (LISV1 .GT. 0) THEN
          ZISAJT = Q(LISV1+9)
          ZDIF = ZISAJT - ZVER(1)
          CALL HF1(1094,ZDIF,1.)
        ENDIF
      ENDIF
C
 1000 RETURN 
      END       
