C VAX/DEC CMS REPLACEMENT HISTORY, Element ZCOSMC.FOR
C *1     4-NOV-1993 11:02:52 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element ZCOSMC.FOR
      SUBROUTINE ZCOSMC(PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKR)
C----------------------------------------------------------------------
C
C  Find central tracks in a road 
C
C  Inputs  : 
C     PHIMIN   = minimum phi
C     PHIMAX   = maximum phi
C     THEMIN   = minimum theta
C     THEMAX   = maximum theta
C     PT       = transverse momentum of muon or electron candidate
C
C  Outputs:
C     NZTR:    number of Central Detector tracks in the road 
C     ZLINKR(I): link of Central Detector track in the road
C
C-   Created  28-MAR-1991   Qizhong Li-Demarteau   
C-   Updated  29-AUG-1991   Qizhong Li-Demarteau  update compressed hits banks 
C-   Updated   1-DEC-1991   Qizhong Li-Demarteau  the call to ZTRHIS is moved
C-                                           to the end of event process hook 
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  removed Machine_block 
C-   Updated  24-MAR-1993   Qizhong Li-Demarteau  move MAXTRK to params file
C-   Updated   8-OCT-1993   Robert E. Avery   Allow use of FDC free t0 
C-                                              track finding and fitting
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INTEGER NC,NF,NF1,NV,ERR, IZ
      INTEGER NZCV, NZFV, NZFC, NZF, NZC, NZV, NZ, NZTR, ZLINKR(ZMAX)
      INTEGER LZTRH, GZZTRH
      INTEGER IDC(MAXTRK),IDF(MAXTRK),IDV(MAXTRK)
      LOGICAL VTDONE, FDDONE, VTONLY, CDONLY, FDONLY, FIRST, MKZFIT
      LOGICAL VTXON, CDCON, FDCON, OK, VTRAKS, DTREVT, FTRAKS
      LOGICAL FREE_T0
      LOGICAL EZERROR
      REAL PHIMIN, PHIMAX, THEMIN, THEMAX, PT
      REAL P1, P2, T1, T2, ZVTX
      REAL    ERRVTX(3), FITVTX(3)
      REAL    T0, T0_ERR, PHI, THETA
      INTEGER IBSET
      INTEGER RUN, ID, RUNSAV, IDSAV, IER
      SAVE RUNSAV, IDSAV, FIRST
      DATA RUNSAV,IDSAV/-1,-1/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZCOSMC',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VTONLY',VTONLY,ERR)
        CALL EZGET('CDONLY',CDONLY,ERR)
        CALL EZGET('FDONLY',FDONLY,ERR)
        CALL EZGET('MKZFIT',MKZFIT,ERR)
        CALL EZGET('VTXON',VTXON,ERR)
        CALL EZGET('CDCON',CDCON,ERR)
        CALL EZGET('FDCON',FDCON,ERR)
        CALL EZGET('FREE_T0',FREE_T0,ERR)
        CALL EZRSET
        ZVTX = 0.0
      ENDIF
      CALL ZFPARH(PHIMIN,PHIMAX,THEMIN,THEMAX,PT)
C
C ****  Initialize a temporary LINK area
C
      IF (ZTRLNK(1) .EQ. 0) 
     &  CALL MZLINT(IXCOM,'/ZTRLNK/',ZTRLNK,ZLINKS(ZMAX),ZTRLNK)
C
      NV = 0
      NC = 0
      NF = 0
      NZ = 0
      NZCV = 0
      NZFV = 0
      NZC  = 0
      NZV  = 0
      NZF  = 0
      P1=PHIMIN
      P2=PHIMAX
C
C  to make sure that T1 is always smaller than T2
C
      IF (THEMIN .LE. THEMAX) THEN
        T1 = THEMIN
        T2 = THEMAX
      ELSE
        T2 = THEMIN
        T1 = THEMAX
      ENDIF
C
C     check all ZTRKs, which built previoursly, to see if any ZTRK belong
C     to this road. 
C
      CALL ZTCHCK(P1,P2,T1,T2,NZ)
C
C  do full tracking on the subdetectors, if it is switched on
C  then pick up the ones belong to the road and try to match them
C
        IF (VTXON) THEN
          OK = VTRAKS() 
          CALL NVROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NV,IDV) 
        ENDIF
        IF (CDCON) THEN
          OK = DTREVT() 
          CALL NCROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NC,IDC) 
        ENDIF
        IF (FDCON) THEN
          IF ( FREE_T0 ) THEN
            CALL FDC_TZERO(T0, T0_ERR, PHI, THETA)
          ELSE
            OK = FTRAKS() 
          ENDIF
          CALL NFROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NF,IDF) 
        ENDIF
        IF (VTXON .AND. CDCON) THEN
          CALL CVROAD(NC,IDC,NV,IDV,PT,NZ,NZCV)
          NZ = NZ + NZCV
        ENDIF
        IF (VTXON .AND. FDCON) THEN
          CALL FVROAD(NF,IDF,NV,IDV,PT,NZ,NZFV)
          NZ = NZ + NZFV
        ENDIF
        IF (FDCON .AND. CDCON) THEN
          CALL FCROAD(NF,IDF,NC,IDC,PT,NZ,NZFC)
          NZ = NZ + NZFC
        ENDIF
        IF (CDONLY .AND. CDCON) THEN
          CALL ZTRCDC(NC,IDC,NZ,NZC)
          NZ = NZ + NZC
        ENDIF
        IF (FDONLY .AND. FDCON) THEN
          CALL FFROAD(NF,IDF,NZ,NZF)
          NZ = NZ + NZF
        ENDIF
        IF (VTONLY .AND. VTXON) THEN
          CALL ZTRVTX(NV,IDV,NZ,NZV)
          NZ = NZ + NZV
        ENDIF
        NZTR = MIN(ZMAX,NZ)
C
C   make global fitting for each ZTRK 
C   (the fitting does not include vertex point)
C      
      IF (MKZFIT) THEN
        CALL VZERO(FITVTX,3)
        CALL VZERO(ERRVTX,3)
        ERRVTX(1) = 9999.0
        DO 201 IZ = 1, NZTR
          CALL ZTRKFT(ZLINKS(IZ),FITVTX,ERRVTX)
  201   CONTINUE
      ENDIF
C
C   update compressed hits banks with ZTRK number
C      
      DO 202 IZ = 1, NZTR
        CALL ZSTCMP(ZLINKS(IZ))
  202 CONTINUE
C
      CALL UCOPY(ZLINKS(1),ZLINKR(1),NZTR)
C
      LZTRH = GZZTRH()
      IQ(LZTRH) = IBSET(IQ(LZTRH),12)
C      CALL ZTRHIS
C
C  deactivate the temporary link area
C
      ZTRLNK(1) = 0
C
 999  RETURN
      END
