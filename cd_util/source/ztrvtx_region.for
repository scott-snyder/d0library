      SUBROUTINE ZTRVTX_REGION(NV,IDV,THE1,THE2,NZ,NZV)
C------------------------------------------------------------------------
C- 
C-   Purpose and Methods : Store non-matched VTX tracks in the theta region 
C-                         of (the1,the2) to be CD track ZTRKs. 
C-                              
C-   Inputs  : NV:     number of VTX tracks in the road
C-             IDV(I): ID number of VTX tracks in the road
C-             THE1:   the theta minimum for the region
C-             THE2:   the theta maxmum for the region
C-             NZ:     number of existing central tracks ZTRK in the road
C-   Outputs : NZV:   number of non-matched VTX tracks in this region to 
C-                    build ZTRKs
C-
C-   Created  30-SEP-1991   Qizhong Li-Demarteau  
C-
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZ, NZV, NV, IV, IDV(*), NZTRAK
      INTEGER STAT, JBIT, LOCV, IER, IPATH
      INTEGER LZTRK, LZTRH, GZZTRH, GZVTXT
      CHARACTER*4 VPATH
      EQUIVALENCE (IPATH, VPATH)
      REAL    THETA, THE1, THE2
      LOGICAL FIRST
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      NZV = 0
      IF (NV .LE. 0) RETURN
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRVTX',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZRSET
      END IF
      LZTRH = GZZTRH()
      NZTRAK = IQ(LZTRH+2)      
C
C  Now to build ZTRKs only containing VTX tracks, which do not match with
C  any CDC or FDC track
C
      DO 400 IV = 1, NV
        IF (IDV(IV) .LE. 0) THEN 
          NV = IV - 1
          GOTO 999
        ENDIF
        CALL PATHST(VPATH)
        LOCV = GZVTXT(IDV(IV))
        CALL PATHRS
        THETA = Q(LOCV+9)
        IF (THETA .LT. THE1 .OR. THETA .GT. THE2)  GOTO 400
        STAT = IQ(LOCV)
        IF ((JBIT(STAT,IUSED).EQ.0).AND.(JBIT(STAT,IUSED2).EQ.0)) THEN
          CALL MZFLAG(0,LOCV,IUSED,' ')
          CALL BKZTRK(LZTRK)
          NZV = NZV + 1
          LZTRH = GZZTRH()
          LOCV = GZVTXT(IDV(IV))
          IQ(LZTRH + 2) = IQ(LZTRH + 2) + 1
          LQ(LOCV - 2) = LZTRK
          LQ(LZTRK - 6) = LOCV
          IQ(LZTRK - 5) = NZTRAK + NZV
          IQ(LZTRK + 2) = IQ(LOCV - 5)
          Q(LZTRK + 6) = 0.0
          ZLINKS(NZ + NZV) = LZTRK
        END IF
  400 CONTINUE
C
  999 RETURN
      END
