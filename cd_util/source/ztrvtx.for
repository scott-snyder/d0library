      SUBROUTINE ZTRVTX(NV,IDV,NZ,NZV)
C------------------------------------------------------------------------
C 
C  Store non-matched VTX tracks as CD tracks. 
C                               
C-   Inputs  : NV:     number of VTX tracks in the road
C-             IDV(I): ID number of VTX tracks in the road
C-             NZ:     number of existing central tracks ZTRK in the road
C-   Outputs : NZV:   number of non-matched VTX tracks in the road
C
C-   Created  06-JAN-1990   Qizhong Li-Demarteau  
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  10-NOV-1991   Qizhong Li-Demarteau  added PATHST and fill ref.
C-                                                link for VTXT
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZ, NZV, NV, IV, IDV(*), NZTRAK
      INTEGER STAT, JBIT, LOCV, IER, IPATH
      INTEGER LZTRK, LZTRH, GZZTRH, GZVTXT
      INTEGER NTOT
      CHARACTER*4 VPATH
      EQUIVALENCE (IPATH, VPATH)
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
        STAT = IQ(LOCV)
        IF ((JBIT(STAT,IUSED).EQ.0).AND.(JBIT(STAT,IUSED2)).EQ.0) THEN
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
          NTOT = NZ + NZV
          NTOT = MIN(NTOT,ZMAX)
          ZLINKS(NTOT) = LZTRK
        END IF
  400 CONTINUE
C
  999 RETURN
      END
