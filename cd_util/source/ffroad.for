      SUBROUTINE FFROAD(NF,IDF,NZ,NZF)
C------------------------------------------------------------------------
C
C-   Purpose and Methods : Store non-matched FDC tracks as CD tracks.
C
C-   Inputs  : NF:     number of FDC tracks in the road
C-             IDF(I): ID number of FDC tracks in the road
C-             NZ:     number of existing central tracks ZTRK in the road
C-   Outputs : NZF:   number of central tracks ZTRK in the FF road
C-
C-   Created  xx-NOV-1988   Daria Zieminska
C-   Updated  21-NOV-1989   Qizhong Li-Demarteau  change ZTRK bank structure
C-                                                to use reference link
C-   Updated  05-APR-1990   Qizhong Li-Demarteau  use PATH and BKZTRK
C-   Updated   8-NOV-1990   Jeffrey Bantly  add SAVE statement
C-   Updated  29-JAN-1991   Qizhong Li-Demarteau  added a check for IDF(IF) 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  10-NOV-1991   Qizhong Li-Demarteau  fill ref. link for FDCT
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZ, NZF, NF, IF, IDF(*), NZTRAK
      INTEGER STAT, JBIT, LOCF, IER, IPATH
      INTEGER LZTRK, LZTRH, GZZTRH, GZFDCT
      INTEGER NTOT
      CHARACTER*4 FPATH
      EQUIVALENCE (IPATH, FPATH)
      LOGICAL EZERROR
      LOGICAL FIRST
      SAVE FIRST,IPATH
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      NZF = 0
      IF (NF .LE. 0) RETURN
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','FFROAD',
     &    'Unable to find bank FTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('FPATH',IPATH,IER)
        CALL EZRSET
      END IF
      LZTRH = GZZTRH()
      IF (LZTRH.EQ.0) GO TO 999
      NZTRAK = IQ(LZTRH+2)
C
C  Now to build ZTRKs only containing FDC tracks, which do not match with
C  any VTX track
C
      DO 400 IF = 1, NF
        IF (IDF(IF) .LE. 0) THEN
          NF = IF - 1
          GOTO 999
        ENDIF
        CALL PATHST(FPATH)
        LOCF = GZFDCT(IDF(IF))
        IF (LOCF.EQ.0) GO TO 400
        CALL PATHRS
        STAT = IQ(LOCF)
        IF (JBIT(STAT,IUSED).EQ.0) THEN
          CALL MZFLAG(0,LOCF,IUSED,' ')
          CALL BKZTRK(LZTRK)
          LZTRH = GZZTRH()
          IF (LZTRH.EQ.0) GO TO 400
          NZF = NZF + 1
          CALL PATHST(FPATH)
          LOCF = GZFDCT(IDF(IF))
          IF (LOCF.EQ.0) GO TO 400
          CALL PATHRS
          IQ(LZTRH + 2) = IQ(LZTRH + 2) + 1
          LQ(LOCF - 2) = LZTRK
          LQ(LZTRK - 8) = LOCF
          IQ(LZTRK - 5) = NZTRAK + NZF
          IQ(LZTRK + 4) = IQ(LOCF - 5)
          Q(LZTRK + 6) = 0.0
          NTOT = NZ + NZF
          NTOT = MIN(NTOT,ZMAX)
          ZLINKS(NTOT) = LZTRK
        END IF
  400 CONTINUE
C
  999 RETURN
      END
