      SUBROUTINE ZTRCDC(NC,IDC,NZ,NZC)
C------------------------------------------------------------------------
C 
C  Store non-matched CDC tracks as CD tracks. 
C                               
C-   Inputs  : NC:     number of CDC tracks in the road
C-             IDC(I): ID number of CDC tracks in the road
C-             NZ:     number of existing central tracks ZTRK in the road
C-   Outputs : NZC:   number of non-matched CDC tracks in the road
C
C-   Created  06-JAN-1990   Qizhong Li-Demarteau  
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  10-NOV-1991   Qizhong Li-Demarteau  fill ref. link for DTRK 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZ, NZC, NC, IC, IDC(*), NZTRAK
      INTEGER STAT, JBIT, LOCC, IER, IPATH
      INTEGER LZTRK, LZTRH, GZZTRH, GZDTRK
      INTEGER NTOT
      CHARACTER*4 DPATH
      EQUIVALENCE (IPATH, DPATH)
      LOGICAL FIRST
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      NZC = 0
      IF (NC .LE. 0) RETURN
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRCDC',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DPATH',IPATH,IER)
        CALL EZRSET
      END IF
      LZTRH = GZZTRH()
      NZTRAK = IQ(LZTRH+2)      
C
C  Now to build ZTRKs only containing CDC tracks, which do not match with
C  any VTX track
C
      DO 400 IC = 1, NC
        IF (IDC(IC) .LE. 0) THEN
          NC = IC - 1
          GOTO 999
        ENDIF
        CALL PATHST(DPATH)
        LOCC = GZDTRK(IDC(IC))
        CALL PATHRS
        STAT = IQ(LOCC)
        IF (JBIT(STAT,IUSED).EQ.0) THEN
          CALL MZFLAG(0,LOCC,IUSED,' ')
          CALL BKZTRK(LZTRK)
          NZC = NZC + 1
          LZTRH = GZZTRH()
          CALL PATHST(DPATH)
          LOCC = GZDTRK(IDC(IC))
          CALL PATHRS
          IQ(LZTRH + 2) = IQ(LZTRH + 2) + 1
          LQ(LOCC - 2) = LZTRK
          LQ(LZTRK - 7) = LOCC
          IQ(LZTRK - 5) = NZTRAK + NZC
          IQ(LZTRK + 3) = IQ(LOCC - 5)
          Q(LZTRK + 6) = 0.0
          NTOT = NZ + NZC
          NTOT = MIN(NTOT,ZMAX)
          ZLINKS(NTOT) = LZTRK
        ENDIF
  400 CONTINUE
C
  999 RETURN
      END

