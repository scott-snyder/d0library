      SUBROUTINE END_D0RECO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      End reconstruction job
C-
C-   Created  11-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*80 FILE_NAME
      CHARACTER*26 TIMSTR
      LOGICAL STORE_HST,VERIFY
      INTEGER SSUNIT,OUT,IER,OUTLEN,ISTAT,TRULEN
      INTEGER TRANS_LOG,EVTCNT,NREAD,NWSTA,NWDST,N
      INTEGER D0_TIME,OFFTIM
      LOGICAL EZERR
C----------------------------------------------------------------------
C
C        final report to Program Manager
      CALL D0RECO_END_REPORT
C
C        route histogram printouts to summary output file
      OUT=SSUNIT()
C        give final time
      CALL GET_TIME(TIMSTR)
      D0_TIME=OFFTIM()
      WRITE(OUT,1010) TIMSTR,D0_TIME
C
C         pick RCP options
      CALL EZPICK('D0RECO_RCP')
      CALL EZGET('STORE_HST',STORE_HST,IER)
      CALL EZGET('VERIFY',VERIFY,IER)
      CALL EZRSET
C
C       write out number of events written to STA and DST
      NREAD=EVTCNT()
      WRITE(OUT,1000) NREAD
      CALL EVWRITES('STA',NWSTA)
      WRITE(OUT,1011) NWSTA
      IF(NWSTA.GT.0) CALL EVSUM_MULT(OUT,1)
      CALL EVWRITES('DST',NWDST)
      WRITE(OUT,1012) NWDST
      IF(NWDST.GT.0) CALL EVSUM_MULT(OUT,2)
      CALL EVCLWO('ALL')
      CALL ERRSUM(OUT)
      CALL RECO_JOB_SUMMARY_PBD
C
C       store histograms if requested
C
      IF(STORE_HST) THEN
        CALL EZPICK('FILES_RCP')
        IF(EZERR(IER)) THEN
C                   translate logical name
          ISTAT=TRANS_LOG('HISTOGRAMS',FILE_NAME,OUTLEN) 
        ELSE
C               construct name starting from tape name
          CALL EZ_GET_CHARS('TAPE_NAME',N,FILE_NAME,IER)
          OUTLEN=TRULEN(FILE_NAME)
          FILE_NAME=FILE_NAME(1:OUTLEN)//'.HST4'
          OUTLEN=OUTLEN+5
        ENDIF
        WRITE(OUT,1001) FILE_NAME(1:OUTLEN)
        CALL HCDIR('//PAWC',' ')
        CALL HRPUT(0,FILE_NAME,'NT')
        CALL EZRSET
      ENDIF
      IF(VERIFY) THEN
        CALL HOUTPU(OUT)
        CALL HERMES(OUT)
      ENDIF
      IF(VERIFY) THEN
        CALL HLDIR('//PAWC','T')
        CALL HPDIR('//PAWC','T')
      ENDIF
  999 RETURN
 1000 FORMAT(/ ' NUMBER OF RECORDS READ FROM INPUT FILE',I5)
 1011 FORMAT( /,'  NUMBER OF RECORDS WRITTEN TO STA FILE',I5)
 1012 FORMAT( /,'  NUMBER OF RECORDS WRITTEN TO DST FILE',I5)
 1001 FORMAT(/,'  HISTOGRAMS STORED IN ',A)
 1010 FORMAT(//'    PROCESSING FINISHED AT ',A,' D0 time=',I10)
      END
