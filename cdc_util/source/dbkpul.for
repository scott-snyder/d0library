      SUBROUTINE DBKPUL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms: pulse <area>, <width> and <height> VS wire #
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY points: DTSPUL(HSTPUL,WITHDL) 
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, WITHDL, HSTPUL, ALSODL
      LOGICAL EZERROR
      INTEGER IER
      INTEGER HISID1, HISID2, HISID3, I, MAXSEC, ERR
      CHARACTER*80   TITLE1, TITLE2, TITLE3
      CHARACTER*26   TLSWDL, TLSW
      CHARACTER*2    SECNUM(32)
      DATA        TLSW/'vs SW wire number    (sec '/
      DATA      TLSWDL/'vs SW+DL wire number (sec '/
      DATA      SECNUM/' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',
     +                 ' 8',' 9','10','11','12','13','14','15',
     +                 '16','17','18','19','20','21','22','23',
     +                 '24','25','26','27','28','29','30','31'/    
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DBKPUL',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      CALL EZRSET
C
C
C
        YES = .FALSE.
        ALSODL = .FALSE.
        CALL GETPAR(1,' Book pulse area, width and height VS wire #
     &  histograms? Y/N>','L',YES)
        IF(.NOT.YES) GOTO 999
C
        CALL GETPAR(1,'  Wire # includes delay lines? Y/N>',
     &    'L',ALSODL)
        IF(ALSODL) THEN
          DO 100 I = 0, MAXSEC
            HISID1 = 2000 + I
            HISID2 = 2100 + I
            HISID3 = 2200 + I
            TITLE1 = 'peak area '//TLSWDL//SECNUM(I+1)//') $'
            TITLE2 = 'peak width '//TLSWDL//SECNUM(I+1)//') $'
            TITLE3 = 'peak height '//TLSWDL//SECNUM(I+1)//') $'
c            CALL HBAVER(HISID1,TITLE1,46,-1.5,44.5,1.,1001.)
c            CALL HBAVER(HISID2,TITLE2,46,-1.5,44.5,1.,101.)
c            CALL HBAVER(HISID3,TITLE3,46,-1.5,44.5,1.,301.)
            CALL HBPROF(HISID1,TITLE1,46,-1.5,44.5,1.,1001.,' ')
            CALL HBPROF(HISID2,TITLE2,46,-1.5,44.5,1.,101.,' ')
            CALL HBPROF(HISID3,TITLE3,46,-1.5,44.5,1.,451.,' ')
            
 100      CONTINUE
        ELSE
          DO 200 I = 0, MAXSEC
            HISID1 = 2050 + I
            HISID2 = 2150 + I
            HISID3 = 2250 + I
            TITLE1 = 'peak area '//TLSW//SECNUM(I+1)//') $'
            TITLE2 = 'peak width '//TLSW//SECNUM(I+1)//') $'
            TITLE3 = 'peak height '//TLSW//SECNUM(I+1)//') $'
C            CALL HBAVER(HISID1,TITLE1,30,-1.5,28.5,1.,1001.)
C            CALL HBAVER(HISID2,TITLE2,30,-1.5,28.5,1.,101.)
C            CALL HBAVER(HISID3,TITLE3,30,-1.5,28.5,1.,301.)
            CALL HBPROF(HISID1,TITLE1,30,-1.5,28.5,1.,1001.,' ')
            CALL HBPROF(HISID2,TITLE2,30,-1.5,28.5,1.,101.,' ')
            CALL HBPROF(HISID3,TITLE3,30,-1.5,28.5,1.,301.,' ')
 200      CONTINUE
        ENDIF  
C
  999 RETURN
C---------------------------------------------------------------------------
      ENTRY DTSPUL(HSTPUL,WITHDL)
      HSTPUL = YES
      WITHDL = ALSODL
      RETURN
      END
