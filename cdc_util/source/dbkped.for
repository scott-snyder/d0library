      SUBROUTINE DBKPED
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book pedestal and t0 histograms 
C-
C-   Controls: called by DTRDIA
C-             ENTRY points: DTSPED(HSTPED) 
C-
C-   Created  16-SEP-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTPED
      LOGICAL EZERROR
      INTEGER IER
      INTEGER SEC, MAXSEC, ERR
      INTEGER IDPDST, IDPSGM, IDSWT0, IDDLT0, IDGAIN, IDGSGM
      CHARACTER*2 SECNUM(32)
      CHARACTER*33  TITLE1, TITLE6
      CHARACTER*37  TITLE2
      CHARACTER*20  TITLE3, TITLE4, TITLE5
      CHARACTER*42  TITLE
      DATA    TITLE1/'PEDESTALS PER WIRE          (SEC '/,
     +        TITLE2/'SIGMA OF THE PEDESTALS PER WIRE (SEC '/,
     +        TITLE3/'SW T0 PER WIRE (SEC '/,
     +        TITLE4/'DL T0 PER WIRE (SEC '/,
     +        TITLE5/'GAINS PER WIRE (SEC '/,
     +        TITLE6/'SIGMA OF THE GAINS PER WIRE (SEC '/
      DATA      SECNUM/' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',
     +                 ' 8',' 9','10','11','12','13','14','15',
     +                 '16','17','18','19','20','21','22','23',
     +                 '24','25','26','27','28','29','30','31'/    
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DBKPED',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      CALL EZRSET
C
      YES=.FALSE.
      CALL GETPAR(1,' Book pedestal and T0 histograms? Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
      DO 100 SEC = 0, MAXSEC
        IDPDST = 2300 + SEC      
        TITLE = TITLE1//SECNUM(SEC+1)//') $'
        CALL HBOOK1(IDPDST,TITLE,46,-1.5,44.5,0.)
        IDPSGM = 2350 + SEC
        TITLE = TITLE2//SECNUM(SEC+1)//') $'
        CALL HBOOK1(IDPSGM,TITLE,46,-1.5,44.5,0.)
        IDSWT0 = 2400 + SEC
        TITLE = TITLE3//SECNUM(SEC+1)//') $'
        CALL HBOOK1(IDSWT0,TITLE,30,-1.5,28.5,0.)
        IDDLT0 = 2450 + SEC
        TITLE = TITLE4//SECNUM(SEC+1)//') $'
        CALL HBOOK1(IDDLT0,TITLE,16,0.5,16.5,0.)
        IDGAIN = 2500 + SEC
        TITLE = TITLE5//SECNUM(SEC+1)//') $'
        CALL HBOOK1(IDGAIN,TITLE,30,-1.5,28.5,0.)
        IDGSGM = 2550 + SEC
        TITLE = TITLE6//SECNUM(SEC+1)//') $'
        CALL HBOOK1(IDGSGM,TITLE,30,-1.5,28.5,0.)
 100  CONTINUE
C
  999 RETURN
C---------------------------------------------------------------------------
      ENTRY DTSPED(HSTPED)
      HSTPED = YES
      RETURN
      END
