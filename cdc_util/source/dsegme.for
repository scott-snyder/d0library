      SUBROUTINE DSEGME
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for finding track segments 
C-                         in CDC using the Link and Tree method 
C-
C-   Inputs  :  none
C-   Outputs :  none
C-
C-   Created   6-NOV-1989   joey    based on VSEGME
C-   Updated   4-DEC-1990   Qizhong Li-Demarteau  fixed the call to MZBOOK
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET, EZERROR
C-                                                and SAVE statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE  
      INCLUDE 'D0$INC:ZEBCOM.INC'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK'                             
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INTEGER NSEC(0:NCDLYR-1),INEFF,NGOOD 
      INTEGER NHIT,LLINK,LCHAI,IPATH,IER,ICALL 
      CHARACTER*4 PATH,DPATH
      EQUIVALENCE (IPATH,DPATH)
      LOGICAL EZERROR
C
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DSEGME',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZGET('NSEC',NSEC,IER)
        CALL EZGET('DPATH',IPATH,IER)
        CALL EZRSET
        ICALL=1
      END IF
      CALL BKDTRH
      CALL MZBOOK(IXMAIN,LUSER,LHEAD,-IZUSER,'USER',6,6,10,2,0)
C
C  Loop over layers and sectors and find track segments  
C                            
      DO 100 LAYER=0,(NCDLYR-1) 
C Need to get the number of hits in a given layer here....
        NHIT = IQ(LDLYR(LAYER) + 1)        
        IF (NHIT.LT.7-INEFF) GO TO 99
        DO 200 SECTOR=0,NSEC(LAYER)
C  Need the number of hits in the sector given by layer and sector loop number
          NHIT = IQ(LDSEC(SECTOR,LAYER) + 1)
          IF (NHIT.LT.(NCDCEL-INEFF)) GO TO 200
          PATH=DPATH 
          CALL PATHST(PATH)
C  For now we assume that all hits are good hits initially
C      Later we may implement a routine to flag bad hits using
C      the status word in DSEC
          NGOOD=NHIT
          LUSER=LQ(LHEAD-IZUSER)
          IQ(LUSER+3)=NGOOD
          IQ(LUSER+4)=IQ(LUSER+4)+NGOOD
          IF (NGOOD.LT.(NCDCEL-INEFF)) GO TO 200
          CALL DSEGLT  
          LUSER=LQ(LHEAD-IZUSER) 
          LLINK=LQ(LUSER-1)               !Drop banks hanging from bank "user"
          LCHAI=LQ(LUSER-2)
          IF (LLINK.GT.0) CALL MZDROP(0,LLINK,' ')
          IF (LCHAI.GT.0) CALL MZDROP(0,LCHAI,' ')
  200   CONTINUE
   99   CONTINUE
        LUSER=LQ(LHEAD-IZUSER) 
        PATH=DPATH 
        CALL PATHST(PATH)
  100 CONTINUE                                                       
      CALL MZDROP(0,LUSER,'L')  
  999 RETURN
      END
