      SUBROUTINE VSEGME(TASK,NSEGML) 
C------------------------------------------------------------------
C 
C  Routine for finding track segments in VTX Drift Chamber
C
C  Input:
C  TASK             = 0 for tracking in volumes
C                   = 1 for full tracking
C  Output:
C  NSEGML(0:NLAYER) =  number of segments in layers
C
C 
C  Daria Zieminska JAN., 1988
C-   Updated   4-NOV-1991   Peter M. Grudberg  Add EZRSET, fix PATH 
C                            
C------------------------------------------------------------------
      IMPLICIT NONE  
      INTEGER NLAYER
      PARAMETER (NLAYER=2)
      INTEGER NSEC(0:NLAYER) 
      INTEGER METHOD,INEFF,NGOOD,DUNIT,TASK 
      INTEGER LAYER,SECTOR,NHIT,LLINK,LCHAI,LPOIN,NEL,NWORDS 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LUSER,LSEGM,NTRSG,NZBANK,IXUSER,LVTRH,GZVTRH  
      INTEGER NSEGML(0:NLAYER)
      REAL CONT(18)
      INTEGER IER,ICALL
      DATA ICALL/0/
C---------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET_iarr('NSEC',NSEC,IER)
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZGET('METHOD',METHOD,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
C  Loop over layers and sectors and find track segments  
C                            
      DO 100 LAYER=0,NLAYER 
        CALL GTVLAY(LAYER,NHIT)
        IF (NHIT.LT.8-INEFF) GO TO 99
        DO 200 SECTOR=0,NSEC(LAYER)
          CALL GTVSEC(LAYER,SECTOR,'SEC',0,NEL,NWORDS,CONT)
          CALL UCOPY(CONT,NHIT,1)
          IF (NHIT.LT.8-INEFF) GO TO 200
C                                                                     
C  Routine VCHEKH is called for 'ideal' Monte Carlo data to simulate 
C  two-hit resolution by flagging hits that have drift time within 84 nsec
C  from previous hit on the same wire and to smear the drift and z
C  coordinates.
C
          CALL VCHEKH(TASK,LAYER,SECTOR,NHIT,NGOOD)  
          LUSER=LQ(LHEAD-IZUSER)
          IQ(LUSER+3)=NGOOD
          IQ(LUSER+4)=IQ(LUSER+4)+NGOOD
          IF (NGOOD.LT.8-INEFF) GO TO 200
          IF (METHOD.EQ.1) CALL VSEGLT(LAYER,SECTOR) 
          IF (METHOD.EQ.2) THEN
            CALL VSEGRD(LAYER,SECTOR)     
          ENDIF
C          CALL VPRSEG(DUNIT,LAYER,SECTOR,NGOOD)   
          LUSER=LQ(LHEAD-IZUSER) 
          IF (METHOD.EQ.1) THEN         ! Drop banks hanging form 'USER'
            LLINK=LQ(LUSER-1)
            LCHAI=LQ(LUSER-2)
            IF (LLINK.GT.0) CALL MZDROP(0,LLINK,'L')
            IF (LCHAI.GT.0) CALL MZDROP(0,LCHAI,' ')
          END IF
          IF (METHOD.EQ.2) THEN
            LPOIN=LQ(LUSER-1)
            CALL MZDROP(0,LPOIN,' ')
          END IF
  200   CONTINUE
   99   CONTINUE
        LUSER=LQ(LHEAD-IZUSER) 
        LVTRH=GZVTRH()
        LSEGM=LQ(LVTRH-3-LAYER) 
        NSEGML(LAYER)=NZBANK(0,LSEGM)     ! Number of segments in LAYER
  100 CONTINUE                                                       
 1000 RETURN 
      END       
