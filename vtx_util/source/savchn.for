      SUBROUTINE SAVCHN(IDEPTH,LINK,IPASS)
C------------------------------------------------------------------------
C
C  Store chain found by subroutine CLIMB in Zebra bank CHAI.
C  Input:
C         IDEPTH     is the number of links on chain.
C         LINK       is an array of links on chain.
C         PASS
C
C  For IPASS=1 store chains with no missing hits,
C      IPASS=2                   1  missing hit,etc.  
C
C  Daria Zieminska  March 1986
C                              
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LUSER,LSTART
      INTEGER NCHAIN,IDEPTH,LINK(IDEPTH),ILINK,LCHAI,NHIT,NZBANK
      INTEGER IGAP,IPASS,LZFIND,LLINK,LOCL,IWR1,MAXCHN,NWORDS 
      LOGICAL NEW
      INTEGER IER,ICALL 
      DATA ICALL/0/
C---------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('MAXCHN',MAXCHN,IER) ! max number of chains in a
                                        ! sector
        CALL EZRSET
        NWORDS=8*MAXCHN+1
        ICALL=1
      END IF
      NHIT=IDEPTH+1                   ! number of hits on chain
      LUSER=LQ(LHEAD-IZUSER) 
      LCHAI=LQ(LUSER-2)
      IF (LCHAI.LE.0) THEN
C
C  Create ZEBRA bank CHAI 
C
        CALL MZBOOK(0,LCHAI,LUSER,-2,'CHAI',0,0,NWORDS,2,0)
      END IF
      NEW=.FALSE.
      LLINK=LQ(LUSER-1)
      DO 100 ILINK=1,IDEPTH                 
        LOCL=LZFIND(0,LLINK,LINK(ILINK),-5)
        IGAP=IQ(LOCL+4)-IQ(LOCL+3)
        IF (ILINK.EQ.1) THEN
          IWR1=IQ(LOCL+3) 
          IF (IWR1.EQ.IPASS-1.OR.IGAP.EQ.IPASS) THEN
            NEW=.TRUE.
            GO TO 300
          END IF
        ELSE
          IF (IGAP.EQ.IPASS) THEN 
            NEW=.TRUE.
            GO TO 300
          END IF
        END IF
  100 CONTINUE  
  300 CONTINUE
      NCHAIN=IQ(LCHAI+1)
      IF (NEW.EQ..FALSE..OR.NCHAIN.EQ.MAXCHN) GO TO 1000
      IQ(LCHAI+1)=IQ(LCHAI+1)+1             ! increment number of chains
      NCHAIN=IQ(LCHAI+1)
      LSTART=LCHAI+1+(NCHAIN-1)*8
      IQ(LSTART+1)=NHIT            
      DO 200 ILINK=1,IDEPTH                 
        IQ(LSTART+1+ILINK)=LINK(ILINK)      ! links on chain  
  200 CONTINUE  
 1000 RETURN
      END            
