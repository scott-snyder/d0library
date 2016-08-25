
      SUBROUTINE DSVCHN(IDEPTH,LINK,IPASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store chain found in DCLIMB in Zebra
C-                         bank CHAI
C-                 For IPASS=1 store chains with no missing hits,
C-                     IPASS=2                   1  missing hit,etc.  
C-
C-   Inputs  : 
C-   Inputs  : IDEPTH     is the number of links on chain.
C              LINK       is an array of links on chain.
C              PASS
C-   Outputs : none
C-
C-   Created   6-NOV-1989   joey thompson: based on SAVCHN
C-   Updated   4-DEC-1990   Qizhong Li-Demarteau  fixed the call to MZBOOK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'                             
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:DLTPAR.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'
      INTEGER LUSER,LSTART
      INTEGER NCHAIN,IDEPTH,LINK(IDEPTH),ILINK,LCHAI,NHIT
      INTEGER IGAP,IPASS,LLINK,LOCL,IWR1,NWORDS 
      LOGICAL NEW
      NHIT=IDEPTH+1                   ! number of hits on chain
      NWORDS=NCDCEL*NMXCHN+1
      LUSER=LQ(LHEAD-IZUSER) 
      LCHAI=LQ(LUSER-2)
      IF (LCHAI.LE.0) THEN
C
C  Create ZEBRA bank CHAI 
C
        CALL MZBOOK(IXMAIN,LCHAI,LUSER,-2,'CHAI',0,0,NWORDS,2,0)
      END IF
      NEW=.FALSE.
      LLINK=LQ(LUSER-1)
      DO 100 ILINK=1,IDEPTH                 
        IF (IQ(LCHAI+1) .EQ. NMXCHN) GO TO 999
        LOCL=(LLINK + ((LINK(ILINK)-1)*6))
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
      IF (NEW.EQV..FALSE.) GO TO 999
      IQ(LCHAI+1)=IQ(LCHAI+1)+1             ! increment number of chains
      NCHAIN=IQ(LCHAI+1)
      LSTART=LCHAI+1+(NCHAIN-1)*NCDCEL
      IQ(LSTART+1)=NHIT            
      DO 200 ILINK=1,IDEPTH                 
        IQ(LSTART+1+ILINK)=LINK(ILINK)      ! links on chain  
  200 CONTINUE  
  999 RETURN
      END            
