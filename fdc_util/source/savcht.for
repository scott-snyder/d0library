      SUBROUTINE SAVCHT(IDEPTH,LINK,IPASS)
C------------------------------------------------------------------------
C
C  Purpose and Methods: Store chain found by subroutine CLIMB 
C                       in Zebra bank CHAI.
C
C  Input: IDEPTH = Number of links on chain.
C         LINK   = Array of links on chain.
C         IPASS  = Number of allowed missing hits, i.e.
C                  For IPASS=1 store chains with no missing hits,
C                      IPASS=2                   1  missing hit,etc.  
C
C-   Created  xx-DEC-1988   Daria Zieminska  
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C                              
C------------------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC'
C
      INTEGER LSTART,IER
      INTEGER NCHAIN,IDEPTH,LINK(IDEPTH),ILINK,LCHAI,NHIT,ICALL
      INTEGER IGAP,IPASS,LLINK,LOCL,IWR1,NWORDS 
      PARAMETER (NWORDS=NBTSEN*MXNSEG+1)
      INTEGER LZFIND
      LOGICAL NEW
      SAVE ICALL
      DATA ICALL /0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        ICALL=1
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
      ENDIF
C
      NHIT=IDEPTH+1                   ! number of hits on chain
      LCHAI=LQ(LFLOC-2)
      IF (LCHAI.LE.0) THEN
C
C  Create ZEBRA bank CHAI 
C
        CALL MZBOOK(IXMAIN,LCHAI,LFLOC,-2,'CHAI',0,0,NWORDS,2,0)
      END IF
      NCHAIN=IQ(LCHAI+1)
      IF (NCHAIN.GE.MXNSEG) GO TO 999
C
C  Check to make sure chain has not been stored before.
C
      NEW=.FALSE.
      LLINK=LQ(LFLOC-1)
      DO 100 ILINK=1,IDEPTH                 
        LOCL=LZFIND(IXCOM,LLINK,LINK(ILINK),-5)
        IGAP=IQ(LOCL+4)-IQ(LOCL+3)
        IF (IGAP.EQ.IPASS) THEN 
          NEW=.TRUE.
          GO TO 300
        END IF
  100 CONTINUE  
  300 CONTINUE
      IF (.NOT. NEW) GO TO 999
C
C  Store chain as its links on the chain, if new.
C
      IQ(LCHAI+1)=IQ(LCHAI+1)+1             ! increment number of chains
      NCHAIN=IQ(LCHAI+1)
      LSTART=LCHAI+1+(NCHAIN-1)*8
      IQ(LSTART+1)=NHIT            
      DO 200 ILINK=1,IDEPTH                 
        IQ(LSTART+1+ILINK)=LINK(ILINK)      ! links on chain  
  200 CONTINUE  
C------------------------------------------------------------------------
  999 RETURN
      END            
