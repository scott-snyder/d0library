      SUBROUTINE DFITCH(ICHAIN,NHIT)
C--------------------------------------------------------------------
C                        
C  Purpose: Fit a straight line to hits on chain. 
C           Store the chain as a track segment in Zebra bank DTSG.  
C               
C  Input:  ICHAIN = chain to be fitted
C          NHIT   = number of hits on the chain
C
C  Output: none
C
C  Created   6-Nov-1989   joey thompson:  Based on VFITCH
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET, EZERROR
C-                                                and SAVE statement
C                  
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'                             
      INCLUDE 'D0$INC:CDCLNK.INC'                             
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK'                             
      INTEGER ICHAIN,NHIT,LRWIR(NCDCEL),IHIT(NCDCEL),IDWIRE,NWIRES
      INTEGER LCHAI,ILINK,LLINK,IPAL,NEL,NWORDS
      INTEGER LOC,ID,J1,IH,JH,LR,NHUSED,IBSET,JBIT 
      INTEGER LUSER,LDRFT,GZDRFT,LDALS,GZDALS 
      REAL DRIFT,PHI,XG,YG,CHISQ,CHIDF 
      REAL XHIT(NCDCEL),YHIT(NCDCEL),SXY(NCDCEL),WT(NCDCEL),ERRPHI,ERRD
      REAL PI,PI2 
      REAL CHIMAX
      LOGICAL BTEST
      LOGICAL EZERROR
      INTEGER IER,ICALL
C
      SAVE ICALL
      DATA PI,PI2/3.141593,6.283185/
      DATA ICALL/0/
C---------------------------------------------------------------------------
      NWIRES = NCDCEL-1
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DFITCH',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
      LUSER=LQ(LHEAD-IZUSER)
      LLINK=LQ(LUSER-1)                                          
      LCHAI=LQ(LUSER-2)
      LDALS=GZDALS(LAYER,SECTOR)
C
C  Loop over hits on chain and find their coordinates
C 
      DO 200 ID=1,NHIT                     
        IF (ID.LT.NHIT) THEN
          ILINK=IQ(LCHAI+1+(ICHAIN-1)*7+1+ID)
          LOC=(LLINK + (ILINK-1)*6)
          IF (IQ(LOC+6).EQ.1) GO TO 999         ! link deleted
          J1=1                
        END IF
        IF (ID.EQ.NHIT) J1=2
        IHIT(ID)=IQ(LOC+J1)/2
        LR=JBIT(IQ(LOC+J1),1)
        IDWIRE=IQ(LOC+J1+2) 
        LRWIR(ID)=IDWIRE*2+LR
        LDRFT=GZDRFT()
        DRIFT=(Q(IHIT(ID)+2+LR))-(C(LDRFT+26+IDWIRE))
        IPAL=LDALS+6+IC(LDALS+6)*IDWIRE
        XHIT(ID) = C(IPAL+1)+DRIFT*C(LDALS+3)  ! hit coordinates in D0 frame
        YHIT(ID) = C(IPAL+2)+DRIFT*C(LDALS+4)
        WT(ID)   = 1./(Q(IHIT(ID)+5))**2
  200 CONTINUE             
C
C  Fit in x-y plane
C
      CALL DFTLIN(XHIT,YHIT,WT,NHIT,PHI,XG,YG,ERRD,ERRPHI,CHISQ) 
      CHIDF=(CHISQ/FLOAT(NHIT-2))
      IF (CHIDF.LT.CHIMAX) THEN
      DO 300 ID=1,NHIT                     
        SXY(ID)=(XHIT(ID)-XG)**2+(YHIT(ID)-YG)**2
        SXY(ID)=SQRT(SXY(ID))
        IF (XHIT(ID)**2+YHIT(ID)**2.LT.XG**2+YG**2) 
     X  SXY(ID)=-SXY(ID)
  300 CONTINUE             
C  Good track segment.
C  Flag hits on track segment. 
C
          NHUSED=0
          DO 400 ID=1,NHIT   
            JH=IHIT(ID)
            IH=IQ(JH+9)
            IF (.NOT.BTEST(IH,2)) THEN
              IQ(JH+9)=IBSET(IH,2)             ! Flag hit ID
              NHUSED=NHUSED+1
            END IF
  400     CONTINUE                 

          IF (XG.LT.0..AND.YG.LT.0.) PHI=PHI+PI
          IF (XG.GT.0..AND.YG.LT.0.) PHI=PHI+PI2
          IF (XG.LT.0..AND.YG.GT.0.) PHI=PHI+PI
C
C  Store this segment in Zebra bank DTSG.
C
          CALL DLDTSG(NHIT,LRWIR,IHIT,PHI,XG,YG,CHISQ,ERRD,ERRPHI)
C
C  Increment number of used hits in this sector
C
          IQ(LUSER+2)=IQ(LUSER+2)+NHUSED        
      END IF
  999 RETURN  
      END        
