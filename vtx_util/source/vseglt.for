      SUBROUTINE VSEGLT(LAYER,SECTOR)
C------------------------------------------------------------------------
C                                                                        
C  Routine for finding track segments in x-y plane in cell (LAYER,SECTOR) 
C  of Vertex Drift Chamber, using the 'link_and_tree' method.
C  Uses 'active' hits, selected earlier. Depending on application, 'active'
C  hits are all hits in the cell or hits along a road.  
C            
C  Inputs:
C          LAYER,SECTOR
C
C  Daria Zieminska Mar. 1986
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LAYER,SECTOR
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER MXNEXT,MAXLNK,MAXCHN
      PARAMETER (MXNEXT=8)
      PARAMETER (MAXLNK=300)
      INTEGER LLINK,NLINK,NNEXT(MAXLNK),INEXT(MXNEXT,MAXLNK) 
      INTEGER LUSER 
      INTEGER IPASS,MXPASS,INEFF,ILINK,LCHAI,NACTIV,NHUSED
      INTEGER NCHAIN,NZBANK,ICHAIN,NHIT
      INTEGER IQUEST
      COMMON /QUEST/IQUEST(100)
      INTEGER LZFIND
      INTEGER IGAP1,IGAP2,IWRB1,LOCL,IBRNCH,IGAP(MAXLNK),IWRB(MAXLNK)
      INTEGER IE1,IWRE1,IB2,IWRB2,IWRE2,LOC2,LINK2,NLINK2,NZFIND
      INTEGER ISTART,NSTART,IWIRE,ICALL
      REAL SL1,SL2,DSL,DSLMAX(4)
      DATA ICALL/0/
      INTEGER IER 
C-------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('DSLMAX',DSLMAX,IER)
        CALL EZGET('INEFF',INEFF,IER) ! max number of missing hits in a segment
        CALL EZGET('MAXCHN',MAXCHN,IER) ! max number of chains in a
                                        ! sector
        CALL EZRSET
        MXPASS=INEFF+1
        ICALL=1
      END IF
C
C  Do the work in 'passes'.
C  For IPASS=1 find track segments with no missing hits,
C      .......
C      IPASS=4                          3  missing hits. 
C
      LUSER=LQ(LHEAD-IZUSER)
      IQ(LUSER+1)=0
      IQ(LUSER+2)=0
      CALL VZERO(NNEXT,MAXLNK)
      CALL VZERO(INEXT,MAXLNK*8)
      NACTIV=IQ(LUSER+3)               ! number of 'active' hits
      NCHAIN=0
      DO 100 IPASS=1,MXPASS
        IF (IPASS.GT.1)   CALL VDELET(LAYER,SECTOR) ! delete used links
C
C  Build links with gap=IPASS
C
        CALL VLINKS(LAYER,SECTOR,NACTIV,IPASS)  
C                         
        LUSER=LQ(LHEAD-IZUSER)
        LLINK=LQ(LUSER-1)        
        NLINK=IQ(LUSER+1)                    ! number of links
        IF (NLINK.LT.3) GO TO 100
        IF (NLINK.GT.MAXLNK) GO TO 1000
C
C  Build elementary trees for all links.   
C  A tree has to satisfy the local cut: 
C  ABS(SL1-SL2).LT.DSLMAX, where SL1,SL2 are slopes of the two links 
C  (defined by Y=SL*X+Y0).          
C
      DO 200 ILINK=1,NLINK
        LOCL=LZFIND(0,LLINK,ILINK,-5)
        IF (IPASS.GT.1) THEN
          IF (LOCL.LE.0) THEN                ! used link
            IF (NNEXT(ILINK).GT.0) THEN
              DO 201 IBRNCH=1,NNEXT(ILINK)
                INEXT(IBRNCH,ILINK)=0        ! cancel its branches
  201         CONTINUE
              NNEXT(ILINK)=0         
              GO TO 200                      ! skip used link 
            END IF
          END IF
        END IF
        SL1=Q(LOCL+5)             ! slope of link ILINK
        IE1=IQ(LOCL+2)            ! 2-nd hit on link ILINK
        IWRB1=IQ(LOCL+3)          
        IWRE1=IQ(LOCL+4)          
        IGAP1=IWRE1-IWRB1 
        IGAP(ILINK)=IGAP1
        IWRB(ILINK)=IWRB1
C
C  Find all potential branches for link ILINK. 
C  Check if slopes agree. If they do, the links make an elementary tree.
C
        LUSER=LQ(LHEAD-IZUSER) 
        LLINK=LQ(LUSER-1)  
        NLINK2=NZFIND(0,LLINK,IE1,1)   
        IF (NLINK2.EQ.0) GO TO 200
        DO 300 LINK2=1,NLINK2           
          LOC2=IQUEST(LINK2)
          IWRB2=IQ(LOC2+3)              
          IWRE2=IQ(LOC2+4)              
          IGAP2=IWRE2-IWRB2 
          IF (IGAP1.EQ.IPASS.OR.IGAP2.EQ.IPASS) THEN  
              SL2=Q(LOC2+5)
              DSL=ABS(SL1-SL2)
              IF (DSL.LT.DSLMAX(IPASS)) THEN
                NNEXT(ILINK)=NNEXT(ILINK)+1
                INEXT(NNEXT(ILINK),ILINK)=IQ(LOC2-5)
                IF (NNEXT(ILINK).EQ.MXNEXT) GO TO 200
              END IF
          END IF
  300   CONTINUE      
  200 CONTINUE
C                                                   
C  Build chains
C  Chains are stored in Zebra bank 'CHAI'.
C
      DO 400 ILINK=1,NLINK
        IF (NNEXT(ILINK).EQ.0) GO TO 400
        IF (ILINK.GT.1.AND.IWRB(ILINK).EQ.IPASS.AND.IGAP(ILINK).EQ.
     &    IPASS) THEN 
          GO TO 500
        END IF
        IF ((IPASS.EQ.1.AND.ILINK.EQ.1).OR.IWRB(ILINK).LT.IPASS)  THEN
          CALL VCLIMB(ILINK,NNEXT,INEXT,IPASS) 
        END IF
        LUSER=LQ(LHEAD-IZUSER)
        LCHAI=LQ(LUSER-2)
        NCHAIN=IQ(LCHAI+1)
        IF (NCHAIN.GE.MAXCHN) GO TO 700   ! don't make more chains
  400 CONTINUE
  500 CONTINUE
C    
  700 CONTINUE
C
C  Fit chains; store track segments  
C
      DO 600 ICHAIN=1,NCHAIN
        NHIT=IQ(LCHAI+1+8*(ICHAIN-1)+1)  ! number of hits on chain
        IF (NHIT.EQ.9-IPASS.OR.(NHIT.EQ.7.AND.IPASS.EQ.1).OR.
     &    NHIT.EQ.NACTIV) CALL VFITCH(LAYER,SECTOR,ICHAIN,NHIT)
        LUSER=LQ(LHEAD-IZUSER) 
        NHUSED=IQ(LUSER+2)
  600 CONTINUE
        NHUSED=IQ(LUSER+2)
        IF (NACTIV-NHUSED.LT.8-INEFF) GO TO 1000
C                                        
  100 CONTINUE
 1000 CONTINUE
      RETURN
      END     
