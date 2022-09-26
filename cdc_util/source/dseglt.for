      SUBROUTINE DSEGLT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine responsible for the nitty gritty
C-                         work in finding x-y segments in a particular
C-                         layer and sector using the Link & Tree
C-                         method.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   6-NOV-1989   joey thompson:  Based on VSEGLT
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET, EZERROR
C-                                                and SAVE statement
C-   Updated   9-MAR-1992   Qizhong Li-Demarteau  changed the call to
C-                                                DLINKS to DTLINK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'                             
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:DLTPAR.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'                             
      INTEGER LLINK,NLINK,NNEXT(MAXLNK),INEXT(MXNEXT,MAXLNK),LUSER 
      INTEGER IPASS,MXPASS,INEFF,ILINK,LCHAI,NACTIV,NHUSED,JLINK
      INTEGER NCHAIN,ICHAIN,NHIT
      INTEGER IGAP1,IGAP2,IWRB1,LOCL,IBRNCH,IGAP(MAXLNK),IWRB(MAXLNK)
      INTEGER IE1,IWRE1,IB2,IWRB2,IWRE2,LOC2,LINK2,ICALL
      INTEGER IER 
      REAL SL1,SL2,DSL,DSLMAX(4)
      LOGICAL EZERROR
      SAVE ICALL
      DATA ICALL/0/
C===========================================================================
C DSLMAX represents the
C    maximum slope difference between two links when trying to build elementary
C    trees.  This is an array, the higher numbered elements are for each
C    successive pass.  Therefore, DSLMAX(0) is small to pick out the best
C    matches. Worse matches are picked out in later passes.  The number of
C    passes is decided by the inefficiency allowed in the tracking.  (INEFF)
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DSEGLT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DSLMAX',DSLMAX,IER)
        CALL EZGET_i('INEFF',INEFF,IER)  !max number of missing hits in a segment
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
      CALL VZERO(INEXT,MAXLNK*MXNEXT)
      NACTIV=IQ(LUSER+3)                  ! number of 'active' hits
      NCHAIN=0
      DO 100 IPASS=1,MXPASS
        IF (IPASS.GT.1)   CALL DDELET     ! delete used links
        IF (NCHAIN.GT.MAXCHN) GO TO 700   ! don't make more chains
C
C  Build links with gap=IPASS
C
        CALL DTLINK(NACTIV,IPASS)  
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
        LOCL=LLINK + (ILINK-1)*6
        IF (IPASS.GT.1) THEN
          IF ((LOCL+6).EQ.1) THEN            ! used link
            IF (NNEXT(ILINK).GT.0) THEN
              DO 201 IBRNCH=1,NNEXT(ILINK)
                INEXT(IBRNCH,ILINK)=0        ! cancel its branches
  201         CONTINUE
              NNEXT(ILINK)=0         
            END IF
            GO TO 200                        ! skip used link 
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
        LLINK=LQ(LUSER-1)     !Now find all of the links with the first
        DO 300 LINK2=1,NLINK  ! hit matching the second of ILINK    
          LOC2=LLINK+((LINK2-1)*6)      
          IF (IQ(LOC2+6).EQ.0) THEN        !Check if link is used
            IF (IQ(LOC2+1) .EQ. IE1) THEN  !Check if hits match
              IWRB2=IQ(LOC2+3)              
              IWRE2=IQ(LOC2+4)              
              IGAP2=IWRE2-IWRB2 
              IF (IGAP1.EQ.IPASS.OR.IGAP2.EQ.IPASS) THEN  
                SL2=Q(LOC2+5)
                DSL=ABS(SL1-SL2)
                IF (DSL.LT.DSLMAX(IPASS)) THEN
                  NNEXT(ILINK)=NNEXT(ILINK)+1
                  INEXT(NNEXT(ILINK),ILINK)=LINK2
                  IF (NNEXT(ILINK).EQ.MXNEXT) GO TO 200
                END IF
              END IF
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
          CALL DCLIMB(ILINK,NNEXT,INEXT,IPASS) 
        END IF
  400 CONTINUE
  500 CONTINUE
C    
        LUSER=LQ(LHEAD-IZUSER)
        LCHAI=LQ(LUSER-2)
        NCHAIN=IQ(LCHAI+1)
  700 CONTINUE
C
C  Fit chains; store track segments  
C
      DO 600 ICHAIN=1,NCHAIN
        LCHAI=LQ(LUSER-2)
        NHIT=IQ(LCHAI+1+NCDCEL*(ICHAIN-1)+1)      ! number of hits on chain
        IF (NHIT.EQ.((NCDCEL+1)-IPASS).OR.(NHIT.EQ.(NCDCEL-1).AND.IPASS
     &    .EQ.1).OR.NHIT.EQ.NACTIV) CALL DFITCH(ICHAIN,NHIT)
        LUSER=LQ(LHEAD-IZUSER) 
        NHUSED=IQ(LUSER+2)
  600 CONTINUE
        NHUSED=IQ(LUSER+2)
        IF ((NACTIV-NHUSED).LT.(NCDCEL-INEFF)) GO TO 1000
C                                        
  100 CONTINUE
 1000 CONTINUE
  999 RETURN
      END     
