      SUBROUTINE PRVERT(PRUNIT,LOC,IVER,FLAG,IFLAG)                 
C------------------------------------------------------------------
C 
C     Print out vertex banks 
C     IF FLAG='ALL'  all tracks
C     IF FLAG='ONE'  only IVER 
C 
C     Daria Zieminska  Feb. 1989
C-   Updated  24-SEP-1990   Qizhong Li-Demarteau  added CDC/FDC/VTX 
C-                                                in output 
C-   Updated  18-JAN-1992   Qizhong Li-Demarteau  added method in output
C                            
C----------------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER PRUNIT,NVERT,IVER,IVERT,IV,ICONT(10),LOC,IFLAG
      INTEGER KVERT(14),IBITS,PRIMARY,EXTRA,DECAY 
      INTEGER VERCDC1, VERCDC2, VERCDC3, VERFDC, VERVTX
      REAL CONT1(14),VERT(14)
      CHARACTER*3 FLAG
      EQUIVALENCE (KVERT,VERT)
C----------------------------------------------------------------------------
      IF (FLAG.EQ.'ONE') THEN
        CALL GTVERT(IVER,VERT) 
        PRIMARY =IBITS(KVERT(2),31,1)
        EXTRA   =IBITS(KVERT(2),30,1)
        DECAY   =IBITS(KVERT(2),29,1)
        VERVTX = IBITS(KVERT(2),28,1)
        VERCDC1 = IBITS(KVERT(2),27,1)
        VERFDC = IBITS(KVERT(2),26,1)
        VERCDC2 = IBITS(KVERT(2),25,1)
        VERCDC3 = IBITS(KVERT(2),24,1)
        WRITE(PRUNIT,101)
        WRITE (PRUNIT,102) IVER,PRIMARY,EXTRA,DECAY,
     &    VERCDC1, VERCDC2, VERCDC3, VERFDC, VERVTX,
     &  (VERT(IV),IV=3,8)
        GO TO 1000
      END IF
      CALL GTVERH(ICONT)
      NVERT=ICONT(2)+ICONT(3)
      IF (NVERT.LE.0) GO TO 1000
      IF (FLAG.EQ.'ALL') WRITE (PRUNIT,101) 
      DO 100 IVERT=1,NVERT
        CALL GTVERT(IVERT,VERT)
        PRIMARY =IBITS(KVERT(2),31,1)
        EXTRA   =IBITS(KVERT(2),30,1)
        DECAY   =IBITS(KVERT(2),29,1)
        VERVTX = IBITS(KVERT(2),28,1)
        VERCDC1 = IBITS(KVERT(2),27,1)
        VERFDC = IBITS(KVERT(2),26,1)
        VERCDC2 = IBITS(KVERT(2),25,1)
        VERCDC3 = IBITS(KVERT(2),24,1)
        WRITE (PRUNIT,102) IVERT,PRIMARY,EXTRA,DECAY,
     &    VERCDC1, VERCDC2, VERCDC3, VERFDC, VERVTX,
     &  (VERT(IV),IV=3,8)
  100 CONTINUE 
C
C   in the output, CDC(1)/CDC(2)/CDC(3)/FDC/VTX indicates the vertex is 
C   reconstructed by:
C   CDC(method=1) or CDC(method=2) or CDC(method=3) or FDC hits or VTX hits
C
  101 FORMAT(/,
     &  '  VERT  Prim/Extra/Decay  CDC(1)/CDC(2)/CDC(3)/FDC/VTX',
     &  '     X       Y       Z      ',
     x  'DX      DY      DZ')
  102 FORMAT(1X,I4,3I6,1X,3I7,1X,2I4,6F8.3) 
C
 1000 RETURN
      END       
