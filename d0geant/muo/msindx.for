      SUBROUTINE MSINDX(NHITS,HITS,NINDX,INDX)
C------------------------------------------------------------------
C-    This creates pointers (INDX) to hits (HITS) in each cell
C- in plane.   NINDX stores number of hits in each cell.
C-
C-  Input:
C-    NHITS       I   no. of hits in HITS(,).
C-    HITS(i,j)   F   hit information for j-th hit in one module.
C- 
C-  Output:
C-    NINDX(l,m)  I   number of hits in l-th cell in m-th plane.
C-    INDX(k,l,m) I   pointer to HITS for k-th hit in l-th cell
C-                    in m-th plane.    j in HITS(i,j) will be
C-                    obtaned by j=INDX(k,l,m).
C-
C-  S.Kunori   31-Mar-87
C-  S.Igarashi 12-Apr-91  Changed NHDIM 6 -> 9
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C                          
      INTEGER NHDIM,NHMAX
      PARAMETER (NHDIM=9)
      PARAMETER (NHMAX=40)
      REAL HITS(NHDIM,NHMAX)
      INTEGER NHITS,NINDX(24,4),INDX(NHMAX,24,4)
C  -- local variables...
      INTEGER I,J,K,IPLN,ICEL
C
********************************************
*     Big Branch to old version,  V1.      *
********************************************
      IF(SMUO(2).LT.1.5) THEN
         CALL MSINDX_V1(NHITS,HITS,NINDX,INDX)
         RETURN
      ENDIF
C  -- clear pointers...
      DO 100 I=1,4
      DO 102 J=1,24
         NINDX(J,I)=0
      DO 104 K=1,NHMAX
         INDX(K,J,I)=0
104   CONTINUE
102   CONTINUE
100   CONTINUE
C
C  loop over all hits in HITS.
C  ===========================
C
      IF(NHITS.LE.0) GO TO 900
      DO 200 I=1,NHITS       
C          -- get plane number...
         IPLN=NINT(HITS(2,I))
C          -- get cell number...
C             assuming cell no. starts with 0...
         ICEL=NINT(HITS(3,I))+1
C          -- count up number of hits...
         NINDX(ICEL,IPLN)=NINDX(ICEL,IPLN)+1
C          -- store address...
         INDX(NINDX(ICEL,IPLN),ICEL,IPLN)=I
200   CONTINUE 
C
900   CONTINUE
      RETURN
      END                     
