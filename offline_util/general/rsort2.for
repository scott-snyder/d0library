      SUBROUTINE RSORT2(IKEY,RDAT,NW,NELE,IDIR,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sort RDAT(NW,NELE) for the real in field 
C-      IKEY. If IDIR>0, the sort is in increasing order.
C-
C-    Uses HEAPSORT.  See Knuth 'ART OF PROGRAMMING' or Press et.al,
C-       'NUMERICAL RECIPES'.  (NB: For an input array already in order,
C-        quicksort is O(N**2).)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IKEY,NW,NELE,IDIR,IERR
      INTEGER IORD,IRANK,ITMP,ILEN,ISTAT,I,J
      REAL    RDAT(NW,NELE)
      INTEGER LIB$GET_VM,LIB$FREE_VM
C
      ILEN = 4*MAX(NW,NELE)
      ISTAT = LIB$GET_VM(ILEN,IORD,0)
      ISTAT = LIB$GET_VM(ILEN,IRANK,0)
      ISTAT = LIB$GET_VM(ILEN,ITMP,0)
C
      IERR=0
      CALL RSORTER(NELE,NW,IKEY,RDAT,%VAL(IORD),%VAL(IRANK),IERR)
      CALL REORDER(NELE,NW,RDAT,%VAL(IORD),%VAL(IRANK),%VAL(ITMP),IERR)
C
      IF( IDIR.GT.0 ) GOTO 999
      DO I=1,NELE/2
         J=NELE+1-I
         CALL UCOPY(RDAT(1,I),%VAL(ITMP),NW)
         CALL UCOPY(RDAT(1,J),RDAT(1,I),NW)
         CALL UCOPY(%VAL(ITMP),RDAT(1,J),NW)
      ENDDO
C
 999  CONTINUE
C
      ISTAT = LIB$FREE_VM(ILEN,IORD,0)
      ISTAT = LIB$FREE_VM(ILEN,IRANK,0)
      ISTAT = LIB$FREE_VM(ILEN,ITMP,0)
C
      RETURN
      END
