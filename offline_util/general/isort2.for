      SUBROUTINE ISORT2(IKEY,IDAT,NW,NELE,IDIR,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sort IDAT(NW,NELE) into increasing order for
C-    the integer in field IKEY. If IDIR<=0, the sort is in decresing
C-    order
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
C-   Modified: 9-JAN-1994   John D. Hobbs - Use LIB$GET_VM and 
C-    LIB$FREE_VM prior to release to D0 library.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IKEY,NW,NELE,IDAT(NW,NELE),IERR,IDIR
      INTEGER IORD,IRANK,ITMP,ILEN,ISTAT,I,J
      INTEGER LIB$GET_VM,LIB$FREE_VM
C
      ILEN = 4*MAX(NW,NELE)
      ISTAT = LIB$GET_VM(ILEN,IORD,0)
      ISTAT = LIB$GET_VM(ILEN,IRANK,0)
      ISTAT = LIB$GET_VM(ILEN,ITMP,0)
C
      IERR=0
      CALL ISORTER(NELE,NW,IKEY,IDAT,%VAL(IORD),%VAL(IRANK),IERR)
      CALL REORDER(NELE,NW,IDAT,%VAL(IORD),%VAL(IRANK),%VAL(ITMP),IERR)
C
      IF( IDIR.GT.0 ) GOTO 999
      DO I=1,NELE/2
         J=NELE+1-I
         CALL UCOPY(IDAT(1,I),%VAL(ITMP),NW)
         CALL UCOPY(IDAT(1,J),IDAT(1,I),NW)
         CALL UCOPY(%VAL(ITMP),IDAT(1,J),NW)
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
