      SUBROUTINE RSORTER(N,NF,IKEY,RDAT,IAORD,IRANK,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill IAORD with the indices into RDAT such
C-    RDAT(IAORD(I))<RDAT(IAORD(I+1).
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
      INTEGER N,NF,IAORD(N),IRANK(N),IERR,IKEY
      REAL    RDAT(NF,N),RHOLD
      INTEGER I,J,K,L,M
C----------------------------------------------------------------------
C
C  Setup...
C
      DO I=1,N
         IAORD(I)=I
      ENDDO
      IF( N.EQ.1 ) GOTO 999
C
C  Sanity check...
C
      IF( N.LE.2 ) THEN
         IF( RDAT(IKEY,1).GT.RDAT(IKEY,2) ) THEN
            IAORD(1)=2
            IAORD(2)=1
         ENDIF
         GOTO 999
      ENDIF
C
C  Heapsort...
C
      K=N/2+1
      L=N
 10   CONTINUE
         IF(K.GT.1) THEN
            K=K-1
            M=IAORD(K)
            RHOLD=RDAT(IKEY,M)
         ELSE
            M=IAORD(L)
            RHOLD=RDAT(IKEY,M)
            IAORD(L)=IAORD(1)
            L=L-1
            IF( L.EQ.1 ) THEN
               IAORD(1)=M
               GOTO 999
            ENDIF
         ENDIF
C
         I=K
         J=K+K
 20      CONTINUE
         IF( J.LE.L ) THEN
            IF( J.LT.L ) THEN
               IF( RDAT(IKEY,IAORD(J)).LT.RDAT(IKEY,IAORD(J+1)) ) J=J+1
            ENDIF
            IF( RHOLD.LT.RDAT(IKEY,IAORD(J)) ) THEN
               IAORD(I)=IAORD(J)
               I=J
               J=J+J
            ELSE
               J=L+1
            ENDIF
         GOTO 20
         ENDIF
         IAORD(I)=M
      GOTO 10
C        
 999  CONTINUE
C
C  Fill rank array
C
      DO I=1,N
         IRANK(IAORD(I))=I
      ENDDO
C
      RETURN
      END
