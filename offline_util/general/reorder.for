      SUBROUTINE REORDER(N,NREC,ILDATA,IORD,IRANK,ITMP,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Use IORD,IRANK to reshuffle the local events
C-    into increasing order...
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,NREC,ILDATA(NREC,N),IORD(N),IRANK(N),ITMP(NREC),IERR
      INTEGER I,J
C----------------------------------------------------------------------
C
      DO I=1,N
         J=IORD(I)
         CALL UCOPY(ILDATA(1,I),ITMP,NREC)
         CALL UCOPY(ILDATA(1,J),ILDATA(1,I),NREC)
         CALL UCOPY(ITMP,ILDATA(1,J),NREC)
         IORD(IRANK(I))=J
         IRANK(J)=IRANK(I)
         IORD(I)=I
         IRANK(I)=I
      ENDDO
C
 999  CONTINUE
      RETURN
      END
