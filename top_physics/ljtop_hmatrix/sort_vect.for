      SUBROUTINE SORT_VECT(P,NELE,NITEM,EL_SORT,P_SORT,IMAP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sort a Vector P with N elements in
C-   ASCENDING OR DESCENDING  order according to the value of
C-   a specified element in it.
C-
C-   Inputs  : P(NELE,NITEM) = VECTOR of NELE elements.
C-             NELE = Number of elements that constitute P
C-             NITEM = Number of instances of P(E.g Nele = 4 and NITEM = 20
C-             for sorting 4 vectors of 20 jets in an event)
C-             IF NITEM IS Negative, descending Order SORT is done.
C-             EL_SORT = the elemnt in P on which to sort.
C-   Outputs : P_SORT = SORTED ARRAY.
C-             IMAP   = MAP OF THE SORTING PROCESS.
C-             I.E. P(NELE,IMAP(1)) = P_SORT(NELE,1)
C-   Controls:
C-
C-   Created   6-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NELE,NITEM,EL_SORT
      INTEGER IMAP(*)
      REAL    P(NELE,*),P_SORT(NELE,*)
      INTEGER I,J,K
      INTEGER ANITEM
      LOGICAL ASCENDING
C----------------------------------------------------------------------
C
C ****  NOW TO SORT ACCORDING TO ET FOR TIME BEING
C
      ASCENDING = .TRUE.
      IF(NITEM.LT.0)ASCENDING = .FALSE.
      ANITEM = IABS(NITEM)
      DO I = 1 , ANITEM
        IMAP(I) = I
        IF ( ASCENDING ) THEN
          P_SORT(I,1) = P(EL_SORT,I)      ! storage
        ELSE
          P_SORT(I,1) = -P(EL_SORT,I)      ! storage
        ENDIF
      ENDDO
C
      CALL SRTFLT(P_SORT,ANITEM,IMAP)
      DO I = 1 ,ANITEM
        J = IMAP(I)
        DO K = 1 , NELE
          P_SORT(K,I) = P(K,J)
        ENDDO
      ENDDO
C
  999 RETURN
      END
