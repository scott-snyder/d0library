      SUBROUTINE SRTINT(IX,NCH,IMAP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sorts a integer array IX into ascending order.
C-                         The array IMAP contains the
C-                         ordered list of pointers
C-
C-   Inputs  : IX -   Integer array to be sorted
C-             NCH  - Number of elements in IX
C-             IMAP - On input IMAP contains the pointers to
C-             the elements in IX. i.e. in an unsorted array
C-             IMAP(1)= 1, IMAP(2) = 2 etc.
C-   Outputs : IMAP -  On output, IMAP will contain the 
C-             permutation of the original order that yields the sorted order. 
C-             IMAP(1) will be the pointer to the original array that contains 
C-             the 1st element of the sorted array.
C-             IX - contains the sorted integer values
C-   Controls: None
C-
C-   Created   3-OCT-1988   Rajendran Raja
C-   Based on the Algorithm of D.L.Shell, High speed sorting
C-   procedure , Communications of the ACM, Vol 2, July 1959, PP 30-32
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IX(*)
      INTEGER TEMP
      INTEGER IMAP(*),NCH,M,I,J,K,IM,IT
C----------------------------------------------------------------------
      M=NCH
   10 M=M/2       !binary chop
      IF(M.EQ.0)GO TO 999
      K=NCH-M
      J=1
   20 I=J
   30 IM=I+M
      IF(IX(I).LE.IX(IM))GO TO 40
      TEMP = IX(I)
      IX(I) = IX(IM)
      IX(IM) = TEMP
      IT = IMAP(I)
      IMAP(I)=IMAP(IM)
      IMAP(IM)=IT
      I = I-M
      IF(I.GE.1)GO TO 30
   40 J=J+1
      IF(J.GT.K)GO TO 10
      GO TO 20
  999 RETURN
      END
