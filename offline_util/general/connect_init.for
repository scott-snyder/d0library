      SUBROUTINE CONNECT_INIT (II,IREP,ICLASS,INEXT,ILO,IHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize words CLASS and NEXT in array
C-                         II(*) for cluster finding use the Youssef
C-                         nearest neighbor connection algorithm. The
C-                         array is assumed to made up of contiguous
C-                         blocks of IREP words, with a CLASS and NEXT
C-                         word per block. A block contains the data
C-                         pertaining to elements which are to be
C-                         clustered. If the array is part of a ZEBRA
C-                         bank then the address of the first word of the
C-                         bank should be supplied in the first argument.
C-                         For example, CALL CONNECT_INIT (IQ(LCATE+1),..).
C-
C-   Inputs  : II(*)    [I]     Array of elements to be clustered
C-             IREP     [I]     Number of words/element in array
C-             ICLASS   [I]     Offset to store class number
C-             INEXT    [I]     Offset to store pointer to next element
C-                              in cluster
C-             ILO      [I]     Number of first element
C-             IHI      [I]     Number of last element
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-JUL-1989   Rajendran Raja, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER II(*)
      INTEGER IREP
      INTEGER ICLASS
      INTEGER INEXT
      INTEGER ILO
      INTEGER IHI
C
      INTEGER I,J,K,M,N
      INTEGER BASE,CLASS_OFFSET,NEXT_OFFSET,REPETITION
      INTEGER CLASS,NEXT
C
      SAVE REPETITION,CLASS_OFFSET,NEXT_OFFSET
C----------------------------------------------------------------------
C
C ****  Define in-line functions
C
      CLASS(J) = (J-1)*REPETITION + CLASS_OFFSET
      NEXT(J)  = (J-1)*REPETITION + NEXT_OFFSET
C----------------------------------------------------------------------
C
      REPETITION   = IREP
      CLASS_OFFSET = ICLASS
      NEXT_OFFSET  = INEXT
C
      DO J = ILO, IHI
        II(CLASS(J)) = J
        II(NEXT(J))  = J
      ENDDO
      RETURN
C
      ENTRY CONNECT (II,M,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Connect elements N and M in array II(*).
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-OCT-1989   Rajendran Raja, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      I = M
      J = N
C
      IF ( II(CLASS(I)) .NE. II(CLASS(J)) ) THEN
C
C ****  Rewrite the class number of all elements connected to J
C ****  with the class number of element I
C
        K = J
        DO WHILE ( II(NEXT(K)) .NE. J )
          II(CLASS(K)) = II(CLASS(I))
          K = II(NEXT(K))
        ENDDO
C
C ****  Complete the connection
C
        II(CLASS(K))= II(CLASS(I))
        II(NEXT(K)) = II(NEXT(I))
        II(NEXT(I)) = J
C
      ENDIF
C
  999 RETURN
      END
