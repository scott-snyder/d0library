      SUBROUTINE ECMERG(NELE,IAOLD,NOLD,IANEW,NNEW,IAOK,IKEY,
     + IAMERG,NTOT,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Merge two existing sorted records into a 
C-     third which contains no duplicates.  The sort key is passed in
C-     IKEY
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NELE,NOLD,NNEW,NTOT,IERR
      INTEGER IAOLD(NELE,*),IANEW(NELE,*),IAOK(*)
      INTEGER IAMERG(NELE,*),IKEY,I,J,K
C-----------------------------------------------------------------------
C
      IERR=0
      IF( NOLD.EQ.0 ) THEN
        CALL UCOPY(IANEW(1,1),IAMERG(1,1),NELE*NNEW)
        GOTO 999
      ENDIF
C
      I=1
      J=1
      K=1
      DO WHILE( I.LE.NOLD .OR. J.LE.NNEW )

        IF( K.GT.NTOT ) THEN
          IERR = -1
          GOTO 999
        ENDIF

        IF( I.GT.NOLD ) THEN
          CALL UCOPY(IANEW(1,J),IAMERG(1,K),NELE)
          J=J+1
        ELSEIF( J.GT.NNEW ) THEN
          CALL UCOPY(IAOLD(1,I),IAMERG(1,K),NELE)
          I=I+1
        ELSEIF( IAOLD(IKEY,I).LT.IANEW(IKEY,J) ) THEN
          CALL UCOPY(IAOLD(1,I),IAMERG(1,K),NELE)
          I=I+1
        ELSEIF( IAOLD(IKEY,I).GT.IANEW(IKEY,J) ) THEN
          CALL UCOPY(IANEW(1,J),IAMERG(1,K),NELE)
          J=J+1
        ELSEIF( IAOK(J).EQ.1 ) THEN
          CALL UCOPY(IANEW(1,J),IAMERG(1,K),NELE)
          J=MIN(J+1,NNEW+1)
          I=MIN(I+1,NOLD+1)
        ELSE
          IERR = -2
          GOTO 999
        ENDIF
        K=K+1
      ENDDO
C
  999 RETURN
      END
