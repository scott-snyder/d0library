      SUBROUTINE FIND_CLOSE_CLUSTER(P4,P4_CLUST,DIFF,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FINDS THE CLUSTER THAT IS CLOSEST IN 4 VECTOR
C-                         SPACE TO THE 4 VECTOR.
C-
C-   Inputs  : P4 = 4 VECTOR TO BE CLOSEST TO
C-   Outputs : P4_CLUST = 4 vector of cluster found.
C-             DIFF = SCALAR DIFFERENCE IN 4 VECTORS.
C-             LCACL WILL BE SET TO LINK OF CLOSEST CLUSTER
C-             IER = NON ZERO IF NO CLOSE CLUSTER FOUND
C-   Controls: 
C-
C-   Created   3-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER GZCACL,I,LCUR
      REAL    P4(*),P4_CLUST(*),P4_CUR(4)
      REAL    DIFF,DIFF_CUR
      REAL    MAX_DIFF
      LOGICAL FIRST
      INTEGER IER
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('MAXIMUM_CLUSTER_DIFF',MAX_DIFF,IER)
        CALL EZRSET
      ENDIF
      LCACL = GZCACL()
      IF ( LCACL.EQ.0 ) THEN
        CALL ERRMSG('HMATRIX','FIND_CLOSE_CLUSTER',
     &    'NO CACL BANKS ','W')
        IER = 1
        RETURN
      ENDIF
      DIFF = 999999.
      DO WHILE ( LCACL.NE.0 ) 
        CALL UCOPY(Q(LCACL+4),P4_CUR,4)
        CALL FIND_VEC_DIFF(P4,P4_CUR,4,DIFF_CUR)
        IF ( DIFF_CUR.LT.DIFF ) THEN
          DIFF= DIFF_CUR
          LCUR = LCACL
          CALL UCOPY(P4_CUR,P4_CLUST,4)
        ENDIF
        LCACL = LQ(LCACL)               ! NEXT CLUSTER
      ENDDO
      IF ( DIFF.LE.MAX_DIFF ) THEN
        LCACL = LCUR
        IER = 0
      ELSE
        CALL ERRMSG('HMATRIX','FIND_CLOSE_CLUSTER',
     &    'NO CLOSE CLUSTER FOUND ','W')
        IER = 1
      ENDIF
  999 RETURN
      END
