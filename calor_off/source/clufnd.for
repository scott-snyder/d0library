      FUNCTION CLUFND()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Find clusters and jets
C-
C-   Created  24-APR-1989   Nick Hadley
C-
C-   MODIFIED OCT-1989 NJ HADLEY
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CLUFND
C
      INTEGER GZCATE,GZCAPH
      INTEGER  NPRECE, NPRECH
      INTEGER MXPREC, NPRECL
      PARAMETER( MXPREC = 200 )
      REAL PRECLU(10,MXPREC)
      INTEGER IPRECL(10,MXPREC)
      EQUIVALENCE ( IPRECL(1,1), PRECLU(1,1) )
C
C
c     NPRECL = NUMBER OF PRECLUSTERS
C     MXPREC = MAXIMUM NUMBER OF PRECLUSTERS
C
C     PRECLU(1,I)  = ETA OF ITH PRECLUSTER CENTROID
C     PRECLU(2,I)  = PHI OF ITH PRECLUSTER CENTROID
C     PRECLU(3,I)  = EX OF ITH PRECLUSTER
C     PRECLU(4,I)  = EY OF ITH PRECLUSTER
C     PRECLU(5,I)  = EZ OF ITH PRECLUSTER
C     PRECLU(6,I)  = ETOT OF ITH PRECLUSTER
C     PRECLU(7,I)  = ET OF ITH PRECLUSTER
C     PRECLU(8,I)  = EM ET IN PRECLUSTER
C     PRECLU(9,I)  = BLANK FOR NOW
C     IPRECL(10,I)  = NUMBER OF TOWERS IN PRECLUSTER
C     IPRECL(11,I)  = INDEX OF FIRST TOWER, SEE BELOW FOR UNPACKING
C     IPRECL(12,I) = INDEX OF SECOND TOWER
C      AND SO ON FOR ALL THE TOWERS
C         ETA = INT(IPRECL(I,11)/64.) -(NETAL+1)
C         PHI = MOD(IPRECL(I,11),64)
C
C----------------------------------------------------------------------
      CLUFND=.FALSE.
      IF(GZCATE().NE.0) THEN
        CLUFND=.TRUE.
C        CALL CAPHFL                       ! book and fill CAPH
      CALL CLUPRE(MXPREC,NPRECL,PRECLU,IPRECL)
      CALL CONCLU(MXPREC,NPRECL,PRECLU,IPRECL)
C        CALL CLUPRE(MXPREC,NPRECL,PRECLU) ! find pre-clusters
C        CALL CONCLU(MXPREC,NPRECL,PRECLU) ! find jets
      ELSE
        CALL ERRMSG(' CATE bank is not filled','CLUFND',
     &    ' Not enough information to find clusters. Event skipped',
     &    'W')
      ENDIF
C test
  999 RETURN
      END
