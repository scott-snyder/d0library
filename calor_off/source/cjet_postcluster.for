      SUBROUTINE CJET_POSTCLUSTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Merge pre-clusters (CACL banks) into larger
C-   clusters using Saul Youssef's connection algorithm. The connection
C-   algorithm works off the Cluster MAP bank CMAP which contains the
C-   CLASS, NEXT words.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-DEC-1989   Harrison B. Prosper
C-   Updated   3-JAN-1990   Boaz Klima   
C-   Updated  11-JAN-1990   Harrison B. Prosper  
C-      Now uses CMAP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NCLUSTERS,IER,PRE_CLUSTER_I,PRE_CLUSTER_J
      INTEGER GZCACL
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
      ENDIF
C
C ****  Check for CACL bank(s)
C
      LCACL = GZCACL()
      IF ( LCACL .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CJET_POSTCLUSTER',
     &    'NO CACL pre-cluster banks were found','W')
        GOTO 999
      ENDIF
C
C ****  Initialize CMAP bank
C
      CALL GTCACL_TOTAL (NCLUSTERS,IER)
      CALL BKCMAP (NCLUSTERS,LCMAP)
      CALL CMAPFL (LCACL)
C
C ****  Connect pre-clusters
C
      DO PRE_CLUSTER_I =  1,NCLUSTERS
C
C ****  Find pre-cluster j to connect with pre-cluster i
C
        CALL CJET_MERGE_PRECLUSTERS (PRE_CLUSTER_I,PRE_CLUSTER_J)
        IF ( PRE_CLUSTER_J .NE. 0 ) THEN
          CALL CONNECT (IQ(LCMAP+1),PRE_CLUSTER_J,PRE_CLUSTER_I)
        ENDIF
      ENDDO
  999 RETURN
      END
