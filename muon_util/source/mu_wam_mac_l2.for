C VAX/DEC CMS REPLACEMENT HISTORY, Element MU_WAM_MAC_L2.FOR
C *1    21-OCT-1993 08:54:43 FORTNER "add terms for scintillator"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MU_WAM_MAC_L2.FOR
      SUBROUTINE MU_WAM_MAC_L2(MODNO,MACHIT,MACHIT_OUT,NCLUSTERS)
C-------------------------------------------------------------------
C  FIND MUOH HITS THAT CREATE FINE CENTROIDS
C
C  INPUT : MODNO is Phil Martin number of module
C          MACHIT is array of pad latch hits in chamber using columns 2-25
C  OUTPUT : MACHIT_OUT = ARRAY OF MUOH HITS THAT FORMED CENTROIDS
C
C  CREATED 4/93 DF; FROM MU_WAM_MAC
C-------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MODNO,MACHIT(26,4),MACHIT_OUT(26,4)
      INTEGER I,J,NCLUSTERS
C<<
      DO I = 1,26
	DO J = 1,4
	  MACHIT_OUT(I,J) = 0
	END DO
      END DO
      NCLUSTERS = 0
C<<
C.. Get raw centroid positions
      DO I=1,24
        IF (MODNO.LE.37) CALL MU_WAM_CEN_CFA_L2(MACHIT,I,MACHIT_OUT,
     &      NCLUSTERS)
        IF (MODNO.GE.61.AND.MODNO.LE.97) CALL MU_WAM_CEN_EFA_L2(MACHIT,
     &      I,MACHIT_OUT,NCLUSTERS)
        IF (MODNO.GE.100.AND.MODNO.LT.200) CALL MU_WAM_CEN_B_L2(MACHIT,
     &      I,MACHIT_OUT,NCLUSTERS)
        IF (MODNO.GE.200.AND.MODNO.LT.400) CALL MU_WAM_CEN_C_L2(MACHIT,
     &      I,MACHIT_OUT,NCLUSTERS)
      ENDDO
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
