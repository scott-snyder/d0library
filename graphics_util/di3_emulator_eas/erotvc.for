      SUBROUTINE EROTVC(PCOMM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transform NORML and UPVEC vectors
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-SEP-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GRFPAR.INC'
      INCLUDE 'D0$INC:SEGINF.INC'
C-
      INTEGER I,J,K,L
      REAL    RMATX(102)
      REAL    FORML(3),FPVEC(3),FWIND(4),FUPNT(3), SCAL,FACT
      CHARACTER*(*) PCOMM
      CHARACTER*80  OLD_PCOMM
      DATA OLD_PCOMM / ' ' /
C----------------------------------------------------------------------
C-
      CALL VZERO(RMATX,102)
      IF (PCOMM .NE. OLD_PCOMM) THEN
        OLD_PCOMM = PCOMM
        DO K=1,3
          FORML(K) = NORML(K)
          FPVEC(K) = UPVEC(K)
          FUPNT(K) = VUPNT(K)
        ENDDO
        DO K=1,4
          FWIND(K) = UWIND(K)
        ENDDO
      ENDIF
C-
C=== Update Normal Vector and 3D View Point here...
C=== Update Up Vector, too. ( Use ROT Dials )
C-
C--- GET_3D_MATRIX
C-
      CALL EGETMX(NSEGS, RMATX)
C-
C--- Get SCALE
C-
      SCAL = 0.
      SCAL = RMATX(65)**2 + RMATX(66)**2 + RMATX(67)**2
      SCAL = SQRT(SCAL)
      IF (SCAL .EQ. 0.) THEN
        FACT = 1.
      ELSE
        FACT = 1./SCAL
      ENDIF
C-
C--- TRANSFORM_3D_VECTOR(Normal Vector)
C-
      DO I=1,3
        RMATX(96+I) = FORML(I)
      ENDDO
C-
      CALL EMULMV(-1, 5, RMATX)
C-
      DO I=1,3
        NORML(I) = RMATX(99+I)*FACT
      ENDDO
C-
C--- TRANSFORM_3D_VECTOR(Up Vector)
C-
      DO J=1,3
        RMATX(96+J) = FPVEC(J)
      ENDDO
C-
      CALL EMULMV(-1, 5, RMATX)
C-
      DO J=1,3
        UPVEC(J) = RMATX(99+J)*FACT
      ENDDO
C-
C--- Update View Point by TRAN Dials
C-
      DO K=1,3
        VUPNT(K) = FUPNT(K) + RMATX(76+K)
      ENDDO
C-
C--- Update Window Size by SCAL Dials
C-
      DO K=1,4
        UWIND(K) = FWIND(K)*SCAL
      ENDDO
C-
      RETURN
C---
C----------------------------------------------------------------------
C---  ENTRY EROTVC_RESET
      ENTRY EROTVC_RESET
C-
      DO L=1,3
        NORML(L) = FORML(L)
        UPVEC(L) = FPVEC(L)
        VUPNT(L) = FUPNT(L)
      ENDDO
      DO L=1,4
        UWIND(L) = FWIND(L)
      ENDDO
C-
  999 RETURN
      END
