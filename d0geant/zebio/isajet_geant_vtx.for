      SUBROUTINE ISAJET_GEANT_VTX(LISV1,NVTX,FIRST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : STORES ISAJET VTX IN GEANT
C-
C-   Inputs  : LISV1 VTX LINK IN ISAJET (PROTECT IN CALLER)
C-   Outputs : NVTX GEANT VTX NUMBER
C-   Controls:
C-
C-   Created  28-SEP-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LISV1,NVTX
      REAL    UBUF(10)
      LOGICAL FIRST
C
C----------------------------------------------------------------------
      CALL GSVERT(Q(LISV1+7),0,0,UBUF,0,NVTX)
C
      IF ( FIRST ) THEN                 ! Save primary vertex
        PKINE(4)= Q(LISV1+7)
        PKINE(5)= Q(LISV1+8)
        PKINE(6)= Q(LISV1+9)
      ENDIF
      IQ(LISV1-5) = NVTX                 ! RENUMBER TO GEANT #'s
  999 RETURN
      END
