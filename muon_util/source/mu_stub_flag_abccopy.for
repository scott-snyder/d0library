      SUBROUTINE MU_STUB_FLAG_ABCCOPY(IPDT,JSTUB,NHITS,IHIT,COPY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-JUN-1993   Tom Diehl
C-   Modified  27-SEP-1995  Darien Wood - add protection against missing
C-                                        MUOT or MHTT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMHTT.LINK'

      LOGICAL COPY
      INTEGER JSTUB,NTRACKS,IPDT,IHIT(28,20,16),NHITS(28,20),I,J
      INTEGER LMUOT,GZMUOT,LMHTT,JHIT,NPTRACK,IPOINTER
C----------------------------------------------------------------------
      COPY = .FALSE.
      CALL GTMTRH(NTRACKS)
      DO I = 1,NTRACKS
        LMUOT = GZMUOT(I)
        IF(LMUOT.GT.0) THEN
          LMHTT=LQ(LMUOT-IZMHTT)
          IF(LMHTT.GT.0) THEN
            NPTRACK=IQ(LMUOT+1)
            DO JHIT=1,NPTRACK
              IPOINTER = IQ(LMHTT+2+5*(JHIT-1))
              DO J = 1,NHITS(IPDT,JSTUB)
                IF(IPOINTER.EQ.IHIT(IPDT,JSTUB,J)) THEN
                  COPY = .TRUE.
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDDO

  999 RETURN
      END
