      SUBROUTINE TOP_LEPTONS_NTUPLE_MUOFIN(MUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find 3 highest Pt mu's and fill MUO
C-
C-   Inputs  : None
C-   Outputs : vector MUO
C-   Controls: 
C-
C-   Created  16-SEP-1991   Jim Cochran
C-   modified  1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Modified 15-Mar-1993   Good_Muon logical changed + routine name
C-                          changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL TOP_LEPTONS_GOOD_MUON
      INTEGER I,J,LPMUO,GZPMUO,NMUONS,PTTAG
      REAL MUO(0:3,14)
C----------------------------------------------------------------------
C
C       muons
C
      LPMUO=GZPMUO(0)
      NMUONS = 0
C
      IF (LPMUO .EQ. 0) THEN            ! No PMUO banks (-6)
        DO J=0,3
          DO I=1,14
            MUO(J,I) = -6.              ! diagnostic
          ENDDO
        ENDDO
C
      ELSEIF(LPMUO.NE.0) THEN
C
        DO I=0,3
          DO J=1,14
            MUO(I,J)= -9.                ! diagnostic
          ENDDO
        ENDDO
      ENDIF
C
      DO WHILE (LPMUO.GT.0)           ! find 3 highest Et mu's
C
        IF (TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
C
          MUO(0,1)=IQ(LPMUO+2)
          DO I=2,9
            MUO(0,I)=Q(LPMUO+I+8)
          ENDDO
          DO I=10,14
            MUO(0,I)=Q(LPMUO+I+18)
          ENDDO
C
        ELSE
C
          DO J=1,3
            DO I=1,14
              MUO(J,I) = -2.             ! diagnostic 
            ENDDO
          ENDDO
C
        ENDIF
C
        PTTAG=6
        CALL TOP_LEPTONS_NTUPLE_MAX3_14(MUO,PTTAG)
C
        LPMUO=LQ(LPMUO)                 ! pointer to next muon
C
        NMUONS=NMUONS+1
        IF (LPMUO .EQ. 0 .AND. NMUONS .LT. 3) THEN
          DO J=NMUONS+1,3
            DO I=1,14
              MUO(J,I) = -3.            ! diagnostic
            ENDDO
          ENDDO
        ENDIF
C
      ENDDO
C
  999 RETURN
      END
