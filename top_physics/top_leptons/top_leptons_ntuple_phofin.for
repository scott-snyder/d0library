      SUBROUTINE TOP_LEPTONS_NTUPLE_PHOFIN(PHO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find three highest Et photons and 
C-                         construct PHO
C-                          
C-   Inputs  : None
C-   Outputs : photon vector PHO
C-   Controls: 
C-
C-   Created  16-SEP-1991   Jim Cochran
C-   Modified  1-SEP-1992   modified to be used in TOP_LEPTONS package
C-   Modified 15-Mar-1993   Name change in Good_Photon logical and 
C-                          routine name cahnged for libraray compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPPHO,GZPPHO,I,J,NPHOT,PTTAG
      REAL PHO(0:3,8)
      LOGICAL TOP_LEPTONS_GOOD_PHOTON
C----------------------------------------------------------------------
C
C       photons
C
      LPPHO=GZPPHO()
      NPHOT=0
C
      IF (LPPHO .EQ. 0) THEN            ! No PPHO banks (-6)
        DO J=0,3
          DO I=1,8
            PHO(J,I) = -6.
          ENDDO
        ENDDO
C
      ELSEIF(LPPHO.NE.0) THEN
C
        DO I=0,3
          DO J=1,8
            PHO(I,J) = -9.
          ENDDO
        ENDDO
      ENDIF
C
C         loop through photon banks and pick maximum
      DO WHILE (LPPHO.GT.0)           ! find 3 highest Et pho's 
        IF (TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
C
          DO I=1,8
            PHO(0,I)=Q(LPPHO+I+2)
          ENDDO
C
        ELSE
C
          DO J=1,3
            DO I=1,8
              PHO(J,I) = -2.
            ENDDO
          ENDDO
C
        ENDIF
C
        PTTAG = 5
        CALL TOP_LEPTONS_NTUPLE_MAX3(PHO,PTTAG)
C
        LPPHO=LQ(LPPHO)          ! pointer to next photon
C
        NPHOT=NPHOT+1
        IF (LPPHO .EQ. 0 .AND. NPHOT .LT. 3) THEN   ! < 3 pho's in PPHO
          DO J=NPHOT+1,3
            DO I=1,8
              PHO(J,I) = -3.
            ENDDO
          ENDDO
        ENDIF
C
      ENDDO
C
  999 RETURN
      END
