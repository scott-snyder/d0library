      SUBROUTINE TOP_LEPTONS_NTUPLE_ELEFIN(ELE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find 3 highest Et e's and fill ELE
C-
C-   Inputs  : None
C-   Outputs : vector ELE (14 quantities associated with 
C-             3 highest Et electrons
C-   Controls: 
C-
C-   Created  16-SEP-1991   Jim Cochran
C-   modified  1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Modified 16-Mar-1993   name change of Good_Electron Function
C-                          and routine
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPELC,GZPELC,NELEC,I,J,PTTAG
      REAL ELE(0:3,14)
      LOGICAL TOP_LEPTONS_GOOD_ELECTRON
C----------------------------------------------------------------------
C
C
C       electrons
C
      LPELC=GZPELC()
      NELEC = 0
C
      IF (LPELC .EQ. 0) THEN            ! No PELC banks (-6)
        DO J=0,3
          DO I=1,14
            ELE(J,I) = -6.
          ENDDO
        ENDDO
C
      ELSEIF(LPELC.NE.0) THEN
C
C         loop through electron banks
C
        DO J=0,3
          DO I=1,14
            ELE(J,I)= -9.               ! diagnostic
          ENDDO
        ENDDO
C
        DO WHILE (LPELC.GT.0)           ! find 3 highest Et e's
          IF (TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
C
            ELE(0,1)=IQ(LPELC+2)
            DO I=2,9
              ELE(0,I)=Q(LPELC+I+1)
            ENDDO
            DO I=10,14
              ELE(0,I)=Q(LPELC+I+4)
            ENDDO
          ELSE                          ! if Q(PELC+7) not GT PTEC
C
            DO J=1,3
              DO I=1,14
                ELE(J,I) = -2.
              ENDDO
            ENDDO
          ENDIF                         ! end if Q(PELC+7) GT PTEC
C
          PTTAG=6
          CALL TOP_LEPTONS_NTUPLE_MAX3_14(ELE,PTTAG)
C
          LPELC=LQ(LPELC)                ! pointer to next electron
C
          NELEC=NELEC+1
          IF (LPELC .EQ. 0 .AND. NELEC .LT. 3) THEN     ! < 3 e's in PELC
            DO J=NELEC+1,3
              DO I=1,14
                ELE(J,I) = -3.          ! diagnostic
              ENDDO
            ENDDO
          ENDIF
C
        ENDDO                           ! do while LPELC LT 0
C
      ENDIF                             ! if LPELC NE 0
  999 RETURN
      END
