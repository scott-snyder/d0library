C+
      SUBROUTINE SSWTRK (DIR, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for SAMUS tracks reconstruction.
C-
C-   Inputs  : DIR    -  1 -> North, 2 -> South
C-   Outputs :
C-             NTRK   -  number of tracks found
C-   Controls: none.
C-
C-             Based on Efimov's SAMTRK   
C-
C-   Created  20-JUN-1994   Joao de Mello Neto
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, KEYTRG, NTRACKS
      INTEGER N_PLANES
      PARAMETER (N_PLANES=3)
      INTEGER GZSAHS, GZSATN, GZSATS
      INTEGER LSAHS, LTRK, NTRA, NTRB,NTRK      
      INTEGER NHTMX, NTRG2,IT,ITRAK
      SAVE    NHTMX, NTRG2, FIRST
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  initializing
C
      LSAHS = GZSAHS()
      IF (LSAHS .LE. 0) GO TO 999
      IF (FIRST) THEN
        NHTMX = 100  ! max. # hits in plane
        NTRG2 = 10   ! max. # of L2 tracks (NTRMX<24)
        FIRST = .FALSE.
      END IF
      IF (DIR .EQ. 1) THEN
        LTRK = 1 + GZSATN()
      ELSE
        LTRK = 1 + GZSATS()
      END IF
      IQ(LTRK) = 0
      KEYTRG = -1
      NTRACKS = 0
C
C ****  full tracks reconstruction
C      (Obs:satgsw mark good hits in Samus, so SSWTRA has to be called first)
C
      CALL SSWTRA (DIR, NTRA)
      IF (NTRA .EQ. 0) GO TO 999
      CALL SSWTRB (DIR, NTRB)
      IF (NTRB .EQ. 0) GO TO 999
      CALL SATRLN (DIR, NTRACKS)
      CALL SATRMC (DIR,NTRK)
      IF (NTRK.GT.0) THEN
        DO IT = 1, NTRK
          CALL SSWFIL(DIR, IT, ITRAK)
          CALL MUIFW3(ITRAK)
        END DO
      ENDIF
      RETURN
C
  999 CONTINUE
      IQ(LTRK) = 0
      RETURN
      END
