      SUBROUTINE PRTRGR_L15_DEBUG_TYPE2(LUN, SOURCE, NLWF, LDBG2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Frame Section.
C-
C-   Inputs  : LUN    (I)   The unit number to write the information to
C-             SOURCE (C)   DSP which generates entry (in hex, eg, A1 )
C-             NLWF   (I)   Number of additional longwords for this entry
C-             LDBG2  (I)   The array of Type 2 entry of DBG Sect. words
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-MAY-1994   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*8 PRTRGR_INT_TO_HEX, NROW
      CHARACTER*2 SOURCE, V1
      CHARACTER*96 RSVL(4), RSVR(4)
      EXTERNAL PRTRGR_INT_TO_HEX
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      INTEGER LUN, NLWF, LDBG2(NLWF)
      INTEGER N, I, J, ID_ENT, ISTAT
C----------------------------------------------------------------------
C      CALL L15CALDBB_DATA_BLOCK_BUILDER()
      IF (SOURCE .EQ. 'A2') N = -24
      IF (SOURCE .EQ. 'A3') N = -20
      IF (SOURCE .EQ. 'A4') N = -16
      IF (SOURCE .EQ. 'A1') N = -12
      IF (SOURCE .EQ. 'B3') N = - 8
      IF (SOURCE .EQ. 'B4') N = - 4
      IF (SOURCE .EQ. 'B1') N =   1
      IF (SOURCE .EQ. 'C3') N =   5
      IF (SOURCE .EQ. 'C4') N =   9
      IF (SOURCE .EQ. 'C1') N =  13
      IF (SOURCE .EQ. 'C2') N =  17
  10  FORMAT(' PHI=', I2, ':', I2)
  20  FORMAT(' ETA=', SP, I3, SS, '    RSV', A)
  30  FORMAT(' ', A3)
  109 FORMAT(' ', A, T50, ': ', A)
      WRITE (LUN,*) '       Reference Set Value for relative etas (hex)'
      WRITE (LUN,*) '       ==========================================='
      WRITE (LUN,*)
      WRITE (LUN, 109) 'Term Number for which this Ref. Set applies',
     &                  PRTRGR_INT_TO_HEX(LDBG2(1))
      DO I = 1, 4
         RSVL(I) = ' '
         RSVR(I) = ' '
      ENDDO
      DO I = 1, 32
         DO J = 1, 4
            ID_ENT = (I-1)*4 + J + 1
            NROW   = PRTRGR_INT_TO_HEX(LDBG2(ID_ENT))
            V1     = NROW(7:8)
            IF (I .LE. 16) THEN
               WRITE(RSVL(J)((I-1)*4+1:(I-1)*4+4),30,IOSTAT=ISTAT)
     &               V1
            ELSE
               WRITE(RSVR(J)((I-17)*4+1:(I-17)*4+4),30,IOSTAT=ISTAT)
     &               V1
            ENDIF
         ENDDO
      ENDDO
      DO J = 1, 4
         IF (ABS(N+1+J) .LE. 20) THEN
            IF (SOURCE .EQ. 'B4') THEN
               IF ((N+1+J) .GE. 0) THEN
                  WRITE (LUN, 10, IOSTAT=ISTAT) 1, 16
                  WRITE (LUN, 20, IOSTAT=ISTAT) N+2+J,
     &                  RSVL(J)(1:TRULEN(RSVL(J)))
                  WRITE (LUN, 10, IOSTAT=ISTAT) 17, 32
                  WRITE (LUN, 20, IOSTAT=ISTAT) N+2+J,
     &                  RSVR(J)(1:TRULEN(RSVR(J)))
               ELSE
                  WRITE (LUN, 10, IOSTAT=ISTAT) 1, 16
                  WRITE (LUN, 20, IOSTAT=ISTAT) N+1+J,
     &                  RSVL(J)(1:TRULEN(RSVL(J)))
                  WRITE (LUN, 10, IOSTAT=ISTAT) 17, 32
                  WRITE (LUN, 20, IOSTAT=ISTAT) N+1+J,
     &                  RSVR(J)(1:TRULEN(RSVR(J)))
               ENDIF
            ELSE
               WRITE (LUN, 10, IOSTAT=ISTAT) 1, 16
               WRITE (LUN, 20, IOSTAT=ISTAT) N+1+J,
     &               RSVL(J)(1:TRULEN(RSVL(J)))
               WRITE (LUN, 10, IOSTAT=ISTAT) 17, 32
               WRITE (LUN, 20, IOSTAT=ISTAT) N+1+J,
     &               RSVR(J)(1:TRULEN(RSVR(J)))
            ENDIF
         ENDIF
      ENDDO
      WRITE (LUN,*)
C----------------------------------------------------------------------
  999 RETURN
      END
