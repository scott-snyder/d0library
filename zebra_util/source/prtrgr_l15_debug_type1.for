      SUBROUTINE PRTRGR_L15_DEBUG_TYPE1(LUN, SOURCE, NLWF, LDBG1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Frame Section.
C-
C-   Inputs  : LUN    (I)   The unit number to write the information to
C-             SOURCE (C)   DSP which generates entry (in hex, eg, A1 )
C-             NLWF   (I)   Number of additional longwords of this entry
C-             LDBG1  (I)   The array of Type 1 entry of DBG Sect. words
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-MAY-1994   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*8 PRTRGR_INT_TO_HEX
      CHARACTER*96 EM_ETLL(4), EM_ETLR(4), EM_ETHL(4), EM_ETHR(4)
      CHARACTER*96 TOT_ELL(4), TOT_ELR(4), TOT_EHL(4), TOT_EHR(4)
      EXTERNAL PRTRGR_INT_TO_HEX
      INTEGER  ISTAT
C
      INTEGER LUN, NLWF, LDBG1(NLWF)
      CHARACTER*8 NROW
      CHARACTER*2 SOURCE, V(4)
      INTEGER TRULEN
      EXTERNAL TRULEN
      INTEGER N, I, J, K, ID_ENT
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
      WRITE (LUN,*)
     &  'Trigger Tower EM energy & Total Energy for relative etas (hex)'
      WRITE (LUN,*)
     &  '=============================================================='
      WRITE (LUN,*)
      DO K = 1, 4
         EM_ETLL(K) = ' '
         EM_ETLR(K) = ' '
         TOT_ELL(K) = ' '
         TOT_ELR(K) = ' '
         EM_ETHL(K) = ' '
         EM_ETHR(K) = ' '
         TOT_EHL(K) = ' '
         TOT_EHR(K) = ' '
      ENDDO
  10  FORMAT(' ETA=', SP, I3, SS, '   EM Et', A)
  20  FORMAT(' PHI=', I2, ':', I2,  ' Tot E', A)
  30  FORMAT(' ', A3)
      DO I = 1, 4
         DO J = 1, 32
            ID_ENT = (I-1)*32 + J
            NROW = PRTRGR_INT_TO_HEX(LDBG1(ID_ENT))
            V(1)   = NROW(7:8)
            V(2)   = NROW(5:6)
            V(3)   = NROW(3:4)
            V(4)   = NROW(1:2)
            IF (I.EQ.1 .AND. J.LE.16) THEN
               DO K = 1, 4
                  WRITE (EM_ETLL(K)((J-1)*4+1:(J-1)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF (I.EQ.1 .AND. J.GT.16) THEN
               DO K = 1, 4
                  WRITE (EM_ETLR(K)((J-17)*4+1:(J-17)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF (I.EQ.2 .AND. J.LE.16) THEN
               DO K = 1, 4
                  WRITE (TOT_ELL(K)((J-1)*4+1:(J-1)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF (I.EQ.2 .AND. J.GT.16) THEN
               DO K = 1, 4
                  WRITE (TOT_ELR(K)((J-17)*4+1:(J-17)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF(I.EQ.3 .AND. J.LE.16) THEN
               DO K = 1, 4
                  WRITE (EM_ETHL(K)((J-1)*4+1:(J-1)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF (I.EQ.3 .AND. J.GT.16) THEN
               DO K = 1, 4
                  WRITE (EM_ETHR(K)((J-17)*4+1:(J-17)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF (I.EQ.4 .AND. J.LE.16) THEN
               DO K = 1, 4
                  WRITE (TOT_EHL(K)((J-1)*4+1:(J-1)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ELSEIF (I.EQ.4 .AND. J.GT.16) THEN
               DO K = 1, 4
                  WRITE (TOT_EHR(K)((J-17)*4+1:(J-17)*4+4),30,
     &                   IOSTAT=ISTAT) V(K)
               ENDDO
            ENDIF
         ENDDO
      ENDDO
C
      DO I = 1, 4
         IF (SOURCE .NE. 'A2') THEN
            WRITE(LUN, 10, IOSTAT=ISTAT) N+I-1,
     &            EM_ETLL(I)(1:TRULEN(EM_ETLL(I)))
            WRITE(LUN, 20, IOSTAT=ISTAT) 1, 16,
     &            TOT_ELL(I)(1:TRULEN(TOT_ELL(I)))
            WRITE(LUN, 10, IOSTAT=ISTAT) N+I-1,
     &            EM_ETLR(I)(1:TRULEN(EM_ETLR(I)))
            WRITE(LUN, 20, IOSTAT=ISTAT) 17, 32,
     &            TOT_ELR(I)(1:TRULEN(TOT_ELR(I)))
            WRITE(LUN,*)
         ENDIF
      ENDDO
      DO I = 1, 4
         IF (SOURCE .NE. 'C2') THEN
            IF (SOURCE .NE. 'B4') THEN
               WRITE(LUN, 10, IOSTAT=ISTAT) N+I+3,
     &              EM_ETHL(I)(1:TRULEN(EM_ETHL(I)))
               WRITE(LUN, 20, IOSTAT=ISTAT) 1, 16,
     &              TOT_EHL(I)(1:TRULEN(TOT_EHL(I)))
               WRITE(LUN, 10, IOSTAT=ISTAT) N+I+3,
     &              EM_ETHR(I)(1:TRULEN(EM_ETHR(I)))
               WRITE(LUN, 20, IOSTAT=ISTAT) 17, 32,
     &              TOT_EHR(I)(1:TRULEN(TOT_EHR(I)))
               WRITE(LUN,*)
            ELSE
               WRITE(LUN, 10, IOSTAT=ISTAT) N+I+4,
     &              EM_ETHL(I)(1:TRULEN(EM_ETHL(I)))
               WRITE(LUN, 20, IOSTAT=ISTAT) 1, 16,
     &              TOT_EHL(I)(1:TRULEN(TOT_EHL(I)))
               WRITE(LUN, 10, IOSTAT=ISTAT) N+I+4,
     &              EM_ETHR(I)(1:TRULEN(EM_ETHR(I)))
               WRITE(LUN, 20, IOSTAT=ISTAT) 17, 32,
     &              TOT_EHR(I)(1:TRULEN(TOT_EHR(I)))
               WRITE(LUN,*)
            ENDIF
         ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
