      SUBROUTINE EGETMX (SEGN, MX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the transformation matrix from E&S.
C-
C-   Inputs  : SEGN        Segment number
C-   Outputs : MX(1:69)    4x4 transformation Matrices as a linear array
C-   Controls: NONE
C-
C-   Created   30-MAY-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SEGN
      REAL MX(96)
      INCLUDE 'D0$INC:DEVSTS.INC'
      INCLUDE 'D0$INC:SEGINF.INC'
      INCLUDE 'D0$INC:NEWDI3.INC'
C
      INTEGER NI, NJ
      PARAMETER (NI=4, NJ=4)
      REAL MX1(16), MX2(16)
      REAL AMX1(NI,NJ),AMX2(NI,NJ)
      REAL AMX1I(NI,NJ), AMX2I(NI,NJ), AMX3I(NI,NJ)
      REAL TAMX1(NI,NJ), TAMX2(NI,NJ)
      CHARACTER*40 HDR(6)
      CHARACTER*4  SEGNM, SSTR
      CHARACTER*80 STRING
      CHARACTER*80 COL1
      CHARACTER*80 COL2
      CHARACTER*80 COL3
      CHARACTER*80 COL4
      CHARACTER*7 CHNM
      CHARACTER*3 ASEG
      INTEGER STRLEN, LEN1, LEN2, LEN3, LEN4, I, J, K, NER
      EXTERNAL ERRHND
      LOGICAL MPRI, FIRST
      INTEGER NAS, NAMSEG
      DATA MPRI, FIRST /.FALSE., .TRUE./
C      DATA MPRI, FIRST /.TRUE., .TRUE./
C
      NAMSEG = SEGINF(6,SEGN)
      CALL KBLDN(NAMSEG, SSTR)
      SEGNM = 'R' // SSTR(1:3)
      
C
      IF( FIRST .OR. LASTCO ) THEN
        FIRST = .FALSE.
        CALL PFN('FXFORMDATA', 'XFORMDATA', ERRHND)
        CALL PFN('FLIST', 'LIST', ERRHND)
        CALL PFN('FPRINT', 'PRINT', ERRHND)
      ENDIF
C
        CALL PCONN('FXFORMDATA', 1, 1, 'FLIST', ERRHND)
        CALL PCONN('FLIST', 1, 1, 'FPRINT', ERRHND)
        CALL PCONN('FPRINT', 1, 1, 'HOST_MESSAGE', ERRHND)
C
      CHNM = 'MXFORM1'
C
  100 CONTINUE
      NER = 0
      CALL PSNST(SEGNM//'.'//CHNM, 2, 'FXFORMDATA', ERRHND)
C
CC      CALL PPURGE(ERRHND)
C
      CALL PSNBOO(.TRUE., 1, 'FXFORMDATA', ERRHND)
C
      CALL PGETW(STRING, STRLEN, ERRHND)
      CALL PGETW(COL1, LEN1, ERRHND)
      CALL PGETW(COL2, LEN2, ERRHND)
      CALL PGETW(COL3, LEN3, ERRHND)
      CALL PGETW(COL4, LEN4, ERRHND)
C
      READ (COL1(2:LEN1), *, ERR=9999) (MX2(I), I=1,4)
      READ (COL2(2:LEN2), *, ERR=9999) (MX2(I), I=5,8)
      READ (COL3(2:LEN3), *, ERR=9999) (MX2(I), I=9,12)
    2 CONTINUE
      READ (COL4(2:LEN4), *, ERR=1) (MX2(I), I=13,16)
C
      IF(CHNM(7:7) .EQ. '1') THEN
        DO I=1,16
          MX1(I) = MX2(I)
        ENDDO
        CHNM(7:7) = '2'
        GOTO 100
      ENDIF
C
      GOTO 999
C
 9999 CONTINUE
      CALL ERROR('ERROR DECODING INPUT: '//STRING(:STRLEN))
      GOTO 999
    1 CONTINUE
      IF(NER .GT. 0) GOTO 9999
      NER = NER + 1
      IF(COL4(LEN4:LEN4) .EQ. ';') THEN
        LEN4 = LEN4 - 1
        GOTO 2
      ENDIF
C
  999 CONTINUE
C
C --------------------------------------------------------------
C
      K = 0
      DO I=1,4
        DO J=1,4
          K = K + 1
          AMX1(I,J) = MX1(K)
          AMX2(I,J) = MX2(K)
          TAMX1(I,J) = MX1(K)
          TAMX2(I,J) = MX2(K)
        ENDDO
      ENDDO
C
C----  Solving [ view(TAMX1) * 3D = total(TAMX2) ]  --> 3D rot. matrix "TAMX2"
C
      CALL MXEQU(TAMX1, TAMX2, NI, NJ)
C
C----  Gettig inverse of rotations matrices.
C
      CALL EINVRS(AMX1,AMX1I)           ! inverse of view    AMX1I
      CALL EINVRS(TAMX2,AMX2I)          ! inverse of 3D      AMX2I
      CALL EINVRS(AMX2,AMX3I)           ! inverse of total   AMX3I
C
      K = 0
      DO I=1,4
        DO J=1,4
          K = K + 1
          MX(K)    = AMX1(I,J)          ! VIEW
          MX(16+K) = TAMX2(I,J)         ! 3D
          MX(32+K) = AMX2(I,J)          ! TOTAL
          MX(48+K) = AMX1I(I,J)         ! inverse VIEW
          MX(64+K) = AMX2I(I,J)         ! inverse 3D
          MX(80+K) = AMX3I(I,J)         ! inverse TOTAL
        ENDDO
      ENDDO
C
        CALL PDI('FXFORMDATA', 1, 1, 'FLIST', ERRHND)
        CALL PDI('FLIST', 1, 1, 'FPRINT', ERRHND)
        CALL PDI('FPRINT', 1, 1, 'HOST_MESSAGE', ERRHND)
C
C----------------------------
      IF(MPRI) THEN
        HDR(1) = ' VIEWING TRANSFORMATION'
        HDR(2) = ' 3D ROTATION MATRIX'
        HDR(3) = ' TOTAL MATRIX'
        HDR(4) = ' inverse of VIEWING TRANSFORMATION'
        HDR(5) = ' inverse of 3D ROTATION MATRIX'
        HDR(6) = ' inverse of TOTAL MATRIX'
        K = -16
        DO I=1,6
          K = K + 16
          PRINT '(A40)', HDR(I)
          PRINT *, MX(K+1),MX(K+2),MX(K+3),MX(K+4)
          PRINT *, MX(K+5),MX(K+6),MX(K+7),MX(K+8)
          PRINT *, MX(K+9),MX(K+10),MX(K+11),MX(K+12)
          PRINT *, MX(K+13),MX(K+14),MX(K+15),MX(K+16)
          PRINT *, '----------------------------------------------'
        ENDDO
      ENDIF
C----------------------------
C
      RETURN
      END
