      SUBROUTINE FALL_LADDERS_2(HALF,NLADD,LADDRS)
C------------------------------------------------------------------------
C
C    Purpose and Methods : Build all possible 2 layer ladders using 
C     unused segments.
C
C    Input  : HALF                = FDC side
C    Output : NLADD               = number of ladders
C             LADDRS(0:2,ILADD)   = segments on ladder ILADD
C
C-   Created  13-JUN-1991   Susan K. Blessing
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD 
C-    arrays.  Don't need HALF information.
C-   Updated  29-AUG-1991   Robert E. Avery  Call ERRMSG if too many ladders. 
C-   Updated  28-JUN-1993   Susan K. Blessing  Remove IZUSER.LINK.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER LFSEG
      INTEGER HALF,LAYER
      INTEGER NLADD,LADDRS(0:2,MXTRAK)
      INTEGER ISEG,ISEG1,ISEG2
      INTEGER NSEG(0:2)
      INTEGER GZFSEG,NZBANK
      INTEGER LAD(0:2),SEG
C
      LOGICAL FCHEKL
      LOGICAL USEDSEG(MXTRAK,0:2)      ! TRUE IF A SEGMENT IS ALREADY
C                                       ! USED IN A LADDER
C
      DATA USEDSEG/MXTRAK*.FALSE.,MXTRAK*.FALSE.,MXTRAK*.FALSE./
C
C------------------------------------------------------------------------
C
C Get number of segments in each layer
C
      DO LAYER = 0, 2
C
        LFSEG = GZFSEG(HALF,LAYER)
C LFSEG=0 means no segments found
        IF (LFSEG.GT.0) THEN
          NSEG(LAYER) = NZBANK(IXCOM,LFSEG)
        ELSE
          NSEG(LAYER) = 0
        END IF
C
C Check if segments have been used
        LAD(0) = 0
        LAD(1) = 0
        LAD(2) = 0
        DO SEG = 1, NSEG(LAYER)
          LAD(LAYER) = SEG
          IF (FCHEKL(HALF,LAD)) USEDSEG(SEG,LAYER) = .TRUE.
        END DO
      END DO
C
C Inner theta and phi
C
      DO ISEG = 1,NSEG(0)
        IF (.NOT.USEDSEG(ISEG,0)) THEN
          DO ISEG2 = 1, NSEG(2)
            IF (.NOT.USEDSEG(ISEG2,2)) THEN
              NLADD = NLADD+1
              LADDRS(0,NLADD) = ISEG
              LADDRS(1,NLADD) = 0
              LADDRS(2,NLADD) = ISEG2
              IF (NLADD.GE.MXTRAK) GO TO 999
            END IF
          END DO
        END IF
      END DO
C
C Phi and outer theta
      DO ISEG1 = 1, NSEG(1)
        IF (.NOT.USEDSEG(ISEG1,1)) THEN
          DO ISEG2 = 1, NSEG(2)
            IF (.NOT.USEDSEG(ISEG2,2)) THEN
              NLADD = NLADD+1
              LADDRS(0,NLADD) = 0
              LADDRS(1,NLADD) = ISEG1
              LADDRS(2,NLADD) = ISEG2
              IF (NLADD.GE.MXTRAK) GO TO 999
            END IF
          END DO
        END IF
      END DO
C
C Inner theta and outer theta
      DO ISEG = 1, NSEG(0)
        IF (.NOT.USEDSEG(ISEG,0)) THEN
          DO ISEG1 = 1, NSEG(1)
            IF (.NOT.USEDSEG(ISEG1,1)) THEN
              NLADD = NLADD+1
              LADDRS(0,NLADD) = ISEG
              LADDRS(1,NLADD) = ISEG1
              LADDRS(2,NLADD) = 0
              IF (NLADD.GE.MXTRAK) GO TO 999
            END IF
          END DO
        END IF
      END DO
C
C---------------------------------------------------------------------
  999 CONTINUE
C
      IF ( NLADD.GE.MXTRAK ) THEN
        NLADD = MXTRAK
        CALL ERRMSG('FDC-Too-Many-Tracks','FLINSG',
     &    ' number of potential FDC tracks greater than MAXTRAK',
     &    'W')
      ENDIF
C
      DO LAYER = 0, 2
        DO SEG = 1, NSEG(LAYER)
          USEDSEG(SEG,LAYER) = .FALSE.
        END DO
      END DO
C
      RETURN
      END
