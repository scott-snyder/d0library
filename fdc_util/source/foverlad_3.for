      SUBROUTINE FOVERLAD_3(HALF,NLADD,LADDRS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make three layer ladders of segments in
C-    overlapping sectors.
C-    Uses looser overlap crtierion for cosmic rays (not from beam line).
C-
C-   Inputs  : HALF
C-   Outputs : NLADD    = Number of ladders formed
C-             LADDRS() = Array of ladders
C-
C-   Created   2-JAN-1991   Susan K. Blessing
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup PARAMS,RCP
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD
C-    arrays.  Don't need HALF information.
C-   Updated  29-AUG-1991   Robert E. Avery  Call ERRMSG if too many ladders. 
C-   Updated   9-MAY-1993   Robert E. Avery  Slightly simpler version.
C-                              (accidently rewritten from scratch!)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF
      INTEGER NLADD,LADDRS(0:2,MXTRAK)
C
      INTEGER I
      INTEGER DUM,ADUM(50,4)
      INTEGER IADD
      INTEGER IER
      INTEGER H,WIRE
      INTEGER LOC0,LOC1,LOC2
      INTEGER UNIT0,QDRT0,SCTR0
      INTEGER UNIT1,QDRT1,SCTR1
      INTEGER UNIT2,QDRT2,SCTR2
      INTEGER GZFSEG
C
      LOGICAL OVERLAP
      LOGICAL UBIT
C----------------------------------------------------------------------
C
C Loop over segments in inner layer
C
      LOC0 = GZFSEG(HALF,0)
      DO WHILE (LOC0.GT.0)
C
C Check if segment has been used
        IF (.NOT.BTEST(IQ(LOC0),IUSED)) THEN
C
C Get sector information for segment
          IADD = IQ(LOC0+2)
          CALL FCODER(IADD,H,UNIT0,QDRT0,SCTR0,WIRE,UBIT,1)
C
C Loop over segments in outer layer
          LOC1 = GZFSEG(HALF,1)
          DO WHILE (LOC1.GT.0)
C
C Check if outer layer segment has been used
            IF (.NOT.BTEST(IQ(LOC1),IUSED)) THEN
C
              IADD = IQ(LOC1+2)
              CALL FCODER(IADD,H,UNIT1,QDRT1,SCTR1,WIRE,UBIT,1)
C
C Check if two sectors overlap
              CALL FOVRLP_COSMIC( HALF,UNIT0,QDRT0,SCTR0,
     &                            HALF,UNIT1,QDRT1,SCTR1,OVERLAP)
C
              IF (OVERLAP) THEN
C
                LOC2 = GZFSEG(HALF,2)
                DO WHILE (LOC2.GT.0)
C
C Check if phi layer segment has been used
                  IF (.NOT.BTEST(IQ(LOC2),IUSED)) THEN
C
C Get sector information for segment
                    IADD = IQ(LOC2+2)
                    CALL FCODER(
     &                IADD,H,UNIT2,QDRT2,SCTR2,WIRE,UBIT,1)
C
C Check if two sectors overlap
                    CALL FOVRLP_COSMIC( 
     &                HALF,UNIT0,QDRT0,SCTR0,
     &                HALF,UNIT2,QDRT2,SCTR2,OVERLAP)
                    IF (OVERLAP) THEN
                      CALL FOVRLP_COSMIC( 
     &                  HALF,UNIT1,QDRT1,SCTR1,
     &                  HALF,UNIT2,QDRT2,SCTR2,OVERLAP)
C
                      IF (OVERLAP) THEN
                        NLADD = NLADD + 1
                        LADDRS(0,NLADD) = IQ(LOC0-5)
                        LADDRS(1,NLADD) = IQ(LOC1-5)
                        LADDRS(2,NLADD) = IQ(LOC2-5)
                        IF (NLADD.GE.MXTRAK) GO TO 999
                      END IF
                    END IF
C
                  END IF
                  LOC2 = LQ(LOC2)
                END DO
              END IF
            END IF
            LOC1 = LQ(LOC1)
          END DO
        END IF
        LOC0 = LQ(LOC0)
      END DO
C
C----------------------------------------------------------------------
  999 CONTINUE
C
      IF ( NLADD.GE.MXTRAK ) THEN
        NLADD = MXTRAK
        CALL ERRMSG('FDC-Too-Many-Ladders','FOVERLAD',
     &      ' number of potential FDC tracks greater than MXTRAK',
     &      'W')
      ENDIF
C
      RETURN
      END
