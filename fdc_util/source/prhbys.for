      SUBROUTINE PRHBYS( LUNDBG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints number of hits in sectors
C-
C-   Inputs  : none
C-   Outputs : print hits
C-   Controls:
C-
C-   Created  23-OCT-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER LKFHLF,LKFDUN,LKFTQD,LKSECT
      INTEGER NUM_HITS(0:35),IQUAR,IBEG,IEND,LUNDBG
      INTEGER HITS_PER_WIRE(0:35,0:15),WIRES_PER_SEG(0:35)
      INTEGER GZFHLF,GZFDUN,GZFTQD,GZFTSC,GZFTDA,GZFPDA,GZFPSC
C----------------------------------------------------------------------
      IF( LUNDBG .LE. 0 ) GOTO 999
      DO 10 HALF=0,1
        LKFHLF=GZFHLF(HALF)
        IF( LKFHLF .LE. 0 ) GOTO 10
        IF( IQ(LKFHLF+1) .LE. 0 ) GOTO 10
        UNIT=0
        LKFDUN=GZFDUN(HALF,UNIT)
        IF( LKFDUN .LE. 0 ) GOTO 20
        IF( IQ(LKFDUN+1) .LE. 0 ) GOTO 20
        WRITE(LUNDBG,900) HALF,UNIT
        DO 30 QUAD=0,7
          LKFTQD=GZFTQD(HALF,QUAD)
          IF( LKFTQD .LE. 0 ) GOTO 30
          IF( IQ(LKFTQD+1) .LE. 0 ) GOTO 30
          DO 40 SECTOR=0,5
            LKSECT = GZFTSC(HALF,QUAD,SECTOR)
            IF( LKSECT .GT. 0 ) THEN
              NUM_HITS(SECTOR) = IQ( LKSECT+1 )
            ELSE
              NUM_HITS(SECTOR) = 0
            ENDIF
            WIRES_PER_SEG(SECTOR) = 0
            DO 45 WIRE=0,7
              HITS_PER_WIRE(SECTOR,WIRE) =IQ(LKSECT+4+IQ(LKSECT+2)+WIRE)
              IF( HITS_PER_WIRE(SECTOR,WIRE) .GE. 1 ) THEN
                WIRES_PER_SEG(SECTOR) = WIRES_PER_SEG(SECTOR) + 1
              ENDIF
   45       CONTINUE
   40     CONTINUE
          WRITE(LUNDBG,100) QUAD,(NUM_HITS(SECTOR),
     &                            WIRES_PER_SEG(SECTOR),SECTOR=0,5)
  100     FORMAT(' QUAD',I3,'   HITS BY SECTOR 0-5 (HITS PER SEG) = '/
     &                                   6(3X,I3,'(',I3,' )'))
   30   CONTINUE
C
   20   CONTINUE
        UNIT = 1
        LKFDUN=GZFDUN(HALF,UNIT)
        IF( LKFDUN .LE. 0 ) GOTO 10
        IF( IQ(LKFDUN+1) .LE. 0 ) GOTO 10
        WRITE(LUNDBG,901) HALF
        DO 50 SECTOR=0,35
          LKSECT = GZFPSC(HALF,SECTOR)
          IF( LKSECT .GT. 0 ) THEN
            NUM_HITS(SECTOR) = IQ( LKSECT+1 )
          ELSE
            NUM_HITS(SECTOR) = 0
          ENDIF
          WIRES_PER_SEG(SECTOR) = 0
          DO 55 WIRE=0,15
            HITS_PER_WIRE(SECTOR,WIRE) = IQ(LKSECT+4+IQ(LKSECT+2)+WIRE)
            IF( HITS_PER_WIRE(SECTOR,WIRE) .GE. 1 ) THEN
              WIRES_PER_SEG(SECTOR) = WIRES_PER_SEG(SECTOR) + 1
            ENDIF
   55     CONTINUE
   50   CONTINUE
        DO 60 IQUAR=0,3
          IBEG=IQUAR*9
          IEND=IQUAR*9 + 8
          DO 70 SECTOR=IBEG,IEND
            IF(NUM_HITS(SECTOR).GT.0) GOTO 80
   70     CONTINUE
          GOTO 60
   80     CONTINUE
          WRITE(LUNDBG,200)IBEG,IEND,(NUM_HITS(SECTOR),
     &                       WIRES_PER_SEG(SECTOR),SECTOR=IBEG,IEND)
   60   CONTINUE
  200   FORMAT(' HITS BY SECTOR ',I3,'to',I3,' (HITS BY SEG) = '/
     &                                   9(3X,I3,'(',I3,' )'))
   10 CONTINUE
C
  900 FORMAT(' HITS LOCATED IN HALF',I3,'  IN THETA',I3)
  901 FORMAT(' HITS LOCATED IN HALF',I3,'  IN PHI')
C----------------------------------------------------------------------
  999 RETURN
      END
