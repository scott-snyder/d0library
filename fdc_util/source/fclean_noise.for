C VAX/DEC CMS REPLACEMENT HISTORY, Element FCLEAN_NOISE.FOR
C *1     4-NOV-1993 10:52:25 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FCLEAN_NOISE.FOR
      SUBROUTINE FCLEAN_NOISE(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop segment banks of track segments that
C-        are likely to be caused by noise. These are segments for 
C-        which:
C-              ABS(SLOPE) < SLOPE_CUT
C-         and  IONIZATION < ION_CUT
C-        Alos, adjust status words of hits for dropped segments.
C-
C-   Created   4-OCT-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IER
      INTEGER HALF,LAYER
      INTEGER UNIT 
      INTEGER SEG,NSEG
      INTEGER LFSEG,GZFSEG      
      INTEGER NHIT,HIT,STATUS
      INTEGER IBCLR, LZFIND, NZBANK
      INTEGER SEGNEW 
      INTEGER OFFSET(0:1)
      INTEGER MAX_SEGHIT
      PARAMETER( MAX_SEGHIT = 16 )
      INTEGER PTR_SC(MAX_SEGHIT),PTR_DA(MAX_SEGHIT)
C
      REAL    SLOPE,IONIZATION
      REAL    SLOPE_CUT(0:1),ION_CUT(0:1)
C
      LOGICAL FIRST
      LOGICAL CLEAN_NOISE  
      LOGICAL GOOD_SEG 
C
      DATA FIRST /.TRUE./
      DATA OFFSET/29,54/
      DATA SLOPE_CUT /0.05, 0.0 /
      DATA ION_CUT / 0.3, 0.0/
      DATA CLEAN_NOISE /.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('SLOPE_CUT',SLOPE_CUT ,IER)
        CALL EZGET('ION_CUT',ION_CUT ,IER)
        CALL EZGET('CLEAN_NOISE',CLEAN_NOISE ,IER)
        FIRST=.FALSE.
      END IF
      IF ( .NOT.CLEAN_NOISE  ) GOTO 999
C
      DO LAYER = 0, 2
        UNIT = LAYER/2
C
        SEGNEW = 0
        LFSEG = GZFSEG(HALF,LAYER)
        IF (LFSEG.GT.0) THEN
          NSEG = NZBANK(0,LFSEG)
          DO SEG = 1, NSEG
            SEGNEW = SEGNEW + 1 
            LFSEG = GZFSEG(HALF,LAYER)
            LFSEG = LZFIND(IXCOM,LFSEG,SEG,-5)
C
            SLOPE = Q(LFSEG+OFFSET(UNIT)+1)
            IONIZATION = Q(LFSEG+OFFSET(UNIT)+4)
            GOOD_SEG = ABS(SLOPE).GT.SLOPE_CUT(UNIT)
     &            .OR. IONIZATION.GT.ION_CUT(UNIT)

C
C Loop though all hits on segment, and change status word:
C
            CALL FGET_SEGHITS(HALF,LAYER,SEG,NHIT,PTR_SC,PTR_DA)
            DO HIT =  1, NHIT
C
              STATUS = IQ(PTR_SC(HIT)+9)
              IF ( GOOD_SEG ) THEN 
                CALL MVBITS(SEGNEW,0,8,STATUS,8)
              ELSE
                STATUS = IBCLR(STATUS,2)
                CALL MVBITS(0,0,8,STATUS,8)
              ENDIF
              IQ(PTR_SC(HIT)+9) = STATUS
C
            ENDDO
            IF ( .NOT.GOOD_SEG ) THEN
C Drop segment
              CALL MZDROP(IXCOM,LFSEG ,' ')
              SEGNEW = SEGNEW - 1 
C
            ENDIF
          END DO
        END IF
        IF ( SEGNEW.NE.NSEG ) THEN
          CALL MZGARB(IXMAIN,0)
        ELSE
          GOTO 100
        ENDIF
C
C Renumber the segments consequetively:
C
        SEG = 0
        LFSEG = GZFSEG(HALF,LAYER)
        DO WHILE (LFSEG.GT.0)
          SEG = SEG+1
          IQ(LFSEG-5) = SEG
          LFSEG = LQ(LFSEG)
        ENDDO
  100   CONTINUE
      END DO
C
  999 CONTINUE
      RETURN
      END
