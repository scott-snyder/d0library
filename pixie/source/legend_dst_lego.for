       SUBROUTINE LEGEND_DST_LEGO(NTYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGEND for DST LEGO plot
C-
C-   Inputs  : Number of particles of each type
C-   Outputs : None
C-
C-   Created  16-JUN-1992 Sharon Hagopian
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*3 COLORS(6),COLLAB(6)
      CHARACTER*15 LABELS(6)
      CHARACTER*15 LABARROW,LABB,LABTAU,LABMUOT
      CHARACTER*3 COLARROW
      CHARACTER*18 LABS(6)
      CHARACTER*5 IBLANK
      INTEGER NTYP(9),NPRINT
      INTEGER I,ICOLOR(4)
      INTEGER NLAB
      REAL RLEVEL
      DATA NLAB/6/
      DATA COLORS/  'GRE','MAG',  'RED'     ,   'CYA'    ,'RED', 'RED' /
      DATA LABELS/'MUON','MISS ET','ELEC','JET (HAD)'
     X ,'(EM)', 'PHOTONS'/
      DATA IBLANK/'     '/
      DATA COLARROW/'FOR'/
      DATA LABARROW/' CD TRAKS'/
      DATA LABTAU/' TAU'/
      DATA LABMUOT/' MU TK'/
C----------------------------------------------------------------------
C ****  Drawing the legend
C
      NPRINT=0
      CALL JIQDIL(RLEVEL)
      IF(RLEVEL .EQ. -2.) THEN
        DO I=1,NLAB
          CALL PXCOLCTOI(COLORS(I),ICOLOR(I))
        ENDDO
        CALL LEGEND3D(ICOLOR,LABELS,NLAB)
      ELSE
        DO 10 I=1,NLAB                                             !!
        IF(NTYP(I+1).EQ.0)GO TO 10
        NPRINT=NPRINT+1
        COLLAB(NPRINT)=COLORS(I)
        IF(I.EQ.5)THEN
          WRITE(LABS(NPRINT),301)IBLANK,LABELS(I)(1:4)
        ELSE
          WRITE(LABS(NPRINT),300) NTYP(I+1),LABELS(I)(1:10)
        ENDIF
   10 CONTINUE
        CALL LEGEND(COLLAB,LABS,NPRINT)
        IF(NTYP(8).GT.0)THEN
          WRITE(LABB,302) NTYP(8),LABTAU
          CALL LEGEND_TAU(COLARROW,LABB,NPRINT)
          NPRINT=NPRINT+1
        ENDIF
        IF(NTYP(1).GT.0)THEN
          WRITE(LABB,302) NTYP(1),LABARROW
          CALL LEGEND_ARROW(COLARROW,LABB,NPRINT)
          NPRINT=NPRINT+1
        ENDIF
        IF(NTYP(9).GT.0)THEN
          WRITE(LABB,302) NTYP(9),LABMUOT
          CALL LEGEND_M(COLARROW,LABB,NPRINT)
          NPRINT=NPRINT+1
        ENDIF
      ENDIF
  300 FORMAT(I3,A13)
  301 FORMAT(A5,3X,A4)
  302 FORMAT(I3,A11)
C-
  999 RETURN
      END
