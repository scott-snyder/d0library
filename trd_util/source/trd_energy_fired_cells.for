      SUBROUTINE TRD_ENERGY_FIRED_CELLS
     &  (LTRDT,RW,IW,FIRED_CELLS,ENERGY_FIRED_CELLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : computes energies with fired cells only
C-
C-   Inputs  : RW       real(3,NWORD)        output of TRD_DST_COR.FOR
C-             IW       integer(3,NWORD)     output of TRD_DST_COR.FOR
C-             LTRDT    integer            link to TRDT
C-   Outputs :
C              ENERGY_FIRED_CELLS             real(5)          energies in MIP
C                       ENERGY_FIRED_CELLS(1) energy layer 1 fired cell
C                       ENERGY_FIRED_CELLS(2) energy layer 2 fired cell
C                       ENERGY_FIRED_CELLS(3) energy layer 3 fired cell
C                       ENERGY_FIRED_CELLS(4) total energy fired cells
C                       ENERGY_FIRED_CELLS(5) truncated energy fired cells
C                       FIRED_CELLS           integer(3) fired cells numbers
C                                             for layers 1,2,3 (in [1.256])
C-   Controls: none
C-
C-   Created  30-JUN-1993   Alain PLUQUET
C-   Updated  25-NOV-1993   Alain PLUQUET  changes call to DELTA_TRD_CELLS
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      REAL RW(3,NWORD),PHIT,PHI(3),POINT(3),ENERGY_FIRED_CELLS(5)
      INTEGER IW(3,NWORD),LTRDT,LTPRL,LAYER,NA,I,D
      INTEGER GEO_CELL,NEAREST_CELL,NEAREST_I,DMIN
      INTEGER DELTA_TRD_CELLS,FIRED_CELLS(3),LZTRK,LCACL,LZFIT
C      INCLUDE 'D0$INC:ZLINKC.INC'

      ENERGY_FIRED_CELLS(1)=RW(1,51)
      ENERGY_FIRED_CELLS(2)=RW(2,51)
      ENERGY_FIRED_CELLS(3)=RW(3,51)

      LZTRK=LQ(LTRDT-4)
      LCACL=LQ(LTRDT-5)
      IF (LZTRK.GT.0 .OR.LCACL.GT.0) THEN
        IF(LZTRK.NE.0)THEN
          LZFIT=LQ(LZTRK-1)
          IF (LZFIT.GT.0) THEN
            POINT(1)=Q(LZFIT+11)
            POINT(2)=Q(LZFIT+12)
            POINT(3)=Q(LZFIT+15)
            PHIT=Q(LZFIT+10)
          ELSE
            POINT(1)=Q(LCACL+14)
            POINT(2)=Q(LCACL+15)
            POINT(3)=Q(LCACL+16)
            PHIT=Q(LCACL+12)
          ENDIF
          CALL PHI_TRD(POINT,PHIT,PHI,FIRED_CELLS)
          DO LAYER=1,3
            LTPRL=LQ(LTRDT-LAYER)
            IF (LTPRL.GT.0) THEN
              NA=IW(LAYER,4)
              IF (NA.EQ.1) THEN
                ENERGY_FIRED_CELLS(LAYER)=RW(LAYER,51)
              ELSEIF (NA.GT.1) THEN
                GEO_CELL=FIRED_CELLS(LAYER)

                NEAREST_I=1
                NEAREST_CELL=IW(LAYER,51)
                DMIN=DELTA_TRD_CELLS(NEAREST_CELL,GEO_CELL,LAYER)
                DO I=2,NA
                  D=DELTA_TRD_CELLS(IW(LAYER,50+I),GEO_CELL,LAYER)
                  IF (D.LT.DMIN) THEN
                    DMIN=D
                    NEAREST_CELL=IW(LAYER,50+I)
                    NEAREST_I=I
                  ENDIF
                ENDDO
                ENERGY_FIRED_CELLS(LAYER)=RW(LAYER,50+NEAREST_I)
              ENDIF
            ENDIF
          ENDDO
        ELSE
          CALL ERRMSG
     &      (' TRD_ENERGY_FIRED_CELLS','TRD_ENERGY_FIRED_CELLS',
     &      'Bank ZFIT not found','W')
        ENDIF
      ELSE
        CALL ERRMSG
     &    (' TRD_ENERGY_FIRED_CELLS','TRD_ENERGY_FIRED_CELLS',
     &    'Bank ZTRK not found','W')
      ENDIF


      ENERGY_FIRED_CELLS(4)=
     &        ENERGY_FIRED_CELLS(1)+
     &        ENERGY_FIRED_CELLS(2)+
     &        ENERGY_FIRED_CELLS(3)
      ENERGY_FIRED_CELLS(5)=
     &        ENERGY_FIRED_CELLS(4)-
     &        MAX(ENERGY_FIRED_CELLS(1),
     &        ENERGY_FIRED_CELLS(2),
     &        ENERGY_FIRED_CELLS(3))

      END
