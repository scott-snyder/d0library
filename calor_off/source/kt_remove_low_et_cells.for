      SUBROUTINE KT_REMOVE_LOW_ET_CELLS (ier)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO REMOVE BOTH NEGATIVE ENERGY CELLS AND
C-                         LOW ET CELLS
C-
C-   Inputs  : None
C-   Outputs : ier          Error code, 0 is ok.
C-   Controls:
C-
C-   Created  11-JUL-1995   Brad Abbott
c-   Updated  24-oct-1995   Return error code (when no caeh bank is
c-                          present, for example).
c-                          Reset common block counters to zero if an
c-                          error occurs during the reading.
c-                          Gordon Watts
c-   Updated  07-Nov-1995   Use errmsg instead of type *
c-                          Gordon Watts
C-   Updated  10-Nov-1995   Energy is spread depending upon Et of cells
c-                          Brad Abbott
C-   Updated   5-Dec-1995   Bug fix in calculating em,ch and fh fractions
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NEG_CELLS,POS_CELLS,I,J,K,size
      REAL NEGATIVE(-46:46,0:65),POSITIVE(-46:46,0:65)
      REAL COUNTER,sum,ET_TO_ADD,ICD_TO_SUBTRACT,ICD_TO_ADD
      REAL em_to_subtract,em_to_add,FH_TO_ADD
      REAL ET_TO_SUBTRACT,ET_BEFORE_REMOVAL
      REAL ET_AFTER_REMOVAL,RATIO
      REAL NEG_ETA(2000),NEG_PHI(2000),FH_TO_SUBTRACT
      REAL POS_ETA(15000),POS_PHI(15000)
      REAL POS_ETA2(-46:46,0:65),POS_PHI2(-46:46,0:65)
      REAL EM(-46:46,0:65),CELL_COUNTER(-46:46,0:65)
      REAL ICD(-46:46,0:65),FH(-46:46,0:65)
      REAL CELL_EM(-46:46,0:65),CELL_ICD(-46:46,0:65)
      REAL CELL_FH(-46:46,0:65)
      INTEGER REMAINING_CELLS,ETA(300),PHI(300),ETAP,PHIP
      INTEGER GIAN_COUNTER,IETA,IPHI
      LOGICAL KEEP_GOING

      integer ier

c--------------------------------------------------------------------
      CALL KT_READ_CAEH(NEG_CELLS,NEG_ETA,NEG_PHI,NEGATIVE,
     &                  POS_CELLS,POS_ETA,POS_ETA2,POS_PHI,POS_PHI2,
     &                  POSITIVE,EM,ICD,FH,CELL_COUNTER,CELL_EM,
     &                  CELL_ICD,CELL_FH, ier)

      if (ier .ne. 0) then
         ngian = 0
         return
      endif

      IF(REMOVE_LOW_ET_CELLS) THEN
     
C..  DETERMINE ET BEFORE REMOVAL OF LOW ET CELLS

      ET_BEFORE_REMOVAL=0
      ET_AFTER_REMOVAL=0
      DELTA_ET=0

      DO I=1,NEG_CELLS
        IETA=INT(10*ABS(NEG_ETA(I))+0.5)
        IF(NEG_ETA(I).LT.0) IETA=-IETA
        IF(abs(ieta).GT.45) ieta=999
        IPHI=INT(10*NEG_PHI(I)+0.5)
        IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=999
        IF((ABS(IETA).LE.45).AND.(IPHI.GE.0).AND.(IPHI.LE.63))
     %  ET_BEFORE_REMOVAL=ET_BEFORE_REMOVAL+NEGATIVE(IETA,IPHI)
       ENDDO
      DO I=1,POS_CELLS
        IETA=INT(10*ABS(POS_ETA(I))+0.5)
        IF(POS_ETA(I).LT.0) IETA=-IETA
        IF(abs(ieta).GT.45) ieta=999
        IPHI=INT(10*POS_PHI(I)+0.5)
        IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=999
        IF((ABS(IETA).LE.45).AND.(IPHI.GE.0).AND.(IPHI.LE.63))
     %  ET_BEFORE_REMOVAL=ET_BEFORE_REMOVAL+POSITIVE(IETA,IPHI)
       ENDDO

C..  FIRST REMOVE ALL NEGATIVE ENERGY CELLS
C..  LOOP OVER VARYING SIZE square UNTIL THERE IS ENOUGH POSITIVE ENERGY
C..  TO REMOVE NEGATIVE ENERGY CELL.


      DO I=1,NEG_CELLS
        keep_going=.true.
        size=0
        DO WHILE(keep_going)
          COUNTER=0
          sum=0
          size=size+1
          DO J=-size,size
            IETA=INT(10*ABS(NEG_ETA(I))+0.5)
            IF(NEG_ETA(I).LT.0) IETA=-IETA
            IF(abs(ieta).GT.45) ieta=999
            IPHI=INT(10*NEG_PHI(I)+0.5)
            IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=999
            ETAP=IETA+J
            DO K=-size,size
              PHIP=IPHI+K
              IF(PHIP.LT.0) PHIP=64+PHIP
              IF(PHIP.GT.63) PHIP=PHIP-64
              IF((ABS(ETAP).LE.45).AND.(PHIP.GE.0).AND.(PHIP.LE.63))
     &          THEN
                IF(POSITIVE(ETAP,PHIP).GT.0) then
                  COUNTER=COUNTER+1
                  sum=sum+positive(etap,phip)
                  ETA(COUNTER)=ETAP
                  PHI(COUNTER)=PHIP
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          IF( (sum.GE.abs(negative(IETA,IPHI))).OR.
     & (SIZE.GE.SEARCH_SIZE))          keep_going=.false.
        ENDDO !WHILE

        DO J=1,COUNTER
          RATIO=POSITIVE(ETA(J),PHI(J))/SUM
          ET_TO_SUBTRACT=ABS(NEGATIVE(IETA,IPHI))*RATIO
          em_to_subtract=em(ieta,iphi)*RATIO
          ICD_TO_SUBTRACT=ICD(IETA,IPHI)*RATIO
          FH_TO_SUBTRACT=FH(IETA,IPHI)*RATIO
          IF(positive(eta(j),phi(j)).GT.0) THEN
            IF(POSITIVE(ETA(J),PHI(J)).GT.ET_TO_SUBTRACT) THEN
              POSITIVE(ETA(J),PHI(J))=
     &                POSITIVE(ETA(J),PHI(J))-ET_TO_SUBTRACT
              cell_counter(eta(j),phi(j))=
     &   cell_counter(eta(j),phi(j))+cell_counter(ieta,iphi)*RATIO
              cell_EM(eta(j),phi(j))=
     &          cell_EM(eta(j),phi(j))+cell_EM(ieta,iphi)*RATIO
              cell_ICD(eta(j),phi(j))=
     &          cell_ICD(eta(j),phi(j))+cell_ICD(ieta,iphi)*RATIO
              cell_FH(eta(j),phi(j))=
     &          cell_FH(eta(j),phi(j))+cell_FH(ieta,iphi)*RATIO
              em(eta(j),phi(j))=em(eta(j),phi(j))+em_to_subtract
              ICD(ETA(J),PHI(J))=ICD(ETA(J),PHI(J))+ICD_TO_SUBTRACT
              FH(ETA(J),PHI(J))=FH(ETA(J),PHI(J))+FH_TO_SUBTRACT
            ELSE
              POSITIVE(ETA(J),PHI(J))=0.000001
              EM(ETA(J),PHI(J))=0.0
              ICD(ETA(J),PHI(J))=0.0
              FH(ETA(J),PHI(J))=0.0
              cell_counter(eta(j),phi(j))=
     &   cell_counter(eta(j),phi(j))+cell_counter(ieta,iphi)*RATIO
              cell_EM(eta(j),phi(j))=
     &          cell_EM(eta(j),phi(j))+cell_EM(ieta,iphi)*RATIO
              cell_ICD(eta(j),phi(j))=
     &          cell_ICD(eta(j),phi(j))+cell_ICD(ieta,iphi)*RATIO
              cell_FH(eta(j),phi(j))=
     &          cell_FH(eta(j),phi(j))+cell_FH(ieta,iphi)*RATIO
            ENDIF
          ENDIF
        ENDDO
        NEGATIVE(IETA,IPHI)=0.0
        CELL_COUNTER(IETA,IPHI)=0
        CELL_EM(IETA,IPHI)=0
        CELL_FH(IETA,IPHI)=0
        CELL_ICD(IETA,IPHI)=0
        EM(IETA,IPHI)=0
        ICD(IETA,IPHI)=0
        FH(IETA,IPHI)=0
      ENDDO  !NEG_CELLS


C..   REMOVE LOW ET CELLS.
C..   DEMAND AT LEAST 4 CELLS HAVE ET AROUND LOW ENERGY CELL
C..   DO NOT WANT TO ADD TO ZERO ET CELLS SINCE THIS WILL INCREASE
C..   TOTAL NUMBER OF CELLS

  
      DO I=1,POS_CELLS
        IETA=INT(10*ABS(POS_ETA(I))+0.5)
        IF(POS_ETA(I).LT.0) IETA=-IETA
        IF(abs(ieta).GT.45) ieta=999
        IPHI=INT(10*POS_PHI(I)+0.5)
        IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=999
        IF((abs(ieta).LE.45).AND.(iphi.GE.0).AND.(iphi.LE.63)) THEN
          IF((POSITIVE(IETA,IPHI).GT.0)
     &      .AND.(POSITIVE(IETA,IPHI).LT.MINIMUM_ET)) THEN
            keep_going=.true.
            size=0
            DO WHILE(keep_going)
              COUNTER=0
              SUM=0
              size=size+1
              DO J=-size,size
                ETAP=IETA+J
                DO K=-size,size
                  PHIP=IPHI+K
                  IF(PHIP.LT.0) PHIP=64+PHIP
                  IF(PHIP.GT.63) PHIP=PHIP-64
                  IF((J.NE.0).AND.(K.NE.0)) THEN
                    IF((ABS(ETAP).LE.45).AND.(PHIP.GE.0).AND.
     &                (PHIP.LE.63)) THEN
                      IF(POSITIVE(ETAP,PHIP).GT.0) then
                        COUNTER=COUNTER+1
                        SUM=SUM+POSITIVE(ETAP,PHIP)
                        ETA(COUNTER)=ETAP
                        PHI(COUNTER)=PHIP
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
              IF( (COUNTER.GE.4).OR.(SIZE.GE.SEARCH_SIZE))
     &          keep_going=.false.  ! DEMAND AT LEAST 4 CELLS
            ENDDO !WHILE

            DO J=1,COUNTER
              RATIO=POSITIVE(ETA(J),PHI(J))/SUM
              ET_TO_ADD=POSITIVE(IETA,IPHI)*RATIO
              em_to_add=em(ieta,iphi)*RATIO
              ICD_TO_ADD=ICD(IETA,IPHI)*RATIO
              FH_TO_ADD=FH(IETA,IPHI)*RATIO
              POSITIVE(ETA(J),PHI(J))=
     &          POSITIVE(ETA(J),PHI(J))+ET_TO_ADD
              cell_counter(eta(j),phi(j))=cell_counter(eta(j),phi(j))+
     &          cell_counter(ieta,iphi)*RATIO
              cell_EM(eta(j),phi(j))=cell_EM(eta(j),phi(j))+
     &          cell_EM(ieta,iphi)*RATIO
              cell_ICD(eta(j),phi(j))=cell_ICD(eta(j),phi(j))+
     &          cell_ICD(ieta,iphi)*RATIO
              cell_FH(eta(j),phi(j))=cell_FH(eta(j),phi(j))+
     &          cell_FH(ieta,iphi)*RATIO
              em(eta(j),phi(j))=em(eta(j),phi(j))+em_to_add
              ICD(eta(j),phi(j))=ICD(eta(j),phi(j))+ICD_to_add
              FH(eta(j),phi(j))=FH(eta(j),phi(j))+FH_to_add
            ENDDO
            POSITIVE(IETA,IPHI)=0.0
          ENDIF
        ENDIF
      ENDDO  !POS_CELLS
      ENDIF

C.. DETERMINE NUMBER OF CELLS WITH ET
      REMAINING_CELLS=0
      GIAN_COUNTER=1
      DO I=-45,45
        DO J=0,63
          IF(POSITIVE(I,J).GT.0) THEN
            ET_AFTER_REMOVAL=ET_AFTER_REMOVAL+POSITIVE(I,J)
            REMAINING_CELLS=REMAINING_CELLS+1
            XGIAN(GIAN_COUNTER)=POS_ETA2(I,J) !ETA
            XGIAN(GIAN_COUNTER+1)=POS_PHI2(I,J) !PHI
            XGIAN(GIAN_COUNTER+2)=POSITIVE(I,J) !ET
            xgian(gian_counter+3)=cell_counter(i,j) !NUMBER OF CELLS
            xgian(gian_counter+4)=em(i,j)/positive(i,j) ! EMFRACTION
            XGIAN(GIAN_COUNTER+5)=ICD(I,J)/POSITIVE(I,J) ! ICD FRACTION
            XGIAN(GIAN_COUNTER+6)=FH(I,J)/POSITIVE(I,J) ! FH FRACTION
            XGIAN(GIAN_COUNTER+7)=CELL_EM(I,J) ! NUMBER OF EM CELLS
            XGIAN(GIAN_COUNTER+8)=CELL_ICD(I,J) ! NUMBER OF ICD CELLS
            XGIAN(GIAN_COUNTER+9)=CELL_FH(I,J) ! NUMBER OF FH CELLS
            GIAN_COUNTER=GIAN_COUNTER+gian_block_size
            IF (gian_counter+9 .gt. max_gian) then
               call errmsg ('too-many-cells', 'kt_remove_low_et_cells',
     $              '**** Too many cells!!! in xgian!', 'w')
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      NGIAN=REMAINING_CELLS
      DELTA_ET=ET_AFTER_REMOVAL-ET_BEFORE_REMOVAL

  999 RETURN
      END
