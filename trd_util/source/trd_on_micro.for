      SUBROUTINE TRD_ON_MICRO(LTRDT,INT_TRD,NINT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : ltrd: pointer to trdt bank
C-   Outputs : int_trd  integr words to be written on micro-dst
C-             nint: nb. of integer words
C-   Controls:
C-
C-   Created  12-JUL-1994   A. Zylberstejn
C-   Modified 29-APR-1995   L. T. Goss  Protect against integers larger than the
C-                                      bit-size available on mDST.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
C      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:TRD_DST_ENERGIES.INC'
      INCLUDE 'D0$INC:WORD_IN_TPRL.INC'
      INCLUDE 'D0$INC:zebcom.INC'
      INTEGER INT(4),JBYT,LAYER,LTRDT,MOT1,MOT2,TMIN
      INTEGER NINT,INT_TRD(10)
C----------------------------------------------------------------------
      NINT=0
      DO LAYER=1,3
        NINT=NINT+1
        INT_TRD(NINT)=0
        IF(LQ(LTRDT-LAYER).NE.0)THEN  ! Check that there is info in the layer
          INT_TRD(NINT)=ENERGY_FIRED_CELLS(LAYER)*10.
          IF (INT_TRD(NINT).LE.1023) THEN
            INT_TRD(NINT)=MIN0(INT_TRD(NINT),1023)
          ELSE
            INT_TRD(NINT)=MIN0(1023,1023)
          ENDIF
C
          TMIN=R_IN_TPRL(LAYER,45)+.5
          IF (TMIN.LE.63) THEN
            CALL SBYT(TMIN,
     &        INT_TRD(NINT),10,6)!pack  minimum drift time
          ELSE
            CALL SBYT(63,INT_TRD(NINT),10,6)
          ENDIF
C
C  debugging
C          mot1=jbyt(INT_TRD(NINT),1,9)
C          mot2=jbyt(INT_TRD(NINT),10,6)
C          if(mot2.ne.tmin)then
C          print*,' layer',layer,'ENERGY_FIRED_CELLS',
C     &      ENERGY_FIRED_CELLS(LAYER),' after decoding',
C     &      float(mot1)*.1,'min time',R_IN_TPRL(LAYER,45),
C     &      ' after decoding',mot2
        END IF
C        END IF
      END DO
      DO LAYER=1,3
        NINT=NINT+1
        INT_TRD(NINT)=0
        IF(LQ(LTRDT-LAYER).NE.0)THEN  ! Check that there is info in the layer
          IF (I_IN_TPRL(LAYER,3).LE.15) THEN
            INT_TRD(NINT)=I_IN_TPRL(LAYER,3)!local density (4 bits)
          ELSE
            INT_TRD(NINT)=15
          ENDIF
C
          IF (I_IN_TPRL(LAYER,4).LE.15) THEN
            CALL SBYT(I_IN_TPRL(LAYER,4),
     &          INT_TRD(NINT),5,4)!pack  nb. of hit anodes
          ELSE
            CALL SBYT(15,INT_TRD(NINT),5,4)
          ENDIF
C
          IF (I_IN_TPRL(LAYER,5).LE.15) THEN
            CALL SBYT(I_IN_TPRL(LAYER,5),
     &          INT_TRD(NINT),9,4)!pack  nb. of hit cathodes
          ELSE
            CALL SBYT(15,INT_TRD(NINT),9,4)
          ENDIF
C
          IF (I_IN_TPRL(LAYER,6).LE.15) THEN
            CALL SBYT(I_IN_TPRL(LAYER,6),
     &          INT_TRD(NINT),13,4)!pack  nb. of clusters
          ELSE
            CALL SBYT(15,INT_TRD(NINT),13,4)
          ENDIF
C
          MOT1=JBYT(INT_TRD(NINT),1,4)
          MOT2=JBYT(INT_TRD(NINT),5,4)
C   debugging
C          print*,' layer',layer,' nint',nint
C          print*,' layer',layer,'local density',I_IN_TPRL(LAYER,3)
C     &      ,' after decoding',
C     &      mot1,' nb. of hit anodes',I_IN_TPRL(LAYER,4),
C     &      mot2
C          mot1=jbyt(INT_TRD(NINT),9,4)
C          mot2=jbyt(INT_TRD(NINT),13,4)
C          print*,' layer',layer,' nb. of hit cathodes',
C     &      I_IN_TPRL(LAYER,5),' after decoding',
C     &      mot1,'  nb. of clusters',I_IN_TPRL(LAYER,6),
C     &      ' after decoding',mot2
        END IF
      END DO
  999 RETURN
      END
