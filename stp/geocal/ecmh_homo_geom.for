      SUBROUTINE ECMH_HOMO_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the ECMH module GEANT volumes.
C-
C-     The Homogenous module will consist of:
C-        1.   Mother (with mixture defined below)
C-        2.   FrontPlate (SS)
C-        3.   Floors 1-5 (with mixtures defined below)
C-        4.   Endplate (SS)
C-     First, calculate the relative areas of the 4 different
C-     materials used in the calorimeter: SS,Ur,LAr,G10.
C-     The following codes will be used to sum up materials:
C-        1    Stainless Steel
C-        2    Uranium
C-        3    Liquid Argon
C-        4    G10
C-     Next, create mixtures for each of the 5 floors.
C-     The following mixtures must be made:
C-        B+0  Mother
C-        B+1  Floor 1
C-        B+2  Floor 2,3,4
C-        B+3  Floor 5
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-MAR-1990   Norman A. Amos
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
      INCLUDE 'D0$INC:MATRIX.INC'
      INCLUDE 'D0$INC:IUSET.INC'
      INCLUDE 'D0$INC:ECMH_MODULE.INC'
      REAL XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS
      REAL CUTBACK,SCUTBACK,CCUTBACK,PI,CM_PER_IN
      REAL THICK,TOT_THICK(11),ANGLE
      REAL ECVOLUME,ECVOLUME1,ECVOLUME2
      REAL MTR_AREA,LAR_AREA,OUTSID_VOL(4),INSIDE_VOL(4,11)
      REAL OUTSID_TOT,INSIDE_TOT
      INTEGER MATERIAL_START_CODE,MAT_CODE,IMATER
      INTEGER ISTEP,NSTEPS,LAST_GAP_PER_STEP(11)
      INTEGER I,N,IER,IGAP,NGAPS,MFH_GAPMAX
      INTEGER IREADOUT,READOUT_GAP(5)
      CHARACTER SECT*5,VOLNAM*4,VOLMOT*4,GAP*2,STEP*1,MATER*1
      EQUIVALENCE(VOLNAM,VOLUME_NAME)
      EQUIVALENCE(VOLMOT,VOLUME_MOTHER)
      DATA TOT_THICK/11*0./
      DATA OUTSID_VOL/4*0./
      DATA INSIDE_VOL/44*0./
      DATA IREADOUT/1/
      DATA ISTEP/1/
      DATA YCURS/0./
      DATA IGAP/0/
C----------------------------------------------------------------------
C- Constants.
C----------------------------------------------------------------------
      CALL EZGET('PI',PI,IER)
      CALL EZGET('CM_PER_INCH',CM_PER_IN,IER)
      CALL EZGET('ECMH_NUMBER_STEPS',NSTEPS,IER)
      CALL EZGET('ECMH_NUMBER_GAPS',NGAPS,IER)
      CALL EZGET('ECMFH_NUMBER_GAPS',MFH_GAPMAX,IER)
      CALL EZGET('ECMH_READOUT_GAPS',READOUT_GAP,IER)
      CALL EZGET('RESISTCOAT_CUTBACK',CUTBACK,IER)
      CALL EZGET('ECMH_MODULE_ANGLE',ANGLE,IER)
      CCUTBACK=CUTBACK/COS(PI*ANGLE/180.)
      SCUTBACK=CUTBACK/SIN(PI*ANGLE/180.)
      CALL EZGET('ECMH_MATERIALS_CODE',MATERIAL_START_CODE,IER)
      CALL EZGET('ECMH_LAST_GAP_PER_STEP',LAST_GAP_PER_STEP,IER)
      CALL EZGET('ECMH_HOMO_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
C
C----------------------------------------------------------------------
C- Calculate the MOTHER Volumes.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_TOTAL_THICKNESS',THICK,IER)
      CALL EZGET('ECMH_MODULE_X_INC',XINC,IER)
      CALL EZGET('ECMH_MODULE_Y_INC',YINC,IER)
      CALL EZGET('ECMH_MODULE_X_OUC',XOUC,IER)
      CALL EZGET('ECMH_MODULE_Y_OUC',YOUC,IER)
      CALL EZGET('ECMH_MODULE_X_TOP',XTOP,IER)
      CALL EZGET('ECMH_MODULE_Y_TOP',YTOP,IER)
      CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,1,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME1)
      CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,2,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME2)
      MTR_AREA=(ECVOLUME1+ECVOLUME2)/THICK
C
C----------------------------------------------------------------------
C- Loop over all BOARDS and ABSORBERS in the ECMH.
C----------------------------------------------------------------------
      DO IGAP=1,NGAPS
        WRITE (GAP,'(I2.2)') IGAP
C-    Choose between MFH and MCH.
        IF (IGAP.GT.MFH_GAPMAX) then
          SECT='ECMCH'
        ELSE
          SECT='ECMFH'
        ENDIF
C-    Choose the proper Step.
        IF (IGAP.GT.LAST_GAP_PER_STEP(ISTEP)) THEN
          ISTEP=ISTEP+1
        ENDIF
C
C----------------------------------------------------------------------
C-    Calculate two ARGON GAP Volumes.
C-    An ARGON GAP has dimensions of the signal board
C-    MINUS the resist-coat cutback.
C----------------------------------------------------------------------
        MAT_CODE=3
        CALL EZGET('ECMH_ARGONGAP_THICKNESS',THICK,IER)
        CALL EZGET(SECT//'_BOARD_X_INC',XINC,IER)
        XINC=XINC-CCUTBACK
        CALL EZGET(SECT//'_BOARD_Y_INC',YINC,IER)
        YINC=YINC+CUTBACK
        CALL EZGET(SECT//'_BOARD_X_OUC',XOUC,IER)
        XOUC=XOUC-SCUTBACK
        CALL EZGET('ECMH_MODULE_Y_OUC',YOUC,IER)
        CALL EZGET(SECT//'_BOARD_X_TOP',XTOP,IER)
        CALL EZGET(SECT//'_BOARD_Y_TOP',YTOP,IER)
        YTOP=YTOP-CCUTBACK
        CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,1,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME1)
        CALL EZGET(SECT//'_BOARD_X_OUC',XOUC,IER)
        XOUC=XOUC-CCUTBACK
        CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,2,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME2)
        ECVOLUME=ECVOLUME1+ECVOLUME2
        LAR_AREA=ECVOLUME/THICK
        INSIDE_VOL(MAT_CODE,ISTEP)=2.*ECVOLUME
        OUTSID_VOL(MAT_CODE)=MTR_AREA*2.*THICK-2.*ECVOLUME
        TOT_THICK(ISTEP)=TOT_THICK(ISTEP)+2.*THICK
C
C----------------------------------------------------------------------
C-    Calculate ABSORBER Volumes.
C----------------------------------------------------------------------
C-    There is no ABSORBER before GAP 1, only the frontplate.
        IF (IGAP.EQ.1) goto 100
C-    Choose between MFH and MCH.
          IF (IGAP.GT.MFH_GAPMAX) then
            MAT_CODE=1
          ELSE
            MAT_CODE=2
          ENDIF
          CALL EZGET(SECT//'_ABSORBER_THICKNESS',THICK,IER)
          CALL EZGET(SECT//'_ABSORB_X_INC',XINC,IER)
          CALL EZGET(SECT//'_ABSORB_Y_INC',YINC,IER)
          CALL EZGET(SECT//'_ABSORB_X_OUC',XOUC,IER)
          CALL EZGET('ECMH_MODULE_Y_OUC',YOUC,IER)
          CALL EZGET(SECT//'_ABSORB_X_TOP',XTOP,IER)
          CALL EZGET(SECT//'_ABSORB_Y_TOP',YTOP,IER)
          CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,1,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME1)
          CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,2,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME2)
          ECVOLUME=ECVOLUME1+ECVOLUME2
          INSIDE_VOL(MAT_CODE,ISTEP)=LAR_AREA*THICK
          OUTSID_VOL(MAT_CODE)=ECVOLUME-LAR_AREA*THICK
          OUTSID_VOL(3)=MTR_AREA*THICK-ECVOLUME
          TOT_THICK(ISTEP)=TOT_THICK(ISTEP)+THICK
100     CONTINUE
C
C----------------------------------------------------------------------
C-    Calculate signal BOARD Volumes.
C----------------------------------------------------------------------
        MAT_CODE=4
        CALL EZGET('ECMH_SIGNALBOARD_THICKNESS',THICK,IER)
        CALL EZGET(SECT//'_BOARD_X_INC',XINC,IER)
        CALL EZGET(SECT//'_BOARD_Y_INC',YINC,IER)
        CALL EZGET(SECT//'_BOARD_X_OUC',XOUC,IER)
        CALL EZGET('ECMH_MODULE_Y_OUC',YOUC,IER)
        CALL EZGET(SECT//'_BOARD_X_TOP',XTOP,IER)
        CALL EZGET(SECT//'_BOARD_Y_TOP',YTOP,IER)
        CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,1,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME1)
        CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,2,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME2)
        ECVOLUME=ECVOLUME1+ECVOLUME2
        INSIDE_VOL(MAT_CODE,ISTEP)=LAR_AREA*THICK
        OUTSID_VOL(MAT_CODE)=ECVOLUME-LAR_AREA*THICK
        OUTSID_VOL(3)=MTR_AREA*THICK-ECVOLUME
        TOT_THICK(ISTEP)=TOT_THICK(ISTEP)+THICK
C
C----------------------------------------------------------------------
C-    If at a READOUT gap, Calculate a READOUT BOARD Volume.
C----------------------------------------------------------------------
        IF (IGAP.EQ.READOUT_GAP(IREADOUT)) then
          IREADOUT=IREADOUT+1
          CALL EZGET('ECMH_READOUTBOARD_THICKNESS',THICK,IER)
          CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,1,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME1)
          CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,2,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME2)
          ECVOLUME=ECVOLUME1+ECVOLUME2
          INSIDE_VOL(MAT_CODE,ISTEP)=LAR_AREA*THICK
          OUTSID_VOL(MAT_CODE)=ECVOLUME-LAR_AREA*THICK
          OUTSID_VOL(3)=MTR_AREA*THICK-ECVOLUME
          TOT_THICK(ISTEP)=TOT_THICK(ISTEP)+THICK
        ENDIF
C
      ENDDO
C
C----------------------------------------------------------------------
C-  Create the Mixtures.
C----------------------------------------------------------------------
C     First do the Mother Mixture.
C     WRITE (*,*) 'ECMH Homogenized Mother Material'
      MATERIAL_LABEL='ECMH_HOMO_MOTHER_CODE'
      MATERIAL_CODE=MATERIAL_START_CODE
      CALL UCTOH('ECMHMAT0 $  ',MATERIAL_NAME,12,12)
      OUTSID_TOT=OUTSID_VOL(1)+OUTSID_VOL(2)+
     &           OUTSID_VOL(3)+OUTSID_VOL(4)
      N = 0
      DO 200 I=1,4
        IF (OUTSID_VOL(I).LE.0.) GO TO 200
        N = N + 1
        IF (I.EQ.1) CALL EZGET('STAINLESS_STEEL_CODE',
     &              COMPONENT_CODE(I),IER)
        IF (I.EQ.2) CALL EZGET('URANIUM_CODE',COMPONENT_CODE(I),IER)
        IF (I.EQ.3) CALL EZGET('LIQUID_ARGON_CODE',COMPONENT_CODE(I),
     &              IER)
        IF (I.EQ.4) CALL EZGET('G10_CODE',COMPONENT_CODE(I),IER)
        COMPONENT_FRACTION(N)=OUTSID_VOL(I)/OUTSID_TOT
200   CONTINUE
      NUMBER_COMPONENTS = N
      CALL STORE_MATERIAL
C     Next do Materials 1-3.
      DO 240 IMATER=1,3
        IF (IMATER.EQ.1) ISTEP=1
        IF (IMATER.EQ.2) ISTEP=2
        IF (IMATER.EQ.3) ISTEP=9
        WRITE (MATER,'(I1)') IMATER
C       WRITE (*,*) 'ECMH Homogenized Material '//MATER
        MATERIAL_LABEL='ECMH_HOMO_MATERIAL'//MATER//'_CODE'
        MATERIAL_CODE=MATERIAL_START_CODE+IMATER
        CALL UCTOH('ECMHMAT'//MATER//' $  ',MATERIAL_NAME,12,12)
        INSIDE_TOT=INSIDE_VOL(1,ISTEP)+INSIDE_VOL(2,ISTEP)+
     &         INSIDE_VOL(3,ISTEP)+INSIDE_VOL(4,ISTEP)
        N = 0
        DO 220 I=1,4
        IF (INSIDE_VOL(I,ISTEP).LE.0.) GO TO 220
          N = N + 1
          IF (I.EQ.1) CALL EZGET('STAINLESS_STEEL_CODE',
     &                COMPONENT_CODE(I),IER)
          IF (I.EQ.2) CALL EZGET('URANIUM_CODE',COMPONENT_CODE(I),IER)
          IF (I.EQ.3) CALL EZGET('LIQUID_ARGON_CODE',COMPONENT_CODE(I),
     &                IER)
          IF (I.EQ.4) CALL EZGET('G10_CODE',COMPONENT_CODE(I),IER)
          COMPONENT_FRACTION(N)=INSIDE_VOL(I,ISTEP)/INSIDE_TOT
220     CONTINUE
        NUMBER_COMPONENTS = N
        CALL STORE_MATERIAL
240   CONTINUE
C
C----------------------------------------------------------------------
C-    Actually create these modules
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- Advance Zcursor for the FRONTPLATE.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_FRONTPLATE_THICKNESS',THICK,IER)
      YCURS=THICK
C
C----------------------------------------------------------------------
C-    Generate the Homogenized Steps.
C-    These have dimensions of the signal board
C-    MINUS the resist-coat cutback.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_HOMO_VOLUME_NAME',VOLUME_NAME,IER)
      DO ISTEP=1,NSTEPS
        IF (ISTEP.GE.9) THEN
          SECT='ECMCH'
        ELSE
          SECT='ECMFH'
        ENDIF
        IF (ISTEP.EQ.1) IMATER=1
        IF (ISTEP.GE.2) IMATER=2
        IF (ISTEP.GE.9) IMATER=3
        WRITE (STEP,'(Z1)') ISTEP
C       WRITE (*,*) 'ECMH Homogenized Step '//STEP
        VOLUME_MATERIAL_CODE=MATERIAL_START_CODE+IMATER
        THICK=TOT_THICK(ISTEP)
        CALL EZGET(SECT//'_BOARD_X_INC',XINC,IER)
        XINC=XINC-CCUTBACK
        CALL EZGET(SECT//'_BOARD_Y_INC',YINC,IER)
        YINC=YINC+CUTBACK
        CALL EZGET(SECT//'_BOARD_X_OUC',XOUC,IER)
        XOUC=XOUC-SCUTBACK
        CALL EZGET('ECMH_MODULE_Y_OUC',YOUC,IER)
        CALL EZGET(SECT//'_BOARD_X_TOP',XTOP,IER)
        CALL EZGET(SECT//'_BOARD_Y_TOP',YTOP,IER)
        YTOP=YTOP-CCUTBACK
        VOLUME_LABEL='ECMH_HOMO_STEP'//STEP//'_VOLUME_TOP'
        VOLNAM(3:4)=STEP//'T'
        VOLMOT(4:4)='T'
        CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,1,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME)
        X_POSITION=X_POSITION-X_MOTHER_TOP
        Y_POSITION=Y_POSITION-Y_MOTHER_TOP
        Z_POSITION=Z_POSITION-Z_MOTHER_TOP
        ROTATION_MATRIX = 1             ! Identity
        CALL WRITE_VOLUME
        CALL EZGET(SECT//'_BOARD_X_OUC',XOUC,IER)
        XOUC=XOUC-CCUTBACK
        VOLUME_LABEL='ECMH_HOMO_STEP'//STEP//'_VOLUME_BOT'
        VOLNAM(3:4)=STEP//'B'
        VOLMOT(4:4)='B'
        CALL ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK,2,
     &               PARAM,X_POSITION,Y_POSITION,Z_POSITION,ECVOLUME)
        X_POSITION=X_POSITION-X_MOTHER_BOT
        Y_POSITION=Y_POSITION-Y_MOTHER_BOT
        Z_POSITION=Z_POSITION-Z_MOTHER_BOT
        ROTATION_MATRIX = 1             ! Identity
        CALL WRITE_VOLUME
        YCURS=YCURS+THICK
      ENDDO
C
  999 RETURN
      END
