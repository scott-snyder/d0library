
      SUBROUTINE ECEM_HOMO_GEOM 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the homogenized GEANT ECEM geometry
C-
C-   There are 8 volumes in the homogenized version of the ECEM, all
C-   contained in the ECEM module volume which is filled with LAr.
C-   (plus 4 volumes defined in ECEM_MISC_GEOM)
C-
C-      - 7 'TUBE's corresponding to EM1-4.  These are active volumes
C-       consisting of a mixture of absorber, LAr, G10 and copper.
C-       EM3 is subdivided into 3 volumes, EM4 into 2.
C-      - 4 'TUBE's each of which is an annulus about EM1-4. These
C-       are inactive volumes which contain the edge materials
C-       extending beyond the active region. 

C- Note: the EM3 and 4 readout boards which extend beyond the inactive
C- 'TUBE's have been ignored. The EM1&2 readout ring, the strongback
C-  and the stainless steel skin on EM4 are defined in ECEM_MISC_GEOM.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-FEB-1989   Natalie Roe   
C-   Updated   5-MAR-1990   Stuart Fuess   Evaluate MOTHERZ;
C-                                         set material names,...
C-   Updated  13-MAR-1990   Natalie Roe    Move strongback, support tube
C-                                         to ECEM_MISC_GEOM
C-   Updated  20-MAY-1990   Natalie Roe    Divide into EM3A,B,C and EM4A,B
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
C  Integers
      INTEGER IER,I,J,K,L,ICODE,NLAY(4),NZ,LSTRING
C  Reals
      REAL CM_PER_INCH,ZHILO(2),VAL(4),
     &  TOTAL_THICKNESS(4), MAT_THICKNESS(4,5),THICK,Z,
     &  IRAD,ORAD,ZFIRST,ZLAST,MOTHERZ
C  Characters
      CHARACTER*32 NAME
      CHARACTER*1 LETTER(3)
C  Parameters
      PARAMETER ( CM_PER_INCH = 2.54 )
      CHARACTER*4 MAT_CODE,MAT_LIST(5)
      CHARACTER*8 PLATE_MATERIAL(5)
      DATA PLATE_MATERIAL/'ABSORBER',
     &                    'ARGON_A ',
     &                    'MLB     ',
     &                    'COPPER  ',
     &                    'ARGON_B '/
      DATA MAT_LIST/'SS  ',
     &              'LA  ',
     &              'G10 ',
     &              'CU  ',
     &              'U   '/   
      DATA NLAY/2,2,6,8/
      DATA LETTER/'A','B','C'/  
C----------------------------------------------------------------------
D     WRITE(6,*)'IN ECEM_HOMO_GEOM'
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
C----------------------------------------------------------------------
C  Mother Volume Z-position
C       NZ      Number of z planes in module polycone
C       ZFIRST  z position of first z plane (inches)
C       ZLAST   z position of last z plane (inches)
C       MOTHERZ z position of center of module (inches)
C----------------------------------------------------------------------
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',4,4,1,NZ,IER)
      IF (IER.NE.0) WRITE(6,1000) IER,'NZ'
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',5,5,1,ZFIRST,IER)
      IF (IER.NE.0) WRITE(6,1000) IER,'ZFIRST'
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',5+3*(NZ-1),5+3*(NZ-1),
     &  1,ZLAST,IER)
      IF (IER.NE.0) WRITE(6,1000) IER,'ZLAST'
      MOTHERZ = 0.5 * ( ZFIRST + ZLAST )
C----------------------------------------------------------------------
C  ACTIVE VOLUMES FOR EM1-4
C------------------------------------------------------------------------
C FIRST COMPUTE RELATIVE THICKNESS OF EACH MATERIAL IN EM1-4
      L=0
      DO I=1,4
       TOTAL_THICKNESS(I)=0.
       DO J=1,5
        MAT_THICKNESS(I,J)=0.
       ENDDO
       DO J=1,NLAY(I)
        L=L+1
        DO K=1,5
         IF(I.EQ.4.AND.J.EQ.1.AND.K.EQ.1)GOTO 101 ! Strongback
         WRITE(NAME,1007)L,PLATE_MATERIAL(K)
         CALL EZGETA(NAME,2,2,1,ICODE,IER) 
         IF(IER.NE.0)WRITE(6,1000)IER,'ICODE'
         CALL UHTOC(ICODE,4,MAT_CODE,4)
         ICODE=1         
         DO WHILE(MAT_CODE.NE.MAT_LIST(ICODE))
          ICODE=ICODE+1
         ENDDO 
         CALL EZGETA(NAME,6,6,1,THICK,IER)
         TOTAL_THICKNESS(I)=TOTAL_THICKNESS(I)+THICK
         MAT_THICKNESS(I,ICODE)=MAT_THICKNESS(I,ICODE)+THICK
101      CONTINUE
        ENDDO
       ENDDO
      ENDDO
C---------------------------------------------------------------------------
C   FILL IN PARAMETERS FOR EACH ACTIVE HOMOGENOUS VOLUME
C---------------------------------------------------------------------------
      DO I=1,4
C FIRST DEFINE MATERIAL FOR THIS MIXTURE
        WRITE(MATERIAL_LABEL,1003) I
        CALL ADDSTR(MATERIAL_LABEL,'_MATERIAL_NAME',NAME,LSTRING)
        CALL EZGET(NAME,MATERIAL_NAME,IER)
        CALL ADDSTR(MATERIAL_LABEL,'_MATERIAL_CODE',NAME,LSTRING)
        CALL EZGET(NAME,MATERIAL_CODE,IER)
        NUMBER_COMPONENTS=0
        DO J=1,5
          IF(MAT_THICKNESS(I,J).GT.0.)THEN
          NUMBER_COMPONENTS=NUMBER_COMPONENTS+1
          CALL EZGET(MAT_LIST(J),COMPONENT_CODE(NUMBER_COMPONENTS),IER)
          COMPONENT_FRACTION(NUMBER_COMPONENTS)=
     &      MAT_THICKNESS(I,J)/TOTAL_THICKNESS(I)
          ENDIF 
        ENDDO 
        CALL STORE_MATERIAL 
        VOLUME_MATERIAL_CODE=MATERIAL_CODE
C NOW REST OF PARAMETERS
        CALL EZGET('ECEM_HOMO_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
        IF(IER.NE.0)WRITE(6,1000)IER,'MOTHER' 
        CALL UCTOH('TUBE',VOLUME_SHAPE,4,4)               
        CALL UCTOH('POS',POSITIONING,4,3)               
        ROTATION_MATRIX = 1               ! Identity
        COPY_NUMBER=1
        IF(I.LE.2)THEN        
         WRITE(VOLUME_LABEL,1002)I
         CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
         CALL EZGET(NAME,VOLUME_NAME,IER)
         IF(IER.NE.0)WRITE(6,1000)IER,'NAME'
         NUMBER_PARAMS=3 
         CALL EZGETA(VOLUME_LABEL,4,7,1,VAL(1),IER)
         PARAM(1)= CM_PER_INCH * VAL(1)  ! inner radius
         PARAM(2)= CM_PER_INCH * VAL(2)  ! outer radius
         PARAM(3)= CM_PER_INCH * (VAL(4)-VAL(3))/2. ! half length
         IF(IER.NE.0)WRITE(6,1000)IER,'PARAMS'
         X_POSITION=0.
         Y_POSITION=0.
         Z_POSITION = CM_PER_INCH * ( (VAL(3)+VAL(4))/2. - MOTHERZ)
         CALL WRITE_VOLUME
        ELSE IF (I.EQ.3)THEN 
         DO J=1,3
          WRITE(VOLUME_LABEL,1008)LETTER(J)
          CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
          CALL EZGET(NAME,VOLUME_NAME,IER)
          IF(IER.NE.0)WRITE(6,1000)IER,'NAME'
          NUMBER_PARAMS=3 
          CALL EZGETA(VOLUME_LABEL,4,7,1,VAL(1),IER)
          PARAM(1)= CM_PER_INCH * VAL(1)  ! inner radius
          PARAM(2)= CM_PER_INCH * VAL(2)  ! outer radius
          PARAM(3)= CM_PER_INCH * (VAL(4)-VAL(3))/2. ! half length
          IF(IER.NE.0)WRITE(6,1000)IER,'PARAMS'
          X_POSITION=0.
          Y_POSITION=0.
          Z_POSITION = CM_PER_INCH * ( (VAL(3)+VAL(4))/2. - MOTHERZ)
          CALL WRITE_VOLUME
         ENDDO
        ELSE IF (I.EQ.4)THEN 
         DO J=1,2
          WRITE(VOLUME_LABEL,1009)LETTER(J)
          CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
          CALL EZGET(NAME,VOLUME_NAME,IER)
          IF(IER.NE.0)WRITE(6,1000)IER,'NAME'
          NUMBER_PARAMS=3 
          CALL EZGETA(VOLUME_LABEL,4,7,1,VAL(1),IER)
          PARAM(1)= CM_PER_INCH * VAL(1)  ! inner radius
          PARAM(2)= CM_PER_INCH * VAL(2)  ! outer radius
          PARAM(3)= CM_PER_INCH * (VAL(4)-VAL(3))/2. ! half length
          IF(IER.NE.0)WRITE(6,1000)IER,'PARAMS'
          X_POSITION=0.
          Y_POSITION=0.
          Z_POSITION = CM_PER_INCH * ( (VAL(3)+VAL(4))/2. - MOTHERZ)
          CALL WRITE_VOLUME
         ENDDO
        ENDIF
       ENDDO
C------------------------------------------------------------------------
C  INACTIVE VOLUMES FOR EM1-4
C  THESE ARE TUBES WITH INNER RADIUS EQUAL TO OUTER RADIUS OF ACTIVE VOL,
C  AND OUTER RADIUS EQUAL TO RADIUS OF MLB'S.  
C------------------------------------------------------------------------
C FIRST COMPUTE RELATIVE THICKNESS OF EACH MATERIAL IN EM1-4
      L=0
      DO I=1,4
       DO J=1,5
        MAT_THICKNESS(I,J)=0.
       ENDDO
       DO J=1,NLAY(I)
         L=L+1 
         WRITE(NAME,1007)L,'MLB     ' 
         CALL EZGETA(NAME,6,6,1,THICK,IER)
         MAT_THICKNESS(I,3)=MAT_THICKNESS(I,3)+THICK
         WRITE(NAME,1007)L,'COPPER  ' 
         CALL EZGETA(NAME,6,6,1,THICK,IER)
         MAT_THICKNESS(I,4)=MAT_THICKNESS(I,4)+THICK
       ENDDO
      ENDDO
C---------------------------------------------------------------------------
C   FILL IN PARAMETERS FOR EACH INACTIVE HOMOGENOUS VOLUME
C---------------------------------------------------------------------------
      L=0
      DO I=1,4
C FIRST DEFINE MATERIAL FOR THIS MIXTURE
        WRITE(MATERIAL_LABEL,1004) I
        CALL ADDSTR(MATERIAL_LABEL,'_MATERIAL_NAME',NAME,LSTRING)
        CALL EZGET(NAME,MATERIAL_NAME,IER)
        CALL ADDSTR(MATERIAL_LABEL,'_MATERIAL_CODE',NAME,LSTRING)
        CALL EZGET(NAME,MATERIAL_CODE,IER)
        NUMBER_COMPONENTS=0
        DO J=1,5
          IF(MAT_THICKNESS(I,J).GT.0)THEN
            NUMBER_COMPONENTS=NUMBER_COMPONENTS+1 
            CALL EZGET(MAT_LIST(J),
     &                 COMPONENT_CODE(NUMBER_COMPONENTS),IER) 
            COMPONENT_FRACTION(NUMBER_COMPONENTS)=
     &        MAT_THICKNESS(I,J)/TOTAL_THICKNESS(I)
          ENDIF
        ENDDO
C LAR COMPONENT IS THE REST
        NUMBER_COMPONENTS=NUMBER_COMPONENTS+1 
        CALL EZGET(MAT_LIST(2),COMPONENT_CODE(NUMBER_COMPONENTS),IER) 
        DO J=1,NUMBER_COMPONENTS-1
          COMPONENT_FRACTION(NUMBER_COMPONENTS)= 
     &    1 - COMPONENT_FRACTION(J) 
        ENDDO
        CALL STORE_MATERIAL 
        VOLUME_MATERIAL_CODE=MATERIAL_CODE
C NOW REST OF PARAMETERS
        WRITE(VOLUME_LABEL,1006)I
        CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
        CALL EZGET(NAME,VOLUME_NAME,IER)
        IF(IER.NE.0)WRITE(6,1000)IER,'NAME'
        CALL EZGET('ECEM_HOMO_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
        IF(IER.NE.0)WRITE(6,1000)IER,'MOTHER'
        CALL UCTOH('TUBE',VOLUME_SHAPE,4,4)               
        CALL UCTOH('POS',POSITIONING,4,3)               
        NUMBER_PARAMS=3 
        L=L+NLAY(I)-1
        WRITE(NAME,1007)L,'ARGON_A ' 
        CALL EZGETA(NAME,5,5,1,IRAD,IER) 
        IF(IER.NE.0)WRITE(6,1000)IER,'RADIUS'
        WRITE(NAME,1007)L,'MLB     ' 
        CALL EZGETA(NAME,5,5,1,ORAD,IER) 
        IF(IER.NE.0)WRITE(6,1000)IER,'RADIUS'
        L=L+1
        PARAM(1)= CM_PER_INCH * IRAD  ! inner radius
        PARAM(2)= CM_PER_INCH * ORAD  ! outer radius
        WRITE(NAME,1002)I
        CALL EZGETA(NAME,6,7,1,VAL(1),IER) 
        PARAM(3)= CM_PER_INCH * (VAL(2)-VAL(1))/2. ! half length
        IF(IER.NE.0)WRITE(6,1000)IER,'PARAMS'
        X_POSITION=0.
        Y_POSITION=0.
        Z_POSITION = CM_PER_INCH * ( (VAL(1)+VAL(2))/2. - MOTHERZ)
        ROTATION_MATRIX = 1               ! Identity
        COPY_NUMBER=1
        CALL WRITE_VOLUME
      ENDDO
C----------------------------------------------------------------------
 1000 FORMAT('IER EQ',I3,' IN EZGET FOR ',A12)
 1002 FORMAT('ECEM+Z_FLOOR',I1.1,'_VOLUME')
 1003 FORMAT('ECEM+Z_FLOOR',I1.1)
 1004 FORMAT('ECEM+Z_X_FLOOR',I1.1)
 1006 FORMAT('ECEM+Z_X_FLOOR',I1.1,'_VOLUME')
 1007 FORMAT('ECEM+Z_',I2.2,'_',A8)
 1008 FORMAT('ECEM+Z_FLOOR3',A1,'_VOLUME')
 1009 FORMAT('ECEM+Z_FLOOR4',A1,'_VOLUME')
C----------------------------------------------------------------------
      RETURN
      END
