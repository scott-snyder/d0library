       SUBROUTINE WRITE_MATERIALS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the GEANT SRCP parameters for a
C-                         material to a file
C-
C-   Entries:
C-              WRITE_MATERIALS
C-              ZERO_MATERIALS
C-              STORE_MATERIAL
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created    10-OCT-1989   Stuart Fuess   from CC_WRITE_MATERIALS
C-   Updated    27-APR-1990   Natalie Roe    check for number_components=1:
C-                            if so, this is a material not a mixture and
C-                             component_fraction(1,nm)=Aeff  
C-                             component_fraction(2,nm)=Zeff  
C-                             component_fraction(3,nm)=density (g/cm3) 
C-                             component_fraction(4,nm)=rad. length (cm)
C-                             component_fraction(5,nm)=abs. length (cm) 
C-                            also change calculation of Aeff, Zeff, and
C-                            WMAT for mixtures w/ molecular proportions    
C-   Updated   18-Sep-1992    Herbert B. Greenlee
C-       Get rid VAX extended format statements.
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:MATERIAL_LIST.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER NM, IM, NC, IC, I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(OUT_VOL,1000) MATERIAL_LIST_SRCP_LABEL
      WRITE(OUT_VOL,1001) NUMBER_MATERIALS
      DO IM=1,NUMBER_MATERIALS
       NC = NUMBER_COMPONENTS_LIST(IM)
       IF(NC.EQ.1)THEN  ! material
        WRITE(OUT_VOL,1004) (MATERIAL_NAME_LIST(I,IM),I=1,3),
     &    MATERIAL_CODE_LIST(IM),
     &    NUMBER_COMPONENTS_LIST(IM)
        WRITE(OUT_VOL,1005) (COMPONENT_FRACTION_LIST(IC,IM),IC=1,5)
       ELSE IF(NC.GE.2)THEN             ! mixture
        WRITE(OUT_VOL,1002) (MATERIAL_NAME_LIST(I,IM),I=1,3),
     &    MATERIAL_CODE_LIST(IM),
     &    NUMBER_COMPONENTS_LIST(IM)
        DO IC=1,NC
          WRITE(OUT_VOL,2002) COMPONENT_CODE_LIST(IC,IM)
        ENDDO
        DO IC=1,NC
          WRITE(OUT_VOL,3002) COMPONENT_FRACTION_LIST(IC,IM)
        ENDDO
       ELSE IF(NC.LE.-2)THEN  ! mixture, molecular proportions
        NC=ABS(NC)
        WRITE(OUT_VOL,1002) (MATERIAL_NAME_LIST(I,IM),I=1,3),
     &    MATERIAL_CODE_LIST(IM),
     &    NUMBER_COMPONENTS_LIST(IM)
        DO IC=1,NC
          WRITE(OUT_VOL,2002) COMPONENT_CODE_LIST(IC,IM)
        ENDDO
        DO IC=1,NC+1
          WRITE(OUT_VOL,3002) COMPONENT_FRACTION_LIST(IC,IM)
        ENDDO
       ENDIF 
      ENDDO
      WRITE(OUT_VOL,1003)
      RETURN
C----------------------------------------------------------------------
C  Entry ZERO_MATERIALS
C----------------------------------------------------------------------
      ENTRY ZERO_MATERIALS
      NUMBER_MATERIALS = 0
      RETURN
C----------------------------------------------------------------------
C  Entry STORE_MATERIAL
C----------------------------------------------------------------------
      ENTRY STORE_MATERIAL
      NUMBER_MATERIALS = NUMBER_MATERIALS + 1
      NM = NUMBER_MATERIALS
      MATERIAL_LABEL_LIST(NM) = MATERIAL_LABEL
      DO I=1,3
        MATERIAL_NAME_LIST(I,NM)  = MATERIAL_NAME(I)
      ENDDO
      MATERIAL_CODE_LIST(NM) = MATERIAL_CODE
      NUMBER_COMPONENTS_LIST(NM) = NUMBER_COMPONENTS
      NC = NUMBER_COMPONENTS
      IF(NC.EQ.1)THEN   ! material
       DO IC=1,5
        COMPONENT_FRACTION_LIST(IC,NM) = COMPONENT_FRACTION(IC)
       ENDDO
      ELSE IF(NC.GE.2)THEN ! mixture
        DO IC=1,NC  
         COMPONENT_CODE_LIST(IC,NM) = COMPONENT_CODE(IC)
         COMPONENT_FRACTION_LIST(IC,NM) = COMPONENT_FRACTION(IC)
        ENDDO
      ELSE IF(NC.LE.2)THEN ! mixture,molecular proportions
       NC=ABS(NC)
       DO IC=1,NC  
        COMPONENT_CODE_LIST(IC,NM) = COMPONENT_CODE(IC)
        COMPONENT_FRACTION_LIST(IC,NM) = COMPONENT_FRACTION(IC)
       ENDDO
       COMPONENT_FRACTION_LIST(NC+1,NM) = COMPONENT_FRACTION(NC+1)
      ENDIF

      RETURN
 1002 FORMAT('''',A4,'''',1X,'''',A4,'''',1X,'''',A4,'''',
     &  1X,I4,1X,I4)
 2002 FORMAT(1X,I4)
 3002 FORMAT(1X,F6.4)
C----------------------------------------------------------------------
 1000 FORMAT('\ARRAY ',A32)
 1001 FORMAT(I4)
 1004 FORMAT('''',A4,'''',1X,'''',A4,'''',1X,'''',A4,'''',
     &  1X,I4,1X,I4)
 1005 FORMAT(5(1X,E10.4))
 1003 FORMAT('\END')
      END
