      SUBROUTINE CC_WRITE_MATERIALS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the GEANT SRCP parameters for a
C-                         material to a file
C-
C-   Entries:
C-              CC_WRITE_MATERIALS
C-              CC_ZERO_MATERIALS
C-              CC_STORE_MATERIAL
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:CC_MATERIAL_LIST.INC'
      INCLUDE 'D0$INC:CC_MATERIAL.INC'
      INCLUDE 'D0$INC:CC_UNIT.INC'
C  Integers
      INTEGER NM, IM, NC, IC, I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(CC_OUT_VOL,1000)
      WRITE(CC_OUT_VOL,1001) CC_NUMBER_MATERIALS
      DO IM=1,CC_NUMBER_MATERIALS
        NC = CC_NUMBER_COMPONENTS(IM)
        WRITE(CC_OUT_VOL,1002) (CC_MATERIAL_NAME(I,IM),I=1,3),
     &    CC_MATERIAL_CODE(IM),
     &    CC_NUMBER_COMPONENTS(IM)
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
     &    ,(CC_COMPONENT_CODE(IC,IM),IC=1,NC),
     &     (CC_COMPONENT_FRACTION(IC,IM),IC=1,NC)
C&ENDIF
C&IF IBMAIX
C&        DO IC=1,NC
C&          WRITE(CC_OUT_VOL,2002) CC_COMPONENT_CODE(IC,IM)
C&        ENDDO
C&        DO IC=1,NC
C&          WRITE(CC_OUT_VOL,3002) CC_COMPONENT_FRACTION(IC,IM)
C&        ENDDO
C&ENDIF
      ENDDO
      WRITE(CC_OUT_VOL,1003)
      RETURN
C----------------------------------------------------------------------
C  Entry CC_ZERO_MATERIALS
C----------------------------------------------------------------------
      ENTRY CC_ZERO_MATERIALS
      CC_NUMBER_MATERIALS = 0
      RETURN
C----------------------------------------------------------------------
C  Entry CC_STORE_MATERIAL
C----------------------------------------------------------------------
      ENTRY CC_STORE_MATERIAL
      CC_NUMBER_MATERIALS = CC_NUMBER_MATERIALS + 1
      NM = CC_NUMBER_MATERIALS
      CC_MATERIAL_LABEL(NM) = MATERIAL_LABEL
      DO I=1,3
        CC_MATERIAL_NAME(I,NM)  = MATERIAL_NAME(I)
      ENDDO
      CC_MATERIAL_CODE(NM) = MATERIAL_CODE
      CC_NUMBER_COMPONENTS(NM) = NUMBER_COMPONENTS
      NC = NUMBER_COMPONENTS
      DO IC=1,NC
        CC_COMPONENT_CODE(IC,NM) = COMPONENT_CODE(IC)
        CC_COMPONENT_FRACTION(IC,NM) = COMPONENT_FRACTION(IC)
      ENDDO
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT('\ARRAY  CC_MIXTURES')
 1001 FORMAT(I4)
 1002 FORMAT('''',A4,'''',1X,'''',A4,'''',1X,'''',A4,'''',
     &  1X,I4,1X,I4
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
     &  ,<NC>(1X,I4),<NC>(1X,F6.4))
C&ENDIF
C&IF IBMAIX,LINUX
C&     &  )
C& 2002 FORMAT(1X,I4)
C& 3002 FORMAT(1X,F6.4)
C&ENDIF
 1003 FORMAT('\END')
      END
