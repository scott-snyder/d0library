      SUBROUTINE TOP_DILEP_SETUP_TAGS(KTAGS,KNAMES,XTUP_NAMES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-OCT-1994   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*32  KNAMES(100),XTUP_NAMES(100),TAGS_SUFFIX
      INTEGER KTAGS,LENX,TRULEN,I,IND
C----------------------------------------------------------------------
      IF (KTAGS.GT.100) THEN
        CALL ERRMSG('TOP_DILEP_setup_tags','TOP_DILEP_setup_tags',
     &    'KTAGS exceed 100','F')
        GOTO 999
      ENDIF
      XTUP_NAMES(1) = KNAMES(1)
      TAGS_SUFFIX = '('//KNAMES(1)(1:TRULEN(KNAMES(1)))//')'
      LENX = TRULEN(TAGS_SUFFIX)
      DO I=2, KTAGS
        IND = INDEX(KNAMES(I),':')
        IF(IND.EQ.0) THEN 
          XTUP_NAMES(I) = KNAMES(I)(1:TRULEN(KNAMES(I)))
     &    //TAGS_SUFFIX(1:LENX)
        ELSE
          XTUP_NAMES(I) = KNAMES(I)(1:IND-1)//TAGS_SUFFIX(1:LENX)
     &      //KNAMES(I)(IND:TRULEN(KNAMES(I)))
        ENDIF
      ENDDO
  999 RETURN
      END
