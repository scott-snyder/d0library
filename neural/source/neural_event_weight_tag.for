      SUBROUTINE NEURAL_EVENT_WEIGHT_TAG(NTAGNAME,TAGNAME,IWTAG,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to tag that is to be used for
C-      event weighting. The pointer refers to the ordinal value of the
C-      tag in the ntuple.
C-
C-   Inputs:
C-         NTAGNAME   [I]   the number of tagnames in the array TAGNAME
C-         TAGNAME(*) [C*]  tags obtained using HGIVEN.
C-   Outputs:
C-         IWTAG      [I]   the index of the event weight in the array TAGNAME
C-         STATUS   is zero if all is OK
C-
C-   Controls:
C-
C-   Created   29-MAR-1995   Chip Stewart
C-   Updated  10-JUL-1995   Harrison B. Prosper
C-      Get tag field same way as RGSEARCH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTAGNAME
      CHARACTER*(*) TAGNAME(*)
      INTEGER IWTAG, STATUS
C----------------------------------------------------------------------
      INTEGER I,J,L,II,JJ,KK, IX
      CHARACTER*20  WTAG
      CHARACTER*80 REMARK
C----------------------------------------------------------------------
      IWTAG = 0
C
C ****  Find tag corresponding to weight
C
      CALL DGN_GET_PREFIXED_TAGS('@',1,I,WTAG,IX)
      CALL WORD(WTAG,II,JJ,L)
C
C ****  Check if weight specified; if so check if field is amongst
C ****  those supplied.
C
      IF ( IX .GT. 0 ) THEN
C
        DO I =  1, NTAGNAME
          IX = INDEX(TAGNAME(I),WTAG(1:L))
          IF ( IX.GT.0) THEN
            IWTAG = I
            CALL WORD(WTAG,II,JJ,KK)
            REMARK = ' The field '//WTAG(II:JJ)//
     &        ' will be used to weight events'
            CALL ERRMSG('EVENT_WEIGHT_FOUND','NEURAL_EVENT_WEIGHT_TAG'
     &        ,REMARK,'I')
            GOTO 999
          ENDIF
        ENDDO
C
C ****  Tag not found in list of tags supplied
C
        REMARK = ' The weight field '//WTAG(II:JJ)//
     &           ' was not found amongst the list of tags'
        CALL ERRMSG('EVENT_WEIGHT_NOT_FOUND',
     &              'NEURAL_EVENT_WEIGHT_TAG'
     &              ,REMARK,'F')
      ENDIF
C
  999 RETURN
      END
