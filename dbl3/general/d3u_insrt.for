C----------------------------------------------------------------------
      SUBROUTINE D3U_INSRT (LNK,KEY,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will insert a new element into DBL3, in the 
C-   current diretory (path). The data is specifyed by the link, LNK. 
C-   KEY(3) and KEY(4) is the validity range (can be time or run number). 
C-   There is three different cases :
C-   1) There is no previous element of same type (same type is elements
C-      with same user keys (7:N)). In that case there will be created a 
C-      dummy very first element with validity range from 1 to key(3)-1. 
C-      Key 4 of the new element will be forced to 999999999.
C-   2) There is a previous element (of same type), with key(3) different 
C-      from the new elements key(3). Then the previous element will have 
C-      the end validity, key(4), modifyed to key(3)-1 of the new element, 
C-      and the new element will have key(4) modifyed to the previous 
C-      elements key(4) (often that would be 999999999).
C-   3) There is a previous element, with key(3) equal to the new
C-      elements key(3) (this should be a rare situation). Then the 
C-      previous element will have the end validity changed to the its 
C-      start validity, and the end validity of the new element will
C-      be changed to the end validity of the previous element.
C-      
C-        *** If any serious errors, the database will be closed ***
C-
C-   Inputs  : LNK           Link to bank or bank structure to be inserted
C-             KEYS          Keys for that element
C-   Outputs : IRET          0 = ok
C-   Controls: 
C-
C-   Created  18-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Updated  28-FEB-1990   J. Guida - Add KEY(11) as Run number
C-   Stolen   26-NOV-1990   Lor
C-   Updated  15-SEP-1992   Lor, to could handle cases where key 3 and 4
C-                          is not time.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER ICFILA
      INTEGER D3UTOP
C
      INTEGER          LNK
      INTEGER          KEY (D3_NK)
      INTEGER          IRET
C
      CHARACTER*24     TOPD
      INTEGER          I,J,K,JX,IR
      INTEGER*2        JF,JI
      INTEGER          LBANK,LDUM
      INTEGER          KEYO(D3_NK),KEYN(D3_NK),KEYC(D3_NK),KEYR(D3_NK)
      INTEGER          FTIM,LK,LD
      LOGICAL          LNEW
C
      EQUIVALENCE   (D3_LNK(1),LDUM)
      EQUIVALENCE   (D3_LNK(2),LBANK)
C----------------------------------------------------------------------
      IRET = 0
      LNEW = .FALSE.
      LBANK = LNK
      JX = ICFILA ('/',D3_PATH,1,LEN(D3_PATH))
      IF (JX .LT. 1 .OR. JX .GT. LEN(D3_PATH)) THEN
         CALL MSGO('e','D3U_INSRT','Path invalid',0)
         GOTO 999
      END IF
      IF (D3_NOT) THEN
         FTIM = 1
      ELSE 
         CALL DBPKTS (860101,0,FTIM)
      END IF
C
C- Is there any element of same kind in the database ?
C- If not, make sure that key(4)=999999999, and create
C- a dummy very first element of same kind.
C
C
      IR = D3UTOP(D3_PATH,TOPD)
      CALL RZCDIR('//'//TOPD,' ')
      IF (IQUEST(1) .NE. 0) THEN
         CALL MSGO ('e','D3U_INSRT','Path name invalid',0)
         RETURN
      END IF
      DO I = 1,D3_NK
         KEYC(I) = KEY(I)
         KEYR(I) = KEY(I)
      END DO
      KEYC(3) = FTIM
      KEYC(4) = FTIM
      CALL DBUKY  (D3_PATH,KEYO,D3_NK,KEYC,D3_FSTR)
      IF (IQUEST(1) .EQ. 24) THEN
         KEYR(4)=999999999
         IF (KEYR(3) .GT. FTIM) THEN
            CALL MZBOOK
     &        (D3_DIV,LDUM,0,2,D3_PATH(JX+1:JX+4),0,0,4,2,0)
            KEYC(3) = 1
            IF (D3_NOT) THEN
               KEYC(4) = KEYR(3)-1
            ELSE 
               CALL DBINCT (KEYR(3), -1,KEYC(4))
            END IF
            CALL DBENTR
     &        (D3_PATH,LK,LD,D3_DIV,LDUM,D3_NK,KEYC,0,D3_ISTR)
            IF (IQUEST(1) .NE. 0) THEN
               CALL MSGO
     &           ('en','D3U_INSRT','DBENTR error, quest=',IQUEST(1))
               GOTO 991
            END IF
            IF (LDUM .GT. 0) CALL MZDROP(D3_DIV,LDUM,' ')
            LDUM = 0
            LNEW = .TRUE.
         END IF
C         
      ELSE IF (IQUEST(1) .EQ. 0) THEN
         CALL DBFREE (D3_PATH,0,KEYO,'K')
C
C- Get last element
C
         KEYC(3) = KEYR(3)
         KEYC(4) = KEYR(3)
         CALL DBUKY (D3_PATH,KEYO,D3_NK,KEYC,D3_FSTR)
         IF (IQUEST(1) .NE. 0) THEN
            CALL MSGO 
     &        ('en','D3U_INSRT','DBUKY error, quest=',IQUEST(1))
            GOTO 991
         END IF
C
C- Change keys for previous entry
C
         DO J = 1,D3_NK
            KEYN(J) = KEYO(J)               ! name new same as old
         END DO
         IF (KEYN(3) .NE. KEYR(3)) THEN
            IF (D3_NOT) THEN
               KEYN(4) = KEYR(3) - 1
            ELSE 
               CALL DBINCT (KEYR(3), -1,KEYN(4))
            END IF
         ELSE                               ! previous Run
            KEYN(4) = KEYR(3)
         ENDIF
         KEYR(4) = KEYO(4)                  ! Overwrite End Validity of 
C                                           ! current Run
         CALL DBRENK(D3_PATH,KEYO,KEYN)
         IF (IQUEST(1) .NE. 0) THEN
            CALL MSGO 
     &        ('en','D3U_INSRT','DBRENK error, quest=',IQUEST(1))
            CALL DBRENK(D3_PATH,KEYN,KEYO)          
            IF (IQUEST(1) .NE. 0) THEN  
               CALL MSGO ('en','D3U_INSRT',
     &           'Error in modifying keys, quest=',IQUEST(1))
            ENDIF
            GO TO 991
         ENDIF
C
         CALL DBFREE (D3_PATH,0,KEYO,'K')
      ELSE 
         CALL MSGO ('en','D3U_INSRT','DBUSE error, quest=',IQUEST(1))
         GOTO 991
      END IF
C                                                               
      IF (LBANK .LE. 0) THEN
         CALL MSGO ('e','D3U_INSRT','Invalid link',0)
         GOTO 10
      END IF
      CALL DBENTR(D3_PATH,LK,LD,D3_DIV,LBANK,D3_NK,KEYR,0,D3_ISTR)
      IF (IQUEST(1) .NE. 0) THEN
         CALL MSGO ('en','D3U_INSRT','DBENTR error, quest=',IQUEST(1))
         IF (.NOT. LNEW) THEN
            CALL DBRENK(D3_PATH,KEYN,KEYO)          
            IF (IQUEST(1) .NE. 0) THEN
               CALL MSGO ('en','D3U_INSRT',
     &        'Error in modifying keys, quest=',IQUEST(1))
            END IF
        END IF
        GO TO 991
      ENDIF
10    CONTINUE
      GOTO 999
C
991   CALL D3U_END
      IRET = -2
999   RETURN
      END
