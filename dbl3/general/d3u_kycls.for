C----------------------------------------------------------------------
      SUBROUTINE D3U_KYCLS (CLS,NKS,CHOP,KYS,NKYS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will return all keys, belonging to the class
C-    specified in CLS for the curremt path. A class is what is specified 
C-    in D3U_SET_FOPT or D3U_INI. The keys will be sorted (descending), 
C-    arcording to key(3).
C-
C-   Inputs  : CLS     Class, which keys has to be returned
C-             NKS     Number of keys pr. entry (1'st dim. of KYS)
C-             CHOP    ' ' if keys already fetched by D3U_KYGET it will
C-                         not fetch keys again
C-                     'F' force it to fetch keys again    
C-   Outputs : KYS     Array of keys for class CLS
C-             NKYS    Number of keys
C-   Controls: 
C-
C-   Created  11-SEP-1992   Lars O. Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER NKS,NKYS
      INTEGER KYS(NKS,*), CLS(NKS)
      CHARACTER*(*) CHOP
C
      INTEGER I,J,K,NK
      LOGICAL LFF
      CHARACTER *12 CHP
C
      INTEGER         KKMAX
      PARAMETER      (KKMAX=20000)
      INTEGER         KKNN
      PARAMETER      (KKNN = 15)
      INTEGER         KKY(KKNN,KKMAX), NKKY
      COMMON /D3UKYS/ KKY,NKKY
C
      INTEGER         XH(KKMAX),IKY(KKMAX)
C----------------------------------------------------------------------
      NKYS = 0
      CALL UPCASE (CHOP,CHP)
C
C- Fetch all keys
C
      IF (NKKY .LE. 0 .OR. INDEX(CHP,'F') .GT. 0) THEN
      CALL RZCDIR (D3_PATH,' ')
      CALL RZKEYS (KKNN,KKMAX,KKY,NKKY)
      IF (NKKY .GE. KKMAX) THEN
         CALL MSGO 
     &        ('e','D3U_KYCLS','Oh oh, to many keys ... we quit',0)
         RETURN
      ELSE IF (NKKY .LE. 0) THEN
         RETURN
      END IF
      END IF
C
C- Get filtered key(3) into a 1 dim. array
C
      NK = 0
      CALL VZERO(XH,KKMAX)
      DO 10 K = 1,NKKY
         IF (KKY(3,K) .LT. 10) GOTO 10
         DO J = 1,D3_NKK
            IF (KKY(D3_XKI(J),K) .NE. CLS(D3_XKI(J))) GOTO 10
         END DO
         NK = NK + 1
         XH (K) = KKY(3,K)
10    CONTINUE
C
      IF (NK .LE. 0) RETURN
C
C- Sort'em
C
      CALL SORTZV(XH,IKY,NKKY,-1,1,0)
      NKYS = 0
      DO I = 1,NKKY
         K = IKY(I)
         IF (XH(K) .GT. 0) THEN
            NKYS = NKYS + 1
            CALL UCOPY(KKY(1,K),KYS(1,NKYS),NKS)
         END IF
      END DO
C
999   RETURN
      END
