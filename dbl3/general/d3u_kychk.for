C----------------------------------------------------------------------
      SUBROUTINE D3U_KYCHK (CLS,NKS,CHOP,NERR,LLA,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will check consistency of key(3) and key(4)
C-    for class CLS for current path. A class and if key(3:4) is time or 
C-    not is what is specified in in D3U_SET_FOPT or D3U_INI. It calls 
C-    D3U_KYCLS, to get all keys for class CLS.
C-
C-   Inputs  : CLS     Class to be checked.
C-             NKS     Number of keys pr. entry (1'st dim. of KYS)
C-             CHOP    ' ' if keys already fetched by D3U_KYGET it will
C-                         not fetch keys again
C-                     'F' force it to fetch keys again 
C-                     'M' it will also modify wrong key(4)'s !!!
C-   Outputs : NERR    Number of bad keys found
C-             LLA     .true.  if last key = 999999999
C-                     .false. else
C-             IRET    Number of keys checked
C-   Controls: 
C-
C-   Created  11-SEP-1992   Lars O. Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      REAL D3UXH
C
      INTEGER NKS
      INTEGER CLS(NKS),NERR,IRET
      LOGICAL LLA
      CHARACTER*(*) CHOP
C
      INTEGER I,J,K
      REAL DXS
      CHARACTER*12 CHP
      CHARACTER*62 STR
C
      INTEGER         KKMAX
      PARAMETER      (KKMAX=20000)
      INTEGER         KKNN
      PARAMETER      (KKNN = 15)
      INTEGER         KKY(KKNN,KKMAX), NKKY
      COMMON /D3UKYS/ KKY,NKKY
C
      INTEGER         XH(KKNN,KKMAX),NK
      INTEGER         EKY(KKNN),NKY(KKNN)
      LOGICAL         LMOD,LERR
C----------------------------------------------------------------------
      NERR = 0
      IRET = 0
      CALL UPCASE (CHOP,CHP)
      LLA = .TRUE.
      LMOD = .FALSE.
C
      IF (INDEX (CHP,'M') .GT. 0) LMOD = .TRUE.
C
C- Get all keys for class CLS
C
      CALL D3U_KYCLS (CLS,NKS,CHP,XH,NK)
      IF (NK .LE. 0) RETURN
C
C- Check the last key ...
C
      IF (XH(4,1) .NE. 999999999) THEN
         LLA = .FALSE.
         IF (LMOD) THEN
            CALL UCOPY(XH(1,1),EKY(1),NKS)
            CALL UCOPY(XH(1,1),NKY(1),NKS)
            NKY(4) = 999999999
            WRITE(STR,801) 'changing',EKY(3),EKY(4),NKY(3),NKY(4)
            CALL MSGO('i','D3U_KYCHK',STR,0)
            CALL DBRENK(D3_PATH,EKY,NKY)
            IF (IQUEST(1) .NE. 0) 
     &        CALL MSGO('en','D3U_KYCHK','ERROR from dbrenk ',iquest(1))
         END IF
      END IF
C
C- ... and now the rest of the keys
C
      DO I = 2,NK
         LERR = .FALSE.
         IF (D3_NOT) THEN
            IF (XH(3,I-1)-XH(4,I) .NE. 1) LERR = .TRUE.
         ELSE 
            DXS = 3600.*D3UXH (XH(3,I-1),XH(4,I))
            IF (DXS .GT.  2.5 .OR. DXS .LT. 0.5) LERR = .TRUE.
         END IF
         IF (LERR) THEN
            NERR = NERR + 1
            IF (LMOD) THEN
               CALL UCOPY(XH(1,I),EKY(1),NKS)
               CALL UCOPY(XH(1,I),NKY(1),NKS)
               IF (D3_NOT) THEN
                  NKY(4) = XH(3,I-1) - 1
               ELSE
                  NKY(4) = XH(3,I-1)
                  CALL DBINCT (NKY(4), -1,NKY(4))
               END IF
               WRITE(STR,801) 'changing',EKY(3),EKY(4),NKY(3),NKY(4)
               CALL MSGO('i','D3U_KYCHK',STR,0)
               CALL DBRENK(D3_PATH,EKY,NKY)
               IF (IQUEST(1) .NE. 0) 
     &        CALL MSGO('en','D3U_KYCHK','ERROR from dbrenk ',iquest(1))
            END IF
         END IF
      END DO
      IRET = NK
C
801   format (' ',a8,1x,' old ',i9,1x,i9,' new ',i9,1x,i9)
      RETURN
      END
