      SUBROUTINE CJET_MUCATESAV(NTWRS,ECATESAV,ETCATESAV,
     &  EXCATESAV,EYCATESAV,EZCATESAV,SAVE_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save the information in the CATE bank since
C-             I will be changing the CATE bank.
C-
C-   Created: 12-21-1992  by Alex Smith
C-
C-   Inputs:
C-             ECATESAV  : array to store cell E info
C-             ETCATESAV : array to store cell ET info
C-             EXCATESAV : array to store cell ET info
C-             EYCATESAV : array to store cell ET info
C-             EZCATESAV : array to store cell ET info
C-             SAVE_CODE : =1 -> save bank, =2 -> restore bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC/LIST'       ! Protected Link area
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
C
      CHARACTER*40 MSG
      INTEGER K,L,GZCATE
      INTEGER SAVE_CODE,PT_CATE,TOWER_POINTER
      INTEGER IETA,IPHI,NRT,NTOT,NTWMAX
      PARAMETER (NTWMAX=4800)
      REAL ETCATESAV(-NETAL:NETAL,NPHIL,2)
      REAL EXCATESAV(-NETAL:NETAL,NPHIL,2),
     &  EYCATESAV(-NETAL:NETAL,NPHIL,2)
      REAL EZCATESAV(-NETAL:NETAL,NPHIL,2),
     &  ECATESAV(-NETAL:NETAL,NPHIL,2)
      INTEGER NTWRS(2),EVONUM
C
C
C----------------------------------------------------------------------
C
C
C        initialize arrays if saving:
C
      IF (SAVE_CODE.EQ.1) THEN
CC        WRITE(6,*) 'saving CATE bank'   !DEBUG
        NTOT=(2*NETAL+1)*NPHIL*2
        CALL UZERO(ETCATESAV,1,NTOT)
        CALL UZERO(EXCATESAV,1,NTOT)
        CALL UZERO(EYCATESAV,1,NTOT)
        CALL UZERO(EZCATESAV,1,NTOT)
        CALL UZERO(ECATESAV,1,NTOT)
      ENDIF
C
C ****  Save OR restore CATE bank:
C
CC      IF (SAVE_CODE.EQ.2) WRITE(6,*)
CC     &  'restoring CATE bank'   !DEBUG
      LCATE = GZCATE()
      IF ( LCATE .LE. 0 ) THEN
        CALL ERRMSG
     &    ('NO CATE BANK','CJET_MUCATESAV','Unable to save CATE bank',
     &    'W')
        GOTO 999
      ENDIF
      NTWRS(1)=0
      NTWRS(2)=0
      NRT=IQ(LCATE+2)
      DO 11 IETA = -NETAL ,  NETAL
        DO 12 IPHI = 1 ,  NPHIL
          DO 13 K = 1 ,  2
            TOWER_POINTER = PTCATE(IETA,IPHI,K)
            PT_CATE = LCATE + ( (TOWER_POINTER-1) * NRT)
            IF ( TOWER_POINTER .GT. 0 ) THEN
              IF (SAVE_CODE.EQ.1) THEN
C
C *** Save CATE bank (only towers that exist):
C
                EXCATESAV(IETA,IPHI,K) = Q(PT_CATE+4)
                EYCATESAV(IETA,IPHI,K) = Q(PT_CATE+5)
                EZCATESAV(IETA,IPHI,K) = Q(PT_CATE+6)
                ECATESAV(IETA,IPHI,K)  = Q(PT_CATE+7)
                ETCATESAV(IETA,IPHI,K) = Q(PT_CATE+8)
              ENDIF
              IF (SAVE_CODE.EQ.2) THEN
C
C *** Restore CATE bank (only write towers that exist):
C
                Q(PT_CATE+4) = EXCATESAV(IETA,IPHI,K)
                Q(PT_CATE+5) = EYCATESAV(IETA,IPHI,K)
                Q(PT_CATE+6) = EZCATESAV(IETA,IPHI,K)
                Q(PT_CATE+7) = ECATESAV(IETA,IPHI,K)
                Q(PT_CATE+8) = ETCATESAV(IETA,IPHI,K)
              ENDIF
              NTWRS(K)=NTWRS(K)+1
              IF( NTWRS(K).GT.NTWMAX ) THEN
                WRITE(MSG,111) EVONUM()
                CALL ERRMSG('Too many CAL Towers','CJET_MUCATESAV',MSG,
     &            'W')
                GOTO 999        !  failure return
              ENDIF
            ENDIF
   13     CONTINUE
   12   CONTINUE
   11 CONTINUE
  999 RETURN
  111 FORMAT(' SKIP PROCESSING EVENT NO.',I8)
      END
