      SUBROUTINE CORRMR
     &  (ETHRESH,ET_THRNEG,SUME_MR,SUMET_MR,VECET_MR,EGLOB,NCG,
     &  VECET_MRX,VECET_CHNEG,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : correct missing Et for main ring bias by excluding
C-                          CH and OH for events for which the MRBS_LOSS or 
C-                          MICRO_BLANK and-or terms were set.
C-        Additional corrections due to negative energies in
C-        FH3, ECMG, CCMG, and ICD are also calculated.  Calculated quantities
C-        are stored in the GLOB bank
C-        Calculates quantities for GLOB bank
C-
C-   Inputs  : ETHRESH  [R]  - Energy Threshold for NCG (GLOB)
C-             ET_THRNEG(2)  - thresholds on negative energy in ccmg, icd,
C-                             ecmg, fh3, and ch
C-   
C-   Outputs : SUME_MR       - sum of energy in main ring region
C-             SUMET_MR      - scalar sum of Et in main ring region
C-             VECET_MR(1:3) - vector sum of Et in CC-CH and EC-OH
C-             VECET_MR(4:5) - error**2 for VECET_MR(1:2)
C-             VECET_MR(6)   - =0.
C-             EGLOB(8)      - Energy words for GLOB bank
C-             NCG           - N cells word for GLOB bank
C-             STATUS        - <0 -> CAEH not present - no correction computed
C-                              0 or -1 -> MICRO_BLANK=false and
C-                                         (MRBS_LOSS=false or not available)
C-                              1 or -2 -> MICRO_BLANK=true or MRBS_LOSS=true
C-                              2 or -3 -> no and-or terms available
C-             VECET_MRX(6)  - vecet_mr for ICD/MG/FH3 with Et.lt.et_thrneg(1)
C-             VECET_CHNEG(6)  - vecet_mr for ch/oh with Et.lt.et_thrneg(2)
C-                             
C-             the "main ring region" consists of all CC-CH and EC-OH cells
C-             with 16=< IPHI =<20.
C-             
C-   Controls: MRBS_LOSS, MICRO_BLANK and-or terms from TRGR or HEAD bank
C-
C-   Created  16-DEC-1992   Ulrich Heintz   
C-   Modified 05-JAN-1993   Andrew G. Brandt fill GLOB bank
C-   Updated  14-JAN-1993   Harrison B. Prosper   
C-      Add ETHRESH argument. Move call to VCORFL to GLOBFL
C-   Modified 04-FEB-1993   Andrew G. Brandt ECS bug fix
C-   Modified Jan-14-1995   Meenakshi Narain, Bob Kehoe -- modified from
C-              corrmr.for to include negative Et vectors from ICD, MG, 
C-              FH3, and CH layers
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL ETHRESH, EN(4),EGLOB(*),CC_SUMET,EC_SUMET
      INTEGER NCG
      REAL    SUME_MR,SUMET_MR,VECET_MR(*)
      REAL    VECET_CHNEG(*),VECET_MRX(*)
      REAL    ET_THRNEG(2)
      INTEGER STATUS,GZTRGR,LTRGR,GZCAEH,LCAEH
      INTEGER NCH,POINTR,I,J,NR,LAYER,IETA,IPHI,IRET,SETA
      LOGICAL MICROBLANK,MRBSLOSS,MRVETO,QNEG_ENERGY
C      LOGICAL MICRO_BLANK,MRBS_LOSS
C----------------------------------------------------------------------
C... initializations
      STATUS = 0
      SUME_MR = 0.
      SUMET_MR = 0.
      DO I=1,6
        VECET_MR(I) = 0.
        VECET_MRX(I) = 0.
        VECET_CHNEG(I) = 0.
      ENDDO
C
C... GLOB variables
      CALL VZERO(EN,4)
      CALL VZERO(EGLOB,8)
      NCG=0
      CC_SUMET=0.
      EC_SUMET=0.
C
C... first check whether MICRO_BLANK or MRBS_LOSS and or terms are true 
      LTRGR=GZTRGR()
      IF(LTRGR.GT.0)THEN
C        IF(MICRO_BLANK(IRET).OR.MRBS_LOSS(IRET))STATUS=1
        MRBSLOSS = MRVETO('MRBS_LOSS')
        MICROBLANK = MRVETO('MICRO_BLANK')
        IF (MRBSLOSS.OR.MICROBLANK) STATUS=1
      ELSE
        IF(IQ(LHEAD+14).GE.5)THEN ! check head bank version
          IF(IAND(IQ(LHEAD+30),1).EQ.1)STATUS=1
        ELSE
          STATUS=2
        ENDIF
      ENDIF
C
      LCAEH=GZCAEH()
      IF(LCAEH.LE.0)THEN
        CALL ERRMSG('CAEH not found','CORRMR','no corrections','W')
        STATUS=-1-STATUS
        GOTO 999
      ENDIF
      NR=IQ(LCAEH+2)
      NCH=IQ(LCAEH+3)
      POINTR=LCAEH
C
C         loop over channels
C
      DO 1 I=1,NCH
        SETA=IQ(POINTR+12)
        IETA=ABS(SETA)
        IPHI=IQ(POINTR+13)
        LAYER=IQ(POINTR+14)
        IF((LAYER.EQ.15.AND.IETA.LE.12).OR.   ! if CC-CH or EC-OH
     &     (LAYER.EQ.16.AND.IETA.LE.13).OR.
     &     (LAYER.EQ.17.AND.IETA.LE.14))THEN
          QNEG_ENERGY = .FALSE.
          IF (Q(POINTR+8).LE.(-ET_THRNEG(1))) THEN
            QNEG_ENERGY = .TRUE.
          ENDIF
          DO J=1,3
            VECET_MR(J)=VECET_MR(J)-Q(POINTR+3+J)
            IF (QNEG_ENERGY) 
     &        VECET_CHNEG(J)=VECET_CHNEG(J)-Q(POINTR+3+J)
          ENDDO
          DO J=4,5
            VECET_MR(J)=VECET_MR(J)+Q(POINTR+5+J)
            IF (QNEG_ENERGY)
     &        VECET_CHNEG(J)=VECET_CHNEG(J)+Q(POINTR+5+J)
          ENDDO
          SUME_MR=SUME_MR+Q(POINTR+7)
          SUMET_MR=SUMET_MR+Q(POINTR+8)
        ENDIF
        IF ((LAYER.EQ.8.AND.IETA.LE.11) .OR. ! CCMG
     &      (LAYER.EQ.9.AND.IETA.LE.11) .OR. ! ICD
     &      (LAYER.EQ.10.AND.IETA.LE.13)) THEN  ! ECMG
          IF (Q(POINTR+8).LE.(-ET_THRNEG(2))) THEN
            DO J=1,3
              VECET_MRX(J)=VECET_MRX(J)-Q(POINTR+3+J)
            ENDDO
            DO J=4,5
              VECET_MRX(J)=VECET_MRX(J)+Q(POINTR+5+J)
            ENDDO
          ENDIF
        ELSEIF (LAYER.EQ.13.AND.IETA.LE.12)THEN ! FH3
          IF (Q(POINTR+8).LE.(-ET_THRNEG(1))) THEN
            DO J=1,3
              VECET_MRX(J)=VECET_MRX(J)-Q(POINTR+3+J)
            ENDDO
            DO J=4,5
              VECET_MRX(J)=VECET_MRX(J)+Q(POINTR+5+J)
            ENDDO
          ENDIF
        ENDIF
C
C Fill E/ET words for GLOB bank
C
        IF(IETA.LT.13) THEN
          CC_SUMET=CC_SUMET+Q(POINTR+8)
C Increment cell counter if energy above ETHRESH (300 MeV)
          IF(((LAYER.GE.1.AND.LAYER.LE.7).OR.
     &        (LAYER.GE.11.AND.LAYER.LE.14)).AND.
     &        Q(POINTR+7).GT.ETHRESH) NCG=NCG+1
        ELSE
          EC_SUMET=EC_SUMET+Q(POINTR+8)
        END IF
C
C J=1 -EC (+z) South   J=2 CC up  J=3 CC down  J=4 EC North
C
        J=1
        IF(SETA.GT.-13.AND.IPHI.LT.33) J=2
        IF(SETA.GT.-13.AND.IPHI.GT.32) J=3
        IF(SETA.GT.12) J=4
        EN(J)=EN(J)+Q(POINTR+7)
        POINTR=POINTR+NR
    1 CONTINUE
      EGLOB(1)=CC_SUMET+EC_SUMET
      EGLOB(2)=EC_SUMET
      EGLOB(3)=CC_SUMET
      EGLOB(4)=EN(1)+EN(2)+EN(3)+EN(4)
      EGLOB(5)=EN(1)
      EGLOB(6)=EN(2)
      EGLOB(7)=EN(3)
      EGLOB(8)=EN(4)
C
  999 RETURN
      END
