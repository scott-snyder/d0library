      SUBROUTINE SAPHFL(IGO,IADD,IMUHP,IFLG1,IFLG2,HIT)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill SAPH bank for one cell hit
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   fill hit
C-                          =98  extend bank for one module
C-                          =99  compress bank
C_              IADD   - cell address
C-              IMUHP  - Pointer to raw data pointer
C-              IFLG1  - quality flag
C-              IFLG2  - dummy
C-              HIT(4) - drift time/error/distance/error
C-
C-    Output :  D0 Zebra output bank.   (SAPH)
C-
C-    Created :  29-AUG-94  M. Fortner
C-    Modified : 10/94 MF add zebra protection
C-               12/94 MF get tube type through SAGEOM
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,IADD,IMUHP,IFLG1,IFLG2
      REAL HIT(4)
C
C  -- local variables.         
      INTEGER LMUHT,LSAPH,LMUHM,ISAPH,NWD,NDUM(4),LMUD1
      INTEGER NHIT,I,NPROC
      INTEGER IMOD,ITUBE,ITYPE,N_SAPH
      PARAMETER (N_SAPH=19)
      REAL SPL,WL1,WL2,RTUB(3),VTUB(3)
      INTEGER  GZSAPH,GZMUHM,GZMUHT
      EXTERNAL GZSAPH,GZMUHM,GZMUHT
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      SAVE LSAPH,ISAPH,LMUHT
      DATA MESSID /'SAPHFL: Zebra bank extend failed'/
      DATA CALLER,MESSAG /'SAPHFL','Insufficient space'/
C
C                Initialize and book bank if needed
C
      IF (IGO.EQ.1) THEN
          LSAPH = GZSAPH(0)
          IF (LSAPH.EQ.0) THEN
              CALL BKSAPH(0,0,LSAPH)
          ENDIF
          LMUHT = GZMUHT(0)
C
C                Extend bank
C
      ELSE IF (IGO.EQ.98) THEN
          LMUHM = GZMUHM(IADD)
          NHIT = IQ(LMUHM+4)
          IF (NHIT.GT.0) THEN
              NWD = 2*NHIT*N_SAPH
              LSAPH = GZSAPH(0)
              IF (LSAPH.EQ.0) THEN
                  NPROC = 0
                  CALL BKSAPH(0,NWD,LSAPH)
              ELSE
                  NPROC = IQ(LSAPH-1)
                  CALL MZPUSH(IXCOM,LSAPH,0,NWD,'I')
              ENDIF
              NHIT = IQ(LSAPH-1) - NPROC
              IF (LSAPH.EQ.0.OR.NHIT.NE.NWD) THEN
                  IADD = -1
                  CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
              END IF
          END IF
          LMUHT = GZMUHT(0)
C 
C                Compress bank
C
      ELSE IF (IGO.EQ.99) THEN
          LMUHM = GZMUHM(IADD)
          NPROC = IQ(LMUHM+7)
          IF (NPROC.EQ.0) THEN
              IFLG2 = -1
              CALL MUHMFL(3,IFLG1,IFLG2,NHIT,NDUM)
          ENDIF
          LSAPH = GZSAPH(0)
          LMUHT = GZMUHT(0)
          NHIT = IQ(LMUHT+5)
          NWD = NHIT*N_SAPH - IQ(LSAPH-1)
          CALL MZPUSH(IXCOM,LSAPH,0,NWD,'I')
C
C                Load new hit
C
      ELSE IF (IGO.EQ.2) THEN
          IMOD = IADD/256
          ITUBE = IADD - IMOD*256
          NHIT = IQ(LMUHT+5)
          ISAPH = LSAPH + NHIT*N_SAPH
          IQ(ISAPH+1) = IADD
          IQ(ISAPH+2) = IMUHP
          IQ(ISAPH+3) = IFLG1
          CALL SAGEOM(IMOD,ITUBE,ITYPE,SPL,WL1,WL2,RTUB,VTUB)
          IQ(ISAPH+6) = ITYPE
          Q(ISAPH+7) = SPL
          Q(ISAPH+8) = WL1
          Q(ISAPH+9) = WL2
          Q(ISAPH+10) = RTUB(1)
          Q(ISAPH+11) = RTUB(2)
          Q(ISAPH+12) = RTUB(3)
          Q(ISAPH+13) = VTUB(1)
          Q(ISAPH+14) = VTUB(2)
          Q(ISAPH+15) = VTUB(3)
          Q(ISAPH+16) = HIT(1)
          Q(ISAPH+17) = HIT(2)
          Q(ISAPH+18) = HIT(3)
          Q(ISAPH+19) = HIT(4)
          NHIT = NHIT + 1
          CALL MUHMFL(3,IMOD,IFLG1,NHIT,NDUM)
          CALL MUOFFL(3,IMOD,IFLG1,NHIT)
C
      ENDIF
C
      RETURN
      END
