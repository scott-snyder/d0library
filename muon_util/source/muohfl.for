      SUBROUTINE MUOHFL(IGO,IFLG1,IFLG2,HIT)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUOH bank for one cell hit
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   fill address, muhp#, corr. data
C-                          =3   fill qual. flag, #times, pos. data
C-                          =4   (muoh#),fill orient, wire coords.
C-                          =5   muoh#, fill rot. flag, rot. offset
C-                          =6   muoh#, fill track#, recalc. pos. data
C-                          =98  extend bank for one module
C-                          =99  compress bank
C-              IFLG1  - address/qual. flag/(muoh#)/muoh#/muoh#
C-              IFLG2  - muhp#/#times/orient/rot. flag/track#
C-              HIT(6) - corr./pos./wire coor(4)/rot.(3)/pos.
C-
C-    Output :  D0 Zebra output bank.   (MUOH)
C-              IFLG1   - hit number in bank (IGO=4)
C-
C-    Created :  9-SEP-93  M. Fortner
C-
C-    Modified: 2/94 M. Fortner  added call to fill MUHM bank
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,IFLG1,IFLG2
      REAL HIT(6)
C
C  -- local variables.         
      INTEGER LMUOH,IMUOH,NWD,NDUM(4),LMUD1
      INTEGER IMOD,NHIT,JHIT,IHIT,I,NPROC
      INTEGER  GZMUOH
      EXTERNAL GZMUOH
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      SAVE LMUOH,IMUOH,NHIT,JHIT
      DATA MESSID /'MUOHFL: Zebra bank extend failed'/
      DATA CALLER,MESSAG /'MUOHFL','Insufficient space'/
C
C                Initialize and book bank if needed
C
      IF (IGO.EQ.1) THEN
          LMUOH = GZMUOH(0)
          IF (LMUOH.EQ.0) THEN
              CALL BKMUOH(0,0,LMUOH)
          ENDIF
          CALL MUDMOD(0,NHIT,JHIT,LMUD1)
          CALL MUHMOD(0,NHIT,JHIT)
C
C                Extend bank
C
      ELSE IF (IGO.EQ.98) THEN
        CALL MUDMOD(IFLG1,NHIT,JHIT,LMUD1)
        IF (NHIT.GT.0) THEN
          NWD = 2*NHIT*28
          LMUOH = GZMUOH(0)
          IF (LMUOH.EQ.0) THEN
              NPROC = 0
              CALL BKMUOH(0,NWD,LMUOH)
          ELSE
              NPROC = IQ(LMUOH-1)
              CALL MZPUSH(IXCOM,LMUOH,0,NWD,'I')
          ENDIF
          NHIT = IQ(LMUOH-1) - NPROC
          IF (LMUOH.EQ.0.OR.NHIT.EQ.0) THEN
              IFLG1 = -1
              CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          END IF
        ENDIF
C
C                Compress bank
C
      ELSE IF (IGO.EQ.99) THEN
          CALL MUHMOD(IFLG1,NHIT,JHIT)
          IF (NHIT.EQ.0) THEN
              IFLG2 = -1
              CALL MUHMFL(3,IFLG1,IFLG2,IHIT,NDUM)
          ENDIF
          LMUOH = GZMUOH(0)
          CALL MUHMOD(0,NHIT,JHIT)
          NWD = NHIT*28 - IQ(LMUOH-1)
          CALL MZPUSH(IXCOM,LMUOH,0,NWD,'I')
C
C                Load new hit with corrected data
C
      ELSE IF (IGO.EQ.2) THEN
          IMOD = IFLG1/256
          CALL MUHMOD(IMOD,NHIT,JHIT)
          IMUOH = LMUOH + (JHIT+NHIT-1)*28
          IQ(IMUOH+1) = IFLG1
          IQ(IMUOH+4) = IFLG2
          DO I = 1,6
              Q(IMUOH+8+I) = HIT(I)
          ENDDO
          IHIT = JHIT+NHIT
          CALL MUHMFL(3,IMOD,IFLG1,IHIT,NDUM)
          CALL MUOFFL(3,IMOD,IFLG1,IHIT)
C
C                Load quality flags and position data
C
      ELSE IF (IGO.EQ.3) THEN
          IQ(IMUOH+2) = IFLG1
          IQ(IMUOH+6) = IFLG2
          DO I = 1,6
              Q(IMUOH+14+I) = HIT(I)
          ENDDO
C
C                Load orientation and wire coordinates
C
      ELSE IF (IGO.EQ.4) THEN
          IFLG1 = NHIT+JHIT+1
          IQ(IMUOH+5) = IFLG2
          DO I = 1,4
              Q(IMUOH+20+I) = HIT(I)
          ENDDO
C
C                Update hit with rotation corrections
C
      ELSE IF (IGO.EQ.5) THEN
          IMUOH = LMUOH + (IFLG1-1)*28
          IQ(IMUOH+7) = IFLG2
          DO I = 1,3
              Q(IMUOH+25+I) = HIT(I)
          ENDDO
C
C                Update hit with new position and track #
C
      ELSE IF (IGO.EQ.6) THEN
          IMUOH = LMUOH + (IFLG1-1)*28
          IQ(IMUOH+3) = IFLG2
          DO I = 1,6
              Q(IMUOH+14+I) = HIT(I)
          ENDDO
C
      ENDIF
C
      RETURN
      END
