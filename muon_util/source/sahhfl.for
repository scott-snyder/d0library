      SUBROUTINE SAHHFL(ISIDE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine fills SAHH and SAMH and SACL
C-                         working banks for SWW overlap code
C-
C-   Created  12-MAR-1991   O. Eroshin 
C-   Updated  DH 5/92 12 bits in module word count
C-   Updated   3-DEC-1992   Alexander Efimov   Change the number of
C-                          parameters in SAADR routine
C-   Replaced  5-Nov-1993   M. Fortner - match 1B unpacking
C-   Replaced  15-Sep-1994  M. Fortner - Fill from MUHM and SAPH
C-                             Combines SAHHFL,SAMHFL,SASORT,SACLST
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER ISIDE,IPLANE,NHITS,IERR
      INTEGER    N_PLANE,N_SAMH,N_SAPH,N_SACL
      PARAMETER (N_PLANE=18,N_SACL=15,N_SAMH=15,N_SAPH=19)
      INTEGER LSAHH,GZSAHH,LSAMH,LSACL,LMUHM1,LMUHM2,GZMUHM,LSAPH,GZSAPH
      EXTERNAL GZSAHH,GZMUHM,GZSAPH
      INTEGER ISAPH,ISAMH,ISACL,JSACL,NCL,LCL,IADD,IASV,NWD
      INTEGER I,II,J,NHIT1,NHIT2,IHIT1,IHIT2,IPLN1
      REAL LEN1,LEN2,S
      INTEGER MODPLN(2,N_PLANE)
      DATA MODPLN/404,400,406,402,405,401,414,410,416,412,413,417,
     &            424,420,426,422,425,421,434,430,436,432,433,437,
     &            444,440,446,442,445,441,454,450,456,452,453,457/
C
C  Get bank pointer and book if needed
C
      IERR = 0
      LSAPH = GZSAPH()
      IF (LSAPH.EQ.0) THEN
        IERR = 1
        GOTO 999
      ENDIF
      LSAHH = GZSAHH()
      IF (LSAHH.EQ.0) THEN
          CALL BKSAHH(0,0,LSAHH)
          DO I = 1,N_PLANE
              IQ(LSAHH+I) = 0
          ENDDO
      ENDIF
      IF (ISIDE.EQ.1) THEN
          IPLN1 = 1
      ELSE
          IPLN1 = 10
      ENDIF
      DO 100 IPLANE = IPLN1,IPLN1+2
        IF (LQ(LSAHH-IPLANE).NE.0) GOTO 100
C
C  Get hit count and book SAMH,SACL banks
C
        LMUHM1 = GZMUHM(MODPLN(1,IPLANE))
        NHIT1 = 0
        IF (LMUHM1.NE.0) THEN
          NHIT1 = IQ(LMUHM1+7)
          IHIT1 = IQ(LMUHM1+8)
        ENDIF
        LMUHM2 = GZMUHM(MODPLN(2,IPLANE))
        NHIT2 = 0
        IF (LMUHM2.NE.0) THEN
          NHIT2 = IQ(LMUHM2+7)
          IHIT2 = IQ(LMUHM2+8)
        ENDIF
        IQ(LSAHH+IPLANE) = NHIT1 + NHIT2
        CALL BKSAMH(IPLANE,LSAMH)
        LSACL = 0
        IF (LSAMH.NE.0) CALL BKSACL(LSAMH,LSACL)
        IF (LSACL.EQ.0) GOTO 100
C
C  Loop over first module
C
        ISACL = LSACL
        NCL    =  0
        LCL    =  0
        IASV   = -2
        NHITS = 0
        IF (NHIT1.GT.0) THEN
          DO I = IHIT1,IHIT1+NHIT1-1
C                                             Load SAMH information
            ISAPH = LSAPH + N_SAPH*(I-1)
            ISAMH = LSAMH + N_SAMH*NHITS
            IQ(ISAMH+1)  = 0
            IQ(ISAMH+2)  = IQ(ISAPH+1)
            IQ(ISAMH+3)  = I
            Q (ISAMH+4)  = Q(ISAPH+7)
            IQ(ISAMH+5)  = IQ(ISAPH+6)
            DO J = 6,15
              Q (ISAMH+J)  = Q(ISAPH+J+4)
            ENDDO
            NHITS = NHITS + 1
C                                             Test for cluster
            IADD = IQ(ISAPH+1)
            IF (IABS(IADD-IASV).LE.1) THEN
              IF (LCL.GT.1) ISACL=ISACL+15
              JSACL = ISACL-15
              S = Q(JSACL+14)/(Q(ISAPH+18)+Q(JSACL+14)+0.00001)
              DO II=1,3
                Q(JSACL+II+5) = Q(JSACL+II+5)
     &                        + (Q(ISAPH+II+9)-Q(JSACL+II+5))*S
              END DO
              IQ(JSACL+ 3) = NHITS
              Q (JSACL+12) = 0.0
              Q (JSACL+14) = 0.0
              CALL SAGTBL(IQ(JSACL+5),LEN1)
              CALL SAGTBL(IQ(ISAPH+6),LEN2)
              IF (LEN1.LT.LEN2) IQ(JSACL+5) = IQ(ISAPH+6)
            ELSE
              LCL = 0
              NCL = NCL+1
            END IF
C                                             Load SACL information
            IQ(ISACL+1) = MOD(IQ(ISACL+1),2**16)+NCL*2**16
            IQ(ISACL+2) = NHITS
            IQ(ISACL+3)  = 0
            Q (ISACL+4)  = Q(ISAPH+7)
            IQ(ISACL+5)  = IQ(ISAPH+6)
            DO J = 6,15
              Q (ISACL+J)  = Q(ISAPH+J+4)
            ENDDO
            IF (LCL.EQ.0) ISACL = ISACL + 15
            LCL = LCL + 1
            IASV = IADD
C
          ENDDO
        ENDIF
C
C  Loop over second module in reverse order
C
        IF (NHIT2.GT.0) THEN
          DO I = IHIT2+NHIT2-1,IHIT2,-1
C                                             Load SAMH information
            ISAPH = LSAPH + N_SAPH*(I-1)
            ISAMH = LSAMH + N_SAMH*NHITS
            IQ(ISAMH+1)  = 0
            IQ(ISAMH+2)  = IQ(ISAPH+1)
            IQ(ISAMH+3)  = I
            Q (ISAMH+4)  = Q(ISAPH+7)
            IQ(ISAMH+5)  = IQ(ISAPH+6)
            DO J = 6,15
              Q (ISAMH+J)  = Q(ISAPH+J+4)
            ENDDO
            NHITS = NHITS + 1
C                                             Test for cluster
            IADD = IQ(ISAPH+1)
            IF (IABS(IADD-IASV).LE.1) THEN
              IF (LCL.GT.1) ISACL=ISACL+15
              JSACL = ISACL-15
              S = Q(JSACL+14)/(Q(ISAPH+18)+Q(JSACL+14)+0.00001)
              DO II=1,3
                Q(JSACL+II+5) = Q(JSACL+II+5)
     &                        + (Q(ISAPH+II+9)-Q(JSACL+II+5))*S
              END DO
              IQ(JSACL+ 3) = NHITS
              Q (JSACL+12) = 0.0
              Q (JSACL+14) = 0.0
              CALL SAGTBL(IQ(JSACL+5),LEN1)
              CALL SAGTBL(IQ(ISAPH+6),LEN2)
              IF (LEN1.LT.LEN2) IQ(JSACL+5) = IQ(ISAPH+6)
            ELSE
              LCL = 0
              NCL = NCL+1
            END IF
C                                             Load SACL information
            IQ(ISACL+1) = MOD(IQ(ISACL+1),2**16)+NCL*2**16
            IQ(ISACL+2) = NHITS
            IQ(ISACL+3)  = 0
            Q (ISACL+4)  = Q(ISAPH+7)
            IQ(ISACL+5)  = IQ(ISAPH+6)
            DO J = 6,15
              Q (ISACL+J)  = Q(ISAPH+J+4)
            ENDDO
            IF (LCL.EQ.0) ISACL = ISACL + 15
            LCL = LCL + 1
            IASV = IADD
C
          ENDDO
        ENDIF
C
C  Fill result into SAHH, compress SACL
C
        IQ(LSAHH+IPLANE) = NCL
        NWD = N_SACL*NCL - IQ(LSACL-1)
        CALL MZPUSH(IXCOM,LSACL,0,NWD,'I')
 100  ENDDO
C
 999  RETURN
      END
