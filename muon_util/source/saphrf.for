      SUBROUTINE SAPHRF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : refill SAPH bank from SATN/S bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-JUL-1995   Andrei Mayorov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LSAPH,GZSAPH,LSATR,GZSATN,GZSATS,GZMUHT,LMUHT
      EXTERNAL GZSATN,GZSATS,GZSAPH,GZMUHT
      INTEGER MODULE,STATION,SECTION,TUBE,NHITS,DIR,ITRAK,IT,POINTER
      INTEGER IH,NHITST
      INTEGER ND,NHTB,NHTA,NWD
      PARAMETER (ND=150)  ! number of words per track in SATN/S
      PARAMETER (NHTB=17) ! pointer on number of hits before magnet
      PARAMETER (NHTA=18) ! -------                   after
      PARAMETER (NWD=19)  ! number of words per hit in SAPH
C----------------------------------------------------------------------
      NHITS=0
      DO DIR=1,2        ! number of hits on SAMUS tracks
        ITRAK=0
        IF(DIR.EQ.1) THEN
          LSATR=GZSATN()
        ELSE
          LSATR=GZSATS()
        END IF
        IF (LSATR.NE.0) ITRAK=IQ(LSATR+1)
        DO IT=1,ITRAK
          NHITS=NHITS+IQ(LSATR+1+NHTB)+IQ(LSATR+1+NHTA)
          LSATR=LSATR+ND
        END DO
      END DO
C
      LSAPH=GZSAPH(0)
      IF(LSAPH.EQ.0) THEN
        CALL BKSAPH(0,NHITS*NWD,LSAPH)
      ELSE
        CALL MZPUSH(IXCOM,LSAPH,0,NWD*NHITS,'I')
      END IF
      DO DIR=1,2
        IF(DIR.EQ.1) THEN
          LSATR=GZSATN()
        ELSE
          LSATR=GZSATS()
        ENDIF
        ITRAK=IQ(LSATR+1)
        DO IT=1,ITRAK
          POINTER=LSATR+1+ND*(IT-1)
          NHITST=IQ(POINTER+NHTB)+IQ(POINTER+NHTA)
          DO IH=1,NHITST
            MODULE=IQ(POINTER+31)
            CALL MVBITS(MODULE,0,5,STATION,0)
            CALL MVBITS(MODULE,5,4,SECTION,0)
            CALL MVBITS(MODULE,16,15,TUBE,0)
            CALL SATOPM(STATION,SECTION,MODULE)
            IQ(LSAPH+1) = MODULE*256+TUBE
            IQ(LSAPH+2) = -999
            IQ(LSAPH+3) = -999
            CALL SAGEOM(MODULE,TUBE,
     &        IQ(LSAPH+6),Q(LSAPH+7),Q(LSAPH+8),
     &        Q(LSAPH+9),Q(LSAPH+10),Q(LSAPH+13))
            Q(LSAPH+16) = -999.
            Q(LSAPH+17) = -999.
            Q(LSAPH+18) = FLOAT(IQ(POINTER+33))*1.E-5
            Q(LSAPH+19) = -999.
            LSAPH=LSAPH+NWD
            POINTER=POINTER+3
          END DO
        END DO
      END DO
      LMUHT=GZMUHT()
      IQ(LMUHT+5)=NHITS
  999 RETURN
      END
