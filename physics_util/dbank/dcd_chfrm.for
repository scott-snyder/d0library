      SUBROUTINE DCD_CHFRM(CHFORM,BANK,BNKLEN,FRMT,IOK)
C----------------------------------------------------------------------
C-
C-    Purpose and Methods : Decodes CHFORM into FRMT
C-                   FRMT   = 1  Means bit string (B)
C-                          = 2  Means integer (I)
C-                          = 3  Means floating-point (F)
C-                          = 4  Means double precision 1st wd (D)
C-                          =-4  Means double precision 2nd wd (D)
C-                          = 5  Means 4-character Hollerith (H)
C-
C-   Inputs  : CHFORM - Zebra chartacter string describing
C-                      Bank format
C-             BANK   - The actual data bank (needed for self-describing
C-                      and dynamic length sectors--not allowed yet)
C-             BNKLEN - Length of bank described by CHFORM
C-   Outputs : FRMT   - bank containing data descriptions as above
C-   Controls: IOK    - error flag, 0 means OK
C-
C-   Created  14-APR-1989   Michael W. Peters  Actual Routine
C-   Updated   2-MAY-1989   Michael W. Peters  Changed codes to conform
C-                                             to self-describing sectors.
C-                                             Changed argument list.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*(*) CHFORM
      INTEGER BNKLEN,IOK
      INTEGER BANK(*),FRMT(*)
      CHARACTER*1 CH
      INTEGER NWC,I,J,ITRBEG,ITREND,IDP
      INTEGER LCHFRM,ICH,NDIG,IDIG(10),NUM,IPART,IBNK,ISTATE,IFMT
C----------------------------------------------------------------------
      IF ( BNKLEN.EQ.0 ) THEN
        IOK = 7                         ! ZERO BANK LENGTH
        RETURN
      ENDIF
      LCHFRM=LEN(CHFORM)
      IBNK=0
C
C ****  Part 1 is leading part of bank, part 2 is trailing part
C
      IPART=1
      ISTATE=1
      DO 900 I = 1 , LCHFRM
        CH=CHFORM(I:I)
        ICH=ICHAR(CH)-ICHAR('0')
C
C ****  State 1 is the state expecting the start of an item
C
        IF(ISTATE.EQ.1) THEN
          NWC=0
          NDIG=0
          IF(ICH.GE.0.AND.ICH.LE.9) THEN
            NDIG=NDIG+1
            IDIG(NDIG)=ICH
            ISTATE=2
          ELSEIF(CH.EQ.'-') THEN
            NWC=-1
            ISTATE=2
          ELSEIF(CH.EQ.'/'.AND.IPART.EQ.1) THEN
            IPART=2
            ITRBEG=IBNK+1
          ELSEIF(CH.EQ.' ') THEN
            ISTATE=1
          ELSE
            IOK=1
            GO TO 999
          ENDIF
C
C ****  State 2 evaluates the word-count and the data type and sets the fmt
C
        ELSEIF(ISTATE.EQ.2) THEN
          IF(ICH.GE.0.AND.ICH.LE.9) THEN
            NDIG=NDIG+1
            IF(NDIG.GT.10) THEN
              IOK=6
              GO TO 999
            ENDIF
            IDIG(NDIG)=ICH
          ELSE
            IF(NDIG.GT.0) THEN
              NUM=0
              DO 500 J=1,NDIG
                NUM=10*NUM+IDIG(J)
  500         CONTINUE
            ELSEIF(NWC.EQ.-1) THEN
              NUM=BNKLEN-IBNK
            ELSE
              IOK=2
              GO TO 999
            ENDIF
            IF(CH.EQ.'B') THEN
              IFMT=1
            ELSEIF(CH.EQ.'I') THEN
              IFMT=2
            ELSEIF(CH.EQ.'F') THEN
              IFMT=3
            ELSEIF(CH.EQ.'D') THEN
              IFMT=4
            ELSEIF(CH.EQ.'H') THEN
              IFMT=5
            ELSE
              IOK=3
              GO TO 999
            ENDIF
            IDP=0
            DO 700 J=1,NUM
              IBNK=IBNK+1
              IF(IFMT.EQ.4.AND.IDP.EQ.1) THEN
                FRMT(IBNK)=-IFMT
              ELSE
                FRMT(IBNK)=IFMT
              ENDIF
              IDP=MOD(IDP+1,2)
  700       CONTINUE
            ISTATE=3
          ENDIF
C
C ****  State 3 checks for the separating blank
C
        ELSEIF(ISTATE.EQ.3) THEN
          IF(CH.EQ.' ') THEN
            ISTATE=1
          ELSEIF(CH.EQ.'/'.AND.IPART.EQ.1) THEN
            IPART=2
            ISTATE=1
          ELSE
            IOK=4
            GO TO 999
          ENDIF
        ENDIF
  900 CONTINUE
      ITREND=IBNK
C
C ****  Repeat the trailing part (if any) until the bank is full
C
      IF(IBNK.LT.BNKLEN) THEN
        IF(IPART.EQ.2.AND.ITREND.GE.ITRBEG) THEN
  910     CONTINUE
          DO 920 J=ITRBEG,ITREND
            IBNK=IBNK+1
            IF(IBNK.GT.BNKLEN) GO TO 950
            FRMT(IBNK)=FRMT(J)
  920     CONTINUE
          GO TO 910
        ELSE
          IOK=5
          GO TO 999
        ENDIF
      ENDIF
  950 CONTINUE
C
C ****  Normal return
C
      IOK=0
  999 RETURN
      END
