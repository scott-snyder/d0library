      SUBROUTINE EVMARK(OSTRM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        Mark banks to be dropped from output stream OSTRM
C-
C-  Input:
C-    OSTRM = character*3 output stream label
C-
C-  ENTRY EVDROP(OSTRM,NAME):  put bank NAME on list to be dropped
C-
C-  ENTRY EVBACK(OSTRM,NAME):  remove bank NAME from list to be dropped
C-
C-  ENTRY EVDRLS(OSTRM,NUM,NAMLST) : give list of banks marked
C-   Output:
C-     NUM   = number of banks on list
C-     NAMLST= character*4 array with list of names
C-
C-  ENTRY EVBANK_CHECK(OSTRM,NAME,YES) : tell if NAME is on list 
C-   Output:
C-     YES   = NAME is on list to drop from OSTRM
C-
C-
C-   Created  7-June-1989   Serban D. Protopopescu
C-   Modified 16-AUG-1992   sss - compile on ibm
C-   Modified 18-Aug-1994   Herbert Greenlee
C-                          Use LZCSAN instead of LZFIDH (faster).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 OSTRM
      CHARACTER*4 NAME
      INTEGER NUM
      CHARACTER*4 NAMLST(*)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      LOGICAL  FOUND,YES
      INTEGER NSTRMS
      PARAMETER (NSTRMS=8)
      INTEGER MAXDRP
      PARAMETER (MAXDRP=100)
      CHARACTER*4 NAMDRS(MAXDRP,NSTRMS),TEMP
      INTEGER I,J,N,IL,LZSCAN,LBANK,LGO,ISTRM
      INTEGER NUMDRS(NSTRMS),IDS(2),IDLAST(2,NSTRMS),HNAM(MAXDRP,NSTRMS)
      SAVE NAMDRS,NUMDRS,IDLAST
      DATA IDLAST/NSTRMS*0,NSTRMS*0/
      DATA NUMDRS/NSTRMS*0/
C----------------------------------------------------------------------
C
      CALL EVSTRM(OSTRM,ISTRM)  ! Get stream number
C
C   Dropping should be done only once per event
C
      CALL EVNTID(IDS(1),IDS(2))
      IF(IDS(1).EQ.IDLAST(1,ISTRM).AND.
     &  IDS(2).EQ.IDLAST(2,ISTRM)) GOTO 999  ! done
      IDLAST(1,ISTRM)=IDS(1)
      IDLAST(2,ISTRM)=IDS(2)
      DO I=1,NUMDRS(ISTRM)
        CALL UCTOH(NAMDRS(I,ISTRM),HNAM(I,ISTRM),4,4)
      ENDDO
C ****** use MZDROP instead of MZMARK for now ****
C      CALL MZMARK(IXCOM,LHEAD,'-',NUMDRS(ISTRM),HNAM(1,ISTRM))
C-
C- Make a linear scan through the event division and look for banks to drop.
C-
      LGO=0
 2    LBANK=LZSCAN(IXMAIN,LGO)
      IF(LBANK.NE.0) THEN
        LGO=LBANK
        IF(IQUEST(1).EQ.0)THEN
          DO I=1,NUMDRS(ISTRM)
            IF(IQ(LBANK-4).EQ.HNAM(I,ISTRM))THEN
              CALL MZDROP(IXCOM,LBANK,'L')
              GOTO 2
            ENDIF
          ENDDO
        ENDIF
        GOTO 2
      ENDIF
C ************************************************
      RETURN
C
C
      ENTRY EVDROP(OSTRM,NAME)
C
      CALL EVSTRM(OSTRM,ISTRM)  ! Get stream number
C
C ****  Check if bank buffer is full.
C
      IF ( NUMDRS(ISTRM) .GE. MAXDRP ) THEN
        CALL ERRMSG('No more room','EVDROP',
     &    ' Number of drops for '//OSTRM//' exceeded','F')
        GOTO 999
      ENDIF
C
C ****  Put bank on list if not already there
C
      CALL LOCSTR (NAME,NAMDRS(1,ISTRM),NUMDRS(ISTRM),FOUND,IL)
      IF ( .NOT.FOUND ) THEN
        NUMDRS(ISTRM) = NUMDRS(ISTRM) + 1
        J=NUMDRS(ISTRM)
        NAMDRS(J,ISTRM) = NAME
      ENDIF
  100 CONTINUE
C
C ****  Sort names into alphabetical order; use Tree sort.
C
      DO 300 I = 1,NUMDRS(ISTRM)
        DO 200 J = 1,NUMDRS(ISTRM)
          IF(NAMDRS(I,ISTRM).GT.NAMDRS(J,ISTRM)) GOTO 200
          TEMP = NAMDRS(I,ISTRM)
          NAMDRS(I,ISTRM)=NAMDRS(J,ISTRM)
          NAMDRS(J,ISTRM) = TEMP
  200   CONTINUE
  300 CONTINUE
C
      RETURN
C
C
      ENTRY EVBACK(OSTRM,NAME)
C
      CALL EVSTRM(OSTRM,ISTRM)  ! Get stream number
C
C ****  Remove bank from list
C
      CALL LOCSTR (NAME,NAMDRS(1,ISTRM),NUMDRS(ISTRM),FOUND,IL)
      IF ( FOUND ) THEN
        NUMDRS(ISTRM) = NUMDRS(ISTRM) - 1
        DO 90 J =  IL,NUMDRS(ISTRM)
          NAMDRS(J,ISTRM) = NAMDRS(J+1,ISTRM)
   90   CONTINUE
      ENDIF
C
      RETURN
C
C
      ENTRY EVDRLS(OSTRM,NUM,NAMLST)
C
      NUM=0
      CALL EVSTRM(OSTRM,ISTRM)  ! Get stream number
      IF(ISTRM.EQ.0) RETURN
      NUM=NUMDRS(ISTRM)
      DO 500 I=1,NUM
        NAMLST(I)=NAMDRS(I,ISTRM)
  500 CONTINUE
C
      RETURN
C
C
      ENTRY EVBANK_CHECK(OSTRM,NAME,YES)
C
      YES=.FALSE.
      CALL EVSTRM(OSTRM,ISTRM)  ! Get stream number
      IF(ISTRM.EQ.0) RETURN
      DO I=1,NUMDRS(ISTRM)
        IF(NAME.EQ.NAMDRS(I,ISTRM)) YES=.TRUE.
      ENDDO
  999 RETURN
      END
