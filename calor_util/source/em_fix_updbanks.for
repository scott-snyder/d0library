      SUBROUTINE EM_FIX_UPDBANKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-JUL-1995   Meenakshi Narain
C-   MODIFIED 19-AUG-1995   DOUGLAS NORMAN
c-                          pushed hmtp banks so that ppho can be
c-                          shunted to pelcs if necessary.
C-   Updated   7-NOV-1995   Meenakshi Narain   use MZREPL instead of MZPUSH
C-                          to define I/O char of the bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INTEGER GZPELC,GZPPHO,NMORE
      INTEGER NDATA,IXIO,BOOK_PELC(5),BOOK_PPHO(5)
      INTEGER LTEMP,LNEW,ND
C----------------------------------------------------------------------
C
C ****  PELC and HMTE
C
      LPELC = GZPELC()
      IF (LPELC.GT.0) THEN
        CALL MZFORM('PELC','2I 27F 2I 4F 1I 10F 1I',IXIO)        ! Bank format
        CALL UCTOH('PELC',BOOK_PELC(1),4,4)
        NDATA = 47
        BOOK_PELC(2) = 4
        BOOK_PELC(3) = 1
        BOOK_PELC(4) = NDATA
        BOOK_PELC(5) = IXIO
      ENDIF
      LTEMP = 0
      DO WHILE (LPELC.GT.0) 
        CALL MZBOOK(IXMAIN,LTEMP,LTEMP,1,'PTMP',2,2,1,0,-1)
        LQ(LTEMP-1) = LPELC
        CALL MZLIFT(IXMAIN,LNEW,LTEMP,-2,BOOK_PELC,0) 
        CALL UCOPY(LQ(LPELC-4),LQ(LNEW-4),4)    
        IQ(LNEW+1) = 7
        ND = IQ(LPELC-1)
        CALL UCOPY(IQ(LPELC+2),IQ(LNEW+2),ND-1)
        LPELC = LQ(LPELC)
      ENDDO
      CALL MZREPL(IXMAIN,LTEMP,' ')

      LPELC = GZPELC()
      DO WHILE (LPELC.GT.0) 
        LHMTE = LQ(LPELC-1)
        IF (IQ(LHMTE+1).LT.4) THEN 
          NMORE = 4
          CALL MZPUSH(IXCOM,LHMTE,0,NMORE,' ')
        ENDIF
        IQ(LHMTE+1) = 4
        LPELC = LQ(LPELC)
      ENDDO
C
C ****  PPHO and HMTP
C
      LPPHO = GZPPHO()
      IF(LPPHO.GT.0) THEN
        CALL MZFORM('PPHO','2I 27F 2I 4F 1I 10F 1I',IXIO)        ! Bank format
        CALL UCTOH('PPHO',BOOK_PPHO(1),4,4)
        NDATA = 47
        BOOK_PPHO(2) = 4
        BOOK_PPHO(3) = 1
        BOOK_PPHO(4) = NDATA
        BOOK_PPHO(5) = IXIO
      ENDIF
      LTEMP = 0
      DO WHILE (LPPHO.GT.0) 
        CALL MZBOOK(IXMAIN,LTEMP,LTEMP,1,'PTMP',2,2,1,0,-1)
        LQ(LTEMP-1) = LPPHO
        CALL MZLIFT(IXMAIN,LNEW,LTEMP,-2,BOOK_PPHO,0) 
        CALL UCOPY(LQ(LPPHO-4),LQ(LNEW-4),4)    
        IQ(LNEW+1) = 7
        ND = IQ(LPPHO-1)
        CALL UCOPY(IQ(LPPHO+2),IQ(LNEW+2),ND-1)
        LPPHO = LQ(LPPHO)
      ENDDO
      CALL MZREPL(IXMAIN,LTEMP,' ')
      LPPHO = GZPPHO()
      DO WHILE (LPPHO.GT.0) 
        LHMTP = LQ(LPPHO-1)
        IF (IQ(LHMTP+1).LE.4) THEN 
          NMORE = 4
          CALL MZPUSH(IXCOM,LHMTP,0,NMORE,' ')
        ENDIF
        IQ(LHMTP+1) = 4
        LPPHO = LQ(LPPHO)
      ENDDO

  999 RETURN
      END
