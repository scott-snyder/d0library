C DEC/CMS REPLACEMENT HISTORY, Element WRTZDV.FOR
C *11    8-FEB-1989 10:49:50 JONCKHEERE "Add eta indices to ZDiv structure"
C *10   29-DEC-1988 16:27:40 RAJA "Z divisions correct now"
C *9    29-DEC-1988 15:16:27 RAJA "misedit corrected"
C *8    29-DEC-1988 15:10:00 RAJA "IDTYPE removed. DET output removed. Stagger Output put in"
C *7    23-DEC-1988 10:31:45 RAJA "Lout and lout+4 are now the output units"
C *6    21-DEC-1988 12:33:08 RAJA "new zdivision format"
C *5    19-DEC-1988 10:12:03 RAJA "Corrected Z reflection problems"
C *4    13-NOV-1988 18:28:56 RAJA "IH Z DVISIONS, PHI DIVISIONS"
C *3     7-NOV-1988 23:07:07 RAJA "IH and MH"
C *2     6-NOV-1988 17:41:07 RAJA "COmment change"
C *1     6-NOV-1988 16:22:31 RAJA "MH geometry done from Pregeom"
C DEC/CMS REPLACEMENT HISTORY, Element WRTZDV.FOR
      SUBROUTINE WRTZDV(LOUT,NAME,LSR,RSR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write out Zdivision info in CGS system
C-                         to unit LOUT.
C-   Inputs  : LOUT = output stream
C-             NAME = Name of SRCP array
C-             LSR,RSR contents (integer and real)
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-NOV-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER LSR(*)
      INTEGER LOUT,I,IPT,K,LN
      REAL RSR(*),CONV
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL GTSRCP('CONV_FACTOR',CONV,1)
      ENDIF
C
      LN = LEN(NAME)
      WRITE(LOUT,1)NAME(1:LN)
    1 FORMAT('\ARRAY ',A)
C
      WRITE(LOUT,2)LSR(1),LSR(2),RSR(3)
    2 FORMAT(2I8,F8.2,' !Num Z div, Mat no,Stagger of module ')
      IPT = 3
      DO 10 I = 1,LSR(1)
        WRITE(LOUT,3)LSR(IPT+1),CONV*RSR(IPT+2),CONV*RSR(IPT+3),
     &    LSR(IPT+4),(CONV*RSR(IPT+4+K),K=1,LSR(IPT+4))
    3   FORMAT(2X,'''',A4,'''',2F10.3,/,I10,/,(7F10.3))
        IPT = IPT + 3 + LSR(IPT+4)+ 1
        WRITE(LOUT,5) LSR(IPT+1),(RSR(IPT+1+K),K=1,LSR(IPT+1))
    5   FORMAT(I10,/,(7F10.3))
        IPT = IPT + LSR(IPT+1) + 1
        WRITE(LOUT,6)LSR(IPT+1),(LSR(IPT+1+K),K=1,LSR(IPT+1))
    6   FORMAT(I10,/,(7I10))
        IPT = IPT + LSR(IPT+1) + 1
C
   10 CONTINUE
      WRITE(LOUT,4)
    4 FORMAT('\END')
  999 RETURN
      END
