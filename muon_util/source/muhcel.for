C VAX/DEC CMS REPLACEMENT HISTORY, Element MUHCEL.FOR
C *1    21-OCT-1993 08:52:01 FORTNER "add terms for scintillator"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUHCEL.FOR
      SUBROUTINE MUHCEL(IHIT,NCEL,IQUAL,IFLG)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Extract cell# and quallity flag from MUOH
C-
C-    Input  :  IHIT   - Hit location in MUOH
C-
C-    Output :  NCEL   - Cell number
C-              IQUAL  - Hit quality flag
C-              IFLG   - Dummy Flag word
C-
C-    Created :  24-SEP-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IHIT,NCEL,IQUAL,IFLG
      INTEGER IWADD,IFW1,IFW2,IHRAW,IORIEN,NHWIR
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1
      REAL DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
      INTEGER LEVE,LODD,LDUM,IDUM
      INTEGER IW1,IW2,IW3,IW4,IW5,IW6,IW7,IW8
C
C                Get hit information
C
      CALL GTMUOH(IHIT,IWADD,IFW1,IFW2,IHRAW,IORIEN,NHWIR,
     X            CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     X            DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
      NCEL = IAND(IWADD,255)
      IQUAL = IFW1
      IFLG = 0
C
      RETURN
      END
