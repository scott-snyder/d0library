      SUBROUTINE DBKDSC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms: DL charge VS SW_o charge
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSDSC(HSTDSC)
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTDSC, SPRTDL, DLMDLP
      INTEGER I, IDM, IDP, IDT, IDAVM, IDAVP, IDAVT
      CHARACTER*25 HSNMM, HSNMP, HSNMT
      CHARACTER*40 TITLE
      CHARACTER*2  OUTWIR(8)
      DATA HSNMM/'DL- charge VS SW_o (wire '/ 
      DATA HSNMP/'DL+ charge VS SW_o (wire '/ 
      DATA HSNMT/'DL charge VS SW_o (wire '/ 
      DATA OUTWIR /'00','06','07','13','14','20','21','27'/
C----------------------------------------------------------------------
C
      YES = .FALSE.
      SPRTDL = .FALSE.
      CALL GETPAR(1,' Book histograms: DL charge VS SW_o charge ? Y/N>',
     &  'L',YES)
      IF(.NOT.YES) GOTO 999
      CALL OUTMSG('  DL charge will be the average of DL- and DL+')
      CALL GETPAR(1,'  Do you also need to seperate DL- and DL+?  Y/N>',
     &  'L',SPRTDL)
C
      IF (SPRTDL) THEN
        DO 100 I = 1,8
          IDM = 4600 + I
          TITLE = HSNMM//OUTWIR(I)//') charge'//'$'
          CALL HBOOK2 (IDM,TITLE,50,1.,1001.,50,1.,1001.,0.) 
          IDAVM = 4630 + I
          CALL HBAVER(IDAVM,TITLE,100,1.,1001.,1.,1001.)
          IDP = 4610 + I
          TITLE = HSNMP//OUTWIR(I)//') charge'//'$'
          CALL HBOOK2 (IDP,TITLE,50,1.,1001.,50,1.,1001.,0.) 
          IDAVP = 4640 + I
          CALL HBAVER(IDAVP,TITLE,100,1.,1001.,1.,1001.)
 100    CONTINUE  
      ENDIF  
C
        DO 200 I = 1,8
          IDT = 4620 + I
          TITLE = HSNMT//OUTWIR(I)//') charge'//'$'
          CALL HBOOK2 (IDT,TITLE,50,1.,1001.,100,1.,2001.,0.) 
          IDAVT = 4650 + I
          CALL HBAVER(IDAVT,TITLE,100,1.,1001.,1.,2001.)
 200   CONTINUE  
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSDSC(HSTDSC,DLMDLP)
      HSTDSC = YES
      DLMDLP = SPRTDL
      RETURN
      END
