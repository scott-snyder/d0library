      SUBROUTINE HIST_TRIG_BOOK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-MAY-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL HCDIR('//PAWC',' ')
      CALL HMDIR('TRIGBITS','S')
C
      CALL HBOOK1(10,'ALL EVENTS L1 BITS',33,-0.5,32.5,0.)
      CALL HBOOK1(100,'EVENTS PASS L1 - L1 BITS',33,-0.5,32.5,0.)
      CALL HBOOK1(101,'EVENTS FAIL L1 - L1 BITS',33,-0.5,32.5,0.)
      CALL HBOOK1(300,'EVENTS PASS USER - L1 BITS',33,-0.5,32.5,0.)
      CALL HBOOK1(301,'EVENTS FAIL USER - L1 BITS',33,-0.5,32.5,0.)
      CALL HBOOK1(500,'EVENTS PASS STRIP - L1 BITS',33,-0.5,32.5,0.)
      CALL HBOOK1(501,'EVENTS FAIL STRIP - L1 BITS',33,-0.5,32.5,0.)
C
      CALL HBOOK1(20,'ALL EVENTS L2 BITS',129,-0.5,128.5,0.)
      CALL HBOOK1(200,'EVENTS PASS L2 - L2 BITS',129,-0.5,128.5,0.)
      CALL HBOOK1(201,'EVENTS FAIL L2 - L2 BITS',129,-0.5,128.5,0.)
      CALL HBOOK1(400,'EVENTS PASS USER - L2 BITS',129,-0.5,128.5,0.)
      CALL HBOOK1(401,'EVENTS FAIL USER - L2 BITS',129,-0.5,128.5,0.)
      CALL HBOOK1(600,'EVENTS PASS STRIP - L2 BITS',129,-0.5,128.5,0.)
      CALL HBOOK1(601,'EVENTS FAIL STRIP - L2 BITS',129,-0.5,128.5,0.)
C
      CALL HCDIR('//PAWC',' ')
  999 RETURN
      END
