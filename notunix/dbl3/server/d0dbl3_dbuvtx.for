      SUBROUTINE D0DBL3_DBUVTX (IDAY, ITIM)    
*     ==============================    
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBUVTX (IDAY*, ITIM*)                                   *    
*                                                                      *    
*   Returns the current date and time                                  *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     IDAY     Date in YYMMDD                                          *    
*     ITIM     Time in seconds                                         *    
*                                                                      *    
*   Called by DBxxxx in the Example patches                            *    
*                                                                      *    
* **********************************************************************    
*   
*   
*     ------------------------------------------------------------------    
*   
* ** Get date and time for constructing the return arguments    
*   
      CALL LIB$DAY (IDAY,,ITIM) 
*                                                             END DBUVTX    
      END   
