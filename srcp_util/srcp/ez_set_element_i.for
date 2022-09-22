      subroutine ez_set_element_i (array_name,array_element,idx
     &     ,ocurrence, val, ier)
      implicit none
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      INTEGER IDX, OCURRENCE, VAL(*), IER
      call ez_set_element (array_name, array_element, idx, ocurrence,
     &     val, ier)
      return
      end
