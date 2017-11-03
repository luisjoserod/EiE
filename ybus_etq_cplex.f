      PROGRAM ybus_etq
      INTEGER :: e1,e2,nb,ne,g
      COMPLEX :: Ybus(50,50),z
      CHARACTER(LEN=20) filesol
      ! SE DECLARA UNA MATRIZ GENERICA 50X50, Y LAS VARIABLES A UTILIZAR

      !SE INDICA EL NOMBRE DE FICHERO PARA ESCRIBIR RESULTADOS
      !WRITE (*,'(A)',ADVANCE='NO') ' FICHERO DE RESULTADOS = '
      !READ (*,'(A)') filesol
      !OPEN (12, FILE=filesol)

      ! SE SOLICITAN LOS DATOS BASICOS DEL SISTEMA
      WRITE(*,*) '****DATOS DEL SISTEMA****'
      WRITE(*,*) 'Numero de Buses'
      READ(*,*) nb
      WRITE(*,*) 'Numero de elementos'
      READ(*,*) ne

        do g=1,ne
        WRITE(*,*) 'ELEMENTO N#',g
        WRITE(*,*) 'ETIQUETA #1 (no importa el orden)'
        READ(*,*) e1
        WRITE(*,*) 'ETIQUETA #2 (no importa el orden)'
        READ(*,*) e2
        WRITE(*,*) 'IMPEDANCIA DE INTERCONEXI‡N Z'
        READ(*,*) z
          IF (e1 == e2) THEN
            !IF ( Ybus(e1,e1) = 0 ) THEN
            !Ybus(e1,e1) = y
            !ENDIF
          Ybus(e1,e1) = Ybus(e1,e1) + 1/z
          ELSE
          Ybus(e1,e2) = Ybus(e1,e2) - 1/z
          Ybus(e2,e1) = Ybus(e2,e1) - 1/z
          Ybus(e1,e1) = Ybus(e1,e1) + 1/z
          Ybus(e2,e2) = Ybus(e2,e2) + 1/z
          ENDIF
        enddo
        
        WRITE(*,*) '  MATRIZ YBUS DEL SISTEMA'
        do i=1,nb
        WRITE(*,*) (Ybus(i,j),j=1,nb)
!110     FORMAT(10(F5.4,1X))
    !   WRITE(12,110)(Zbus(i,j),j=1,nb)
        enddo
      WRITE(*,*) '***HA FINALIZADO EL PROGRAMA._'
      READ(*,*) ! ESTA LINEA ES PARA EVITAR CIERRE AUTOMµTICO
      END PROGRAM ybus_etq
