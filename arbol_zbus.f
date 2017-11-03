      PROGRAM arbol_zbus
      REAL ,dimension(50,50) :: Zbus = 0,cont=0
      INTEGER :: i,j,rr,ne,b,ra,x,nb,P,Q
      REAL :: z
      CHARACTER(LEN=20) filesol
      ! SE DECLARA UNA MATRIZ GENERICA 50X50, Y LAS VARIABLES A UTILIZAR

      ! SE INDICA EL NOMBRE DE FICHERO PARA ESCRIBIR RESULTADOS
      !WRITE (*,'(A)',ADVANCE='NO') ' FICHERO DE RESULTADOS = '
      !READ (*,'(A)') filesol
      !OPEN (12, FILE=filesol)

      ! SE SOLICITAN LOS DATOS BASICOS DEL SISTEMA
      WRITE(*,*) '****DATOS DEL SISTEMA****'
      WRITE(*,*) 'Numero de Buses'
      READ(*,*) nb
      WRITE(*,*) 'Numero de Ramas conectadas a referencia'
      READ(*,*) rr
      WRITE(*,*) 'Numero de Eslabones'
      READ(*,*) ne
      ra = nb-rr ! NÈMERO DE RAMAS QUE NO SON REFERENCIA
      
      ! COMIENZA EL ALGORITMO, SE TRABAJA EL CASO #1
      WRITE(*,*) '***Ahora conecte las ramas con Zb a referencia'
        do j=1,rr
        WRITE(*,*) 'RAMA A REFERENCIA N#',j
        WRITE(*,*) 'Bus a conectar'
        READ(*,*) b
        WRITE(*,*) 'Zb'
        READ(*,*) z
        Zbus(b,b) = z
        enddo
      ! SE IMPRIME LA MATRIZ EN PANTALLA
      WRITE(*,*) '  MATRIZ RESULTANTE'
        do i=1,nb
        WRITE(*,*)(Zbus(i,j),j=1,nb)
        enddo
        
      ! COMIENZA EL CASO #2
       IF ( ra/=0 ) THEN
       WRITE(*,*)'***Ahora conecte las ramas no conect. a tierra con Zb'
         do j=1,ra
         WRITE(*,*) 'RAMA N#',j
         WRITE(*,*) 'Bus Existente'
         READ(*,*) P
         WRITE(*,*) 'Bus Nuevo'
         READ(*,*) Q
         WRITE(*,*) 'Zb'
         READ(*,*) z
         Zbus(:,Q) = Zbus(:,P)
         Zbus(Q,:) = Zbus(P,:)
         Zbus(Q,Q) = z + Zbus(P,P)
         enddo
       ENDIF
      WRITE(*,*) '  MATRIZ RESULTANTE'
        do i=1,nb
        WRITE(*,*)(Zbus(i,j),j=1,nb)
        enddo

      ! COMIENZA EL CASO #4
       IF ( ne/=0 ) THEN
       WRITE(*,*) '***Ahora se agregaran los eslabones con Zb'
         do j=1,ne
         WRITE(*,*) 'ESLABON N#' ,j
         WRITE(*,*) 'Bus inicial (no importa el orden)'
         READ(*,*) P
         WRITE(*,*) 'Bus final (no importa el orden)'
         READ(*,*) Q
         WRITE(*,*) 'Zb'
         READ(*,*) z
         Zbus(:,nb+1) = Zbus(:,P) - Zbus(:,Q)
         Zbus(nb+1,:) = Zbus(P,:) - Zbus(Q,:)
         Zbus(nb+1,nb+1) = Zbus(P,P) + Zbus(Q,Q) - 2*Zbus(P,Q) + z
           do i=1,nb
             do k=1,nb
       cont(i,k)=Zbus(i,k)-( Zbus(i,nb+1)*Zbus(k,nb+1)/Zbus(nb+1,nb+1))
             enddo
           enddo
         Zbus = cont
         enddo
       ENDIF
      WRITE(*,*) '  MATRIZ ZBUS DEL SISTEMA'
        do i=1,nb
        WRITE(*,110) (Zbus(i,j),j=1,nb)
110     FORMAT(10(F5.4,1X))
    !   WRITE(12,110)(Zbus(i,j),j=1,nb)
        enddo
      WRITE(*,*) '***HA FINALIZADO EL PROGRAMA._'
      READ(*,*) ! ESTA LINEA ES PARA EVITAR CIERRE AUTOMµTICO
      END
