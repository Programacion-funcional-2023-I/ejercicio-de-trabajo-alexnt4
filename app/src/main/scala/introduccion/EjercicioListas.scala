package introduccion

import javax.naming.OperationNotSupportedException

class EjercicioListas {
  /*
  * Punto 2 repetir lista
  * @param lista Lista a repetir
  * @param n Número de veces a repetir la lista
  * @return Lista repetida n veces
  * @example repetirLista(List(1, 2, 3), 2) = List(List(1,1),List(2,2),List(3,3))
  * @throws IllegalArgumentException si n es negativo
  */
  def repetirListas(lista: List[Int], n: Int): List[List[Int]] = {
    //Complete el código
    if (n < 0) {
      throw new IllegalArgumentException("El valor de n debe ser mayor o igual a 0")
    } else {
      def repetirElemento(elemento: Int, veces: Int): List[Int] = {
        if (veces == 0) {
          List.empty[Int]
        } else {
          elemento :: repetirElemento(elemento, veces - 1)
        }
      }

      lista.map(elemento => repetirElemento(elemento, n))
    }
    //throw new UnsupportedOperationException("No implementado aún")
  }
  /*
  * Punto 3: Filtrar listas
  * @param criterioIn Criterio de filtrado que puede ser "mayor", "menor", "mayoroigual", "igual", "diferente" o "menoroigual"
  * @param n Número a comparar
  * @param lista Lista a filtrar
  * @return Lista filtrada de acuerdo al criterio y n
  * @throws IllegalArgumentException si el criterio no es uno de los valores válidos
  */

  def filtrarListas(criterioIn: String, n: Int, lista: List[Int]) : List[Int] = {
    //Complete el código
    val criterio = criterioIn.toLowerCase()
    criterio match {
      case "mayor" => lista.filter(_ > n)
      case "menor" => lista.filter(_ < n)
      case "mayoroigual" => lista.filter(_ >= n)
      case "igual" => lista.filter(_ == n)
      case "diferente" => lista.filter(_ != n)
      case "menoroigual" => lista.filter(_ <= n)
      case _ => throw new IllegalArgumentException("Criterio no válido")
    }
    //throw new UnsupportedOperationException("No implementado aún")
  }
}
