package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Type algébrique simple modélisant les bits (0 ou 1)
 */

sealed trait Bit
case object Zero extends Bit
case object One extends Bit

/**
 * Type algébrique récursif modélisant les arbres de code de Huffman
 */

sealed trait Huffman
case class Feuille(freq: Double, c: Char) extends Huffman
case class Noeud(freq: Double, zero: Huffman, one: Huffman) extends Huffman

/**
 * Application principale V0
 */

object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  
  val h: Huffman = Noeud(
      1.00, Feuille(0.45,'a'), Noeud(
          0.54, Feuille(0.18,'r'), Noeud(
              0.36, Noeud(
                  0.18,Feuille(0.09,'c'),Feuille(0.09,'d')),
              Feuille(0.18,'b'))))

  /**
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */
  
  def toString(l: List[Bit]): String = {
    l match {
    case Nil => ""
    case x::reste =>
      if ( x ==Zero ) "0" + toString(reste)
      else "1" + toString(reste)
    }
  }
  

  /**
   * Fonction principale de la v0 qui affiche les résultats des fonction pour chaque lettre en fonction de h
   * Effet de bord : Affiche les caractères de l'alphabet encodés puis décodés
   */
  
  def appV0(): Unit = {
    for(x <- 'a' to 'z') {
      print(x + "   ")
      var encodage = encodeSymbol(x, h)
      encodage match {
        case Some(a) => print(a + "   " + toString(a) + "   ")
        decodeSymbolv0(h, a) match {
        case Some(b) => print(b)
        case None => print("")
        }
      case None => print("N'existe pas")
      }
    println()
    }
  }

}