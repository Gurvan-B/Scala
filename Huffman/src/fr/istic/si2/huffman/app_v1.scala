package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Application principale V1
 */
object HuffmanApp1 extends App {

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
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).reduce(_ + _)
  }
  
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

  appV1()
  
    /**
   * Fonction principale de la v1 qui affiche les résultats des fonction pour la chaine de caractere demandée
   * Effet de bord : Affiche des chaînes de caractères encodés puis décodés
   */
  
  def appV1 (): Unit = {
    var arret = false
    while (!arret){
      var reponse = ""
      println("Chaîne à encoder ?")
      var chaine = io.StdIn.readLine()
      println("Chaîne encodée standard:")
      println(vers16Bits(chaine))
      println("Taille (nb Bits) : " + vers16Bits(chaine).length )
      println("Chaîne encodée Huffman:")
      var chainencode = encode(chaine,h)
      println(toString(chainencode))
      println("Taille (nb Bits) : " + chainencode.length )
      println("Chaîne décodée Huffman:")
      println(decode(chainencode,h))
      println("encore ? [Y/N]")
      while ( reponse != "N" && reponse != "Y" ){
      reponse = io.StdIn.readLine()
      }
      if (reponse=="Y") arret = false
      else arret = true
    }
    
  }

}