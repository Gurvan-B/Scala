package fr.istic.si2.huffman

import scala.io.Source
import java.io.{ File, PrintWriter }
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

/**
 * Application principale
 */
object HuffmanApp extends App {

  /**
   * @param nom le nom d'un fichier
   * @return la chaîne contenue dans le fichier nommé nom
   */
  
  def lireFichier(nom: String): String = {
    Source.fromFile(nom).getLines.mkString
  }

  /**
   * Ecrit une chaîne de caractères dans un fichier.
   *  Le fichier est écrasé s'il était déjà existant.
   *
   * @param nom le nom du fichier dans lequel on écrit
   * @param contenu la chaîne de caractères à écrire
   */
  
  def ecrireFichier(nom: String, contenu: String): Unit = {
    val writer = new PrintWriter(new File(nom))
    writer.write(contenu)
    writer.close()
  }
  
  
testV2(  ('a',0.6)::('b',0.1)::('c',0.3)::Nil, "abracadabra" )


   /**
   * @param l une liste de tuples contenant un caractère et sa fréquence d'apparition
   * @param s une chaîne de caractères
   * Affiche le résultat des différentes fonctions selon les 2 paramètres
   */

  def testV2 (l: List[(Char, Double)] , s :String) : Unit = {
    println("**** Test V2 ****")
    println()
    
    println("- Initialisation: ")
    println(l)
    val h = initHuffman(l)
    println(h)
    println()
    
    println("- Tri selon les fréquences: ")
    val h2 = triSelonFreq(h)
    println(h2)
    println()
    
    println("- Code Huffman: ")
    val code = codeHuffman(l)
    println(code)
    println()
    
    println("- Analyse des fréquences: ")
    println(analyseFrequences(s))
    println()
    
    println("- Arbre final pour " + s + " : ")
    println(codeHuffman(analyseFrequences(s)))
  }


appV2("fichiers/texte2.txt")

  /**
   * @param s une chaîne de caractères correspondant à l'adresse du fichier à compresser.
   * Ecrit un fichier "Texte encodé" qui contient une chaîne de bits correspondants au texte en binaire compressé.
   * Ecrit un deuxième fichier "Texte non encodé" qui contient une chaîne de bits correspondant au texte en binaire non compressé.
   */

  def appV2 (s: String) : Unit = {
    println()
    println("**** App V2 ****")
    println()
    val texte = lireFichier(s)
    val h = codeHuffman(analyseFrequences(texte))
    val encodage = toString2(encode(texte,h))
    ecrireFichier("Texte encodé",encodage)
    println("Longueur texte encodé: " + encodage.length)
    val texteNE = vers16Bits(texte)
    ecrireFichier("Texte non encodé", texteNE)
    println("Longueur texte non encodé: " + texteNE.length)
  }

  /**
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */

  def toString2(l: List[Bit]): String = {
    l match {
    case Nil => ""
    case x::reste =>
      if ( x ==Zero ) "0" + toString2(reste)
      else "1" + toString2(reste)
    }
  }
  
  /**
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).reduce(_ + _)
  }
  
}