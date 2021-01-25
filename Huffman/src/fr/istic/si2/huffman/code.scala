package fr.istic.si2.huffman

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match {
      case Nil => Nil
      case x::reste =>
        x match {
          case (char,double) => Feuille(double,char) :: initHuffman(reste)
        }
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */

  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    l match {
      case Nil => Nil
      case x::reste => 
        insertion( triSelonFreq(reste) ,x )   
    }
  }
  
  /**
   * @param l une liste triée d'arbre de huffman
   * @param x un arbre de huffman
   * @return la liste triée d'arbres de Huffman contenant x
   */ 
  
  def insertion(l: List[Huffman],x: Huffman): List[Huffman] = {
    l match {
      case Nil => x::Nil
      case m::n =>
        if ( getFrequence(x) < getFrequence(m) ) x::l
        else m::insertion(n,x)
    }
  }
  
  /**
   * @param h un arbre de huffman
   * @return la fréquence du noeud racine
   */ 
  
  def getFrequence(h: Huffman): Double = {
    h match {
          case Feuille(freq,_) => freq
          case Noeud(freq,_,_) => freq
    }
  }

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(triSelonFreq(initHuffman((freqs))))
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    l match {
      case x1::x2::reste => 
        Noeud(getFrequence(x1) + getFrequence(x2),x1,x2)::reste
      case x1::Nil => sys.error("uneFusion: Liste n'a pas 2 éléments") 
      case Nil => sys.error("uneFusion: Liste vide")
    }
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  
    def fusion(l: List[Huffman]): Huffman = {
      l match {
        case _ :: _ :: reste => 
          triSelonFreq(l) 
          fusion(uneFusion(l))
        case x::Nil => x
        case Nil => sys.error("uneFusion: Liste vide")
      }
    }

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
    
  def analyseFrequences(s: String): List[(Char, Double)] = {
    analyseFrequences2(s,lettresPresentes(s.toList))
  }
  
  /**
   * @param c un caractère
   * @param l une liste de caractères
   * @return le nombre d'occurences de c dans l
   */
  
  def compter(c: Char, l: List[Char]): Double = {
    l match {
      case x :: reste => if (x == c){ 1 + compter(c,reste) } else { compter(c,reste) }
      case Nil => 0
    }
  }
  
  /**
   * @param s une liste de caractères
   * @return la liste des caractères présents dans s
   */
  
  def lettresPresentes(s: List[Char]): List[Char] = {
    s match {
      case Nil => Nil
      case x :: reste => x :: lettresPresentes(remove(x,s))
    }
  }
  
  /**
   * @param c un caractère
   * @param l une liste de caractères
   * @return la liste de caractères l sans les occurences du caractère c
   */ 
  
  def remove(c : Char,l : List[Char]) : List[Char] = {
    l match {
      case x :: reste =>
       if ( x == c ) remove(c,reste)
       else x :: remove(c,reste)
      case Nil => Nil
    }
  }
  
  /**
   * @param s une chaîne de caractère 
   * @param lettres les lettres présentes dans s 
   * @return les fréquences d'apparition des caractères de lettres dans s
   */ 
  
  def analyseFrequences2(s: String, lettres : List[Char]) : List[(Char, Double)] = {
    val s2 = s.toList
    val longueur = s2.length.toDouble
    lettres match {
      case x1 :: reste =>
        (x1, compter(x1,s2)/longueur) :: analyseFrequences2(s, reste)
      case Nil => Nil
    }
  }
  
  
  

}