package fr.istic.si2.huffman

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   */
  
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (l, h) match {
      case (Zero::reste, Noeud(_,a,b)) => decodeSymbolv0(a,reste)
      case (One::reste, Noeud(_,a,b)) => decodeSymbolv0(b,reste)
      case (_, Feuille(_,c)) => Some(c)
      case(Nil, Noeud(_,_,_)) => None
    }
  }

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage de l selon h
   *         - deuxième composante : la liste des bits restant à décoder après avoir suivi l dans h.
   */

  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    
    (l, h) match {
      case (Zero::reste, Noeud(_,a,b)) => decodeSymbol(a,reste)
      case (One::reste, Noeud(_,a,b)) => decodeSymbol(b,reste)
      case (reste, Feuille(_,c)) => ( Some(c) , reste )
      case(Nil, Noeud(_,_,_)) => ( None,Nil )
    }
    
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h
   */
    def decode(l: List[Bit], h: Huffman): String = {
    l match {
      case Nil => ""
      case x::reste =>
        decodeSymbol(h,l) match {
          case ( char, suite ) => 
            char match {
              case Some(a) => a + decode(suite,h)
              case None => ""
            }
        }
    }
  }

}