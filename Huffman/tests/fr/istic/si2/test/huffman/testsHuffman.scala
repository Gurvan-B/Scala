package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

import fr.istic.si2.huffman._

class TestsHuffman {

    /**
   * Un arbre à utiliser dans tous les tests
   */
  val h: Huffman = Noeud(1.00, Feuille(0.45,'a'), Noeud(0.54, Feuille(0.18,'r'), Noeud(0.36, Noeud(0.18,Feuille(0.09,'c'),Feuille(0.09,'d')),Feuille(0.18,'b'))))

 
  /**
   * Test décodage d'un caractère V0
   */
  

  @Test
  def testdecodeSymbolv0n0() {
    assertEquals(Some('a'),decodeSymbolv0(h,List(Zero) ) )
  }
  
  @Test
  def testdecodeSymbolv0n1() {
    assertEquals(Some('b'),decodeSymbolv0(h,List(One,One,One) ) )
  }
  
  @Test
  def testdecodeSymbolv0n2() {
    assertEquals(Some('b'),decodeSymbolv0(h,List(One,One,One,One) ) )
  }
  
  @Test
  def testdecodeSymbolv0n3() {
    assertEquals(None,decodeSymbolv0(h,List(One) ) )
  }
   

  /**
   * Test décodage d'une chaine de caractère V1
   */
  
  @Test
  def testdecoden0() {
    assertEquals("a",decode(List(Zero),h ) )
  }
  
  @Test
  def testdecoden1() {
    assertEquals("ab",decode(List(Zero,One,One,One),h ) )
  }
  
  @Test
  def testdecode2() {
    assertEquals("dar",decode(List(One,One,Zero,One,Zero,One,Zero),h ) )
  }
  
  @Test
  def testdecoden3() {
    assertEquals("r",decode(List(One,Zero,One),h ) )
  }
  
  @Test
  def testdecoden4() {
    assertEquals("rb",decode(List(One,Zero,One,One,One,One),h) )
  }
  
  
    /**
   * Test d'encodage d'un caractère V0
   */
  
  @Test
  def testEncodeSymboln0() {
    assertEquals(Some(List(Zero)),encodeSymbol('a',h) )
  }
  
    @Test
  def testEncodeSymboln1() {
    assertEquals(Some(List(One,One,One)),encodeSymbol('b',h) )
  }
    
      @Test
  def testEncodeSymboln2() {
    assertEquals(None,encodeSymbol('z',h) )
  }
      
      
   /**
   * Test d'encodage d'une chaine de caractère V1
   */

      @Test
  def testEncoden0() {
    assertEquals(List(One,One,One,One,Zero,Zero),encode("bra",h) )
  }
  
      @Test
  def testEncoden1() {
    assertEquals(List(One,One,One,One,Zero,Zero),encode("braz",h) )
  }
  
      @Test
  def testEncoden2() {
    assertEquals(List(),encode("",h) )
  }
      
      
   /**
   * Test d'initialisation d'un arbre de Huffman v2
   */
  
  @Test
  def testInitHuffmann0() {
    assertEquals(List(Feuille(0.5,'a'), Feuille(0.3,'b'), Feuille(0.2,'c')) ,initHuffman(('a', 0.5)::('b', 0.3)::('c', 0.2)::Nil ) )
  }
  
  @Test
  def testInitHuffmann1() {
    assertEquals(Nil,initHuffman(Nil))
  }
  
  /**
   * Test de tri d'une liste d'arbre de Huffman v2
   */
  
  @Test
  def testTriSelonFreqn0() {
    assertEquals(triSelonFreq(Feuille(0.5, 'a')::Feuille(0.2, 'b')::Feuille(0.3, 'c')::Nil),Feuille(0.2, 'b')::Feuille(0.3, 'c')::Feuille(0.5, 'a')::Nil)
  }

  @Test
  def testTriSelonFreqn1() {
    assertEquals(Nil,triSelonFreq(Nil))
  }
  
  /**
   * Test de création d'un arbre à partir d'une liste de feuilles / Noeuds v2
   */
  
  @Test
  def testCodeHuffmann0() {
    assertEquals(Noeud(1, Noeud(0.5, Feuille(0.2, 'c'), Feuille(0.3, 'b')), Feuille(0.5, 'a')),codeHuffman(('c', 0.2)::('b', 0.3)::('a', 0.5)::Nil))
  }
  
  @Test
  def testFusionn0() {
    assertEquals(fusion(Feuille(0.2, 'b')::Feuille(0.3, 'c')::Nil), Noeud(0.5,Feuille(0.2, 'b'),Feuille(0.3, 'c')))
  }
  
  /**
   * Test de l'analyse des fréquences de chaque lettre d'une chaîne de caractères v2
   */
  
  @Test
  def testAnalyseFrequencesn0() {
    assertEquals(('a', 0.5)::('b', 0.25)::('r', 0.25)::Nil,analyseFrequences("abra"))
  }
  
  @Test
  def testAnalyseFrequencesn1() {
    assertEquals(Nil,analyseFrequences(""))
  }

  /**
   * Test des sous fonctions de analyseFrequences v2
   */
  
  @Test
  def testComptern0() {
    //assertEquals(2.0,compter('a',"abra".toList))
    if ( compter('a',"abra".toList) != 2.0 ) fail()
  }
  
  @Test
  def testComptern1() {
    //assertEquals(0,compter('a',Nil))
    if(compter('a', Nil) != 0) fail()
  }
  
  
  
  @Test
  def testLettresPresentesn0() {
    assertEquals('a'::'b'::'r'::Nil, lettresPresentes("abra".toList))
  }
  
  @Test
  def testLettresPresentesn1() {
    assertEquals(Nil, lettresPresentes(Nil))
  }
  
  
  
  @Test
  def testRemoven0() {
    assertEquals(remove('a', "abra".toList), "br".toList)
  }
  
  @Test
  def testRemoven1() {
    assertEquals(remove('c',"abra".toList), "abra".toList)
  }
  
  @Test
  def testRemoven2() {
    assertEquals(remove('a',Nil), Nil)
  }
  
}
