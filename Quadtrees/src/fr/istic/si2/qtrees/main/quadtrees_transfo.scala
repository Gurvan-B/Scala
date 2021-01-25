package fr.istic.si2.qtrees.main

import fr.istic.si2.scribble._
import fr.istic.si2.qtrees.lib._

object QtreeTransfo {

  /**
   * @param q un quadtree
   * @return le quadtree correspondant à la compression de q
   */
  
  def compress(q: Quadtree): Quadtree = {
    q match{
      case Area(c) => Area(c)
      case Mix(Area(TRANSPARENT), Area(TRANSPARENT), Area(TRANSPARENT), Area(TRANSPARENT)) => Area(WHITE)
      case Mix(no,ne,se,so) => 
        val quart = compress(no)
        if ( quart == compress(ne) && quart == compress(se) && quart == compress(so) )
          quart
        else
          Mix(compress(no),compress(ne),compress(se),compress(so)) 
    }
  }
  

  /**
   * @param q un quadtree
   * @return le quadtree correspondant à une rotation de q de 90º vers la droite
   */

  def rotate90(q: Quadtree): Quadtree = {
    q match {
      case Area(c) => Area(c)
      case Mix(no,ne,se,so) => 
        Mix(rotate90(so),rotate90(no),rotate90(ne),rotate90(se))
    }
  }
  

  /**
   * @param q un quadtree
   * @return le quadtree correspondant à q ayant subi un retournement horizontal
   *
   * @note Attention, ce retournement NE PEUT PAS être programmé avec la rotation.
   */

  def mirror(q: Quadtree): Quadtree = {
    q match {
      case Area(c) => Area(c)
      case Mix(no,ne,se,so) => 
        Mix(mirror(se),mirror(so),mirror(no),mirror(ne))
    }
  }
  

  /**
   * @param q un quadtree
   * @param f une fonction de transformation de couleur
   * @return le quadtree correspondant à la même image que q, mais où chaque couleur a subi
   *         la transformation f
   */

  def mapQuadtree(q: Quadtree, f: Color => Color): Quadtree = {
    q match {
      case Area(c) => Area(f(c))
      case Mix(no,ne,se,so) => Mix(mapQuadtree(no,f),mapQuadtree(ne,f),mapQuadtree(se,f),mapQuadtree(so,f))
    }
  }
  
  
  /**
   * @param c une couleur
   * @return la même couleur avec une opacité plus faible
   */
  
  def baissOpacite (c: Color) : Color = {
    c match {
      case Color(r,g,b,a) => Color(r,g,b,a/2)
    }
  }
  
    /**
   * @param c une couleur
   * @return la même couleur avec une opacité plus faible
   */
  
  def montOpacite (c: Color) : Color = {
    c match {
      case Color(r,g,b,a) => 
        if (a*2 > 255) Color(r,g,b,255)
        else Color(r,g,b,a*2)  
    }
  }
  
  
  /**
   * @param c une couleur
   * @return la couleur inverse
   */
  
  def inverseCouleur (c: Color) : Color = {
    c match {
      case Color(r,g,b,a) => Color(255-r, 255-g, 255-b, a)
    }
  }

}