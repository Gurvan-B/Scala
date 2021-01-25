package fr.istic.si2.qtrees.main

import fr.istic.si2.scribble._
import fr.istic.si2.qtrees.lib._

object QtreeImage {

  /**
   * @param q un quadtree
   * @param dim la dimension, en pixels, de l'image carrée à produire.
   *        dim doit être une puissance de 2
   * @param grid un booléen indiquant si on souhaite visualiser, dans
   *        l'image produite, la grille repère indiquant la structure
   *        de l'image correspondant à q
   * @return une image de dimension (dim x dim) pixels, correspondant à q,
   *         avec la grille repère selon grid
   *
   * @note Attention, dim doit être une puissance de 2
   */

  def image(q: Quadtree, dim: Int, grid: Boolean): Image = {
    q match {
      case Area(c) => 
        if ( grid )
          FillColor(LineColor(Rectangle(dim,dim),RED),c)
        else 
          FillColor(LineColor(Rectangle(dim,dim),c),c)
      case Mix(no,ne,se,so) => below(beside(image(no,dim/2,grid),image(ne,dim/2,grid)),beside(image(so,dim/2,grid),image(se,dim/2,grid)))
    }
  }
  
}